###############################################################################
# K-means clustering + PCA visualization, step-by-step #                                 #
# Instructor notes: This script demonstrates (1) building an "elbow curve" to #
# choose k, (2) running k-means, (3) inspecting cluster profiles, and (4)     #
# visualizing clusters via PCA (scores + loadings + biplot).                  #
###############################################################################

# --------------------------#
# 0) Load required libraries
# --------------------------#
# ggplot2   -> visualization
# data.table-> fast data frames + group-by syntax
# readxl    -> read Excel files (.xlsx)
library(ggplot2)
library(data.table)
library(readxl)

# ------------------------------------------------------------#
# 1) Set working directory to the folder that holds this file  #
# ------------------------------------------------------------#
# This uses an RStudio helper to setwd to the directory of the
# currently open script. Nice for reproducibility in class.
# If not in RStudio, you can comment this out and set a path manually.
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
# Alternative (manual): setwd("/path/to/your/folder")

# ----------------------------------------------#
# 2) Read data for clustering from the Excel file
# ----------------------------------------------#
# We have 10 columns or variables in our data:
#   
#   respondent_id is an identifier for our observations
# 
# Respondents rated the importance of each of the following attributes on a 1-10 scale: 
# variety_of_choice, electronics, furniture, quality_of_service, low_prices, return_policy.
# 
# professional: 1 for professionals, 0 for non-professionals
# 
# income: expressed in thousands of dollars
# 
# age
# 
# The cluster analysis will try to identify clusters with similar patterns of ratings.


dd <- as.data.table(read_excel("data/segmentation_office.xlsx", sheet = "SegmentationData"))

# Quick peek (sanity check): shows the first rows and column names
head(dd)
str(dd)  # See variable types (helpful before clustering)

# --------------------------------------------------------------#
# 3) Make sure categorical variables are factors (not characters)
# --------------------------------------------------------------#
# K-means requires numeric features only; we’ll keep factors in the full data
# (for labeling/averages) but EXCLUDE them from the clustering matrix below.
# In your data, 'professional' is categorical -> convert to factor for clarity.
if ("professional" %in% names(dd)) {
  dd[, professional := as.factor(professional)]
}

# ----------------------------------------------------------------------#
# 4) Choose the set of variables to cluster on (NUMERIC columns only!)
# ----------------------------------------------------------------------#
# IMPORTANT POINT:
# - K-means uses Euclidean distance; variables must be numeric.
# - If variables are on very different scales (e.g., revenue vs. counts),
#   you typically standardize (z-scores). Here we follow your original code.
#
# Let's keep the consumer responses: variety_of_choice, electronics, furniture, quality_of_service, low_prices, return_policy
cluster.dd <- dd[, 2:(ncol(dd) - 3)]


# Optional: standardize variables for k-means (common best practice)
# cluster.dd <- scale(cluster.dd)

# -------------------------------------------------------#
# 5) "Elbow method" to help pick the number of clusters k
# -------------------------------------------------------#
set.seed(123)  # Ensures we get the same random starts each time (reproducible)

# We'll test k = 2, 3, ..., (n+1) and store total within-cluster sum of squares.
# 'tot.withinss' shrinks as k increases; we look for a "knee/elbow".
n <- 9
kmeans_fit_vec <- rep(NA_real_, n + 1)  # index aligns with k (so we can use k directly)

for (k in 2:(n + 1)) {
  # nstart = 20 -> run k-means with 20 random initializations; pick the best
  km <- kmeans(cluster.dd, centers = k, nstart = 20)
  # store total within-cluster sum of squares for this k
  kmeans_fit_vec[k] <- km$tot.withinss
}

# Tidy data frame for plotting elbow curve
elbow_df <- data.frame(
  k = 2:(n + 1),
  tot.withinss = kmeans_fit_vec[2:(n + 1)]
)

# Plot the elbow curve:
# - X: number of clusters
# - Y: total within-cluster sum of squares
# - Look for the "elbow" where extra clusters give diminishing returns
p <- ggplot(elbow_df, aes(x = k, y = tot.withinss)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Total Within-Cluster Sum of Squares vs. Number of Clusters\n",
    x = "\nNumber of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares\n"
  ) +
  scale_x_continuous(breaks = seq(2, n + 1, 1)) +  # use n consistently
  theme_minimal()
p  # print plot

# ------------------------------------------#
# 6) Fix k and run k-means (here we pick k=3)
# ------------------------------------------#
# In class, you can ask students to eyeball the elbow plot and pick k.
# We'll follow your choice of k = 3.
k_fixed <- 3
kmeans_result <- kmeans(cluster.dd, centers = k_fixed, nstart = 20)

# Attach cluster assignments (1..k) back to the original data for profiling
dd[, cluster := as.factor(kmeans_result$cluster)]

# --------------------------------------------------------------#
# 7) Explore cluster profiles via group means (simple diagnostic)
# --------------------------------------------------------------#
# This reports the average of each numeric feature within each cluster.
# It helps us interpret what each cluster "values" (e.g., price-sensitive, variety-seeking).
cluster_averages <- dd[, lapply(.SD, mean, na.rm = TRUE),
                       by = cluster, .SDcols = names(cluster.dd)]

# Sort by cluster label for tidy printing
cluster_averages[order(cluster)]

# What do we see
# Cluster 1: low prices
# Cluster 2: service
# Cluster 3: variety
# => In practice, derive these labels from the pattern in 'cluster_averages'.

# -------------------------------------------#
# 8) PCA to visualize clusters in 2D (scores)
# -------------------------------------------#
# PCA reduces the high-dimensional numeric space to orthogonal components.
# We center+scale so each feature contributes equally (standard PCA practice).
pca_result <- prcomp(cluster.dd, center = TRUE, scale. = TRUE)

# Quick summary: variance explained by PC1, PC2, ...
summary(pca_result)

# Keep first two principal components (scores) and add cluster labels
pca_data <- data.frame(pca_result$x[, 1:2], cluster = dd$cluster)
# Column names are "PC1", "PC2" by default in prcomp$x

# -------------------------------------------------------------------#
# 9) PCA loadings (a.k.a. rotation): links original vars to the PCs
# -------------------------------------------------------------------#
# Loadings tell you how each original variable contributes to each PC.
pca_loadings <- as.data.frame(pca_result$rotation[, 1:2]) # 2 columns: PC1, PC2
pca_loadings$variable <- rownames(pca_loadings)           # keep var names for plotting
pca_loadings

# Scale loadings for better visibility in plots
arrow_scale <- 10   # tweak if arrows are too short/long
loadings[, `:=`(xend = PC1 * arrow_scale, yend = PC2 * arrow_scale)]

# Plot loadings alone (labels at the loading coordinates)
p_loadings <- ggplot(pca_loadings, aes(x = PC1 * arrow_scale, y = PC2 * arrow_scale, label = variable)) +
  geom_segment(aes(x = 0, y = 0, xend = PC1 * arrow_scale, yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.3, "cm")), color = "blue") +
  geom_text(size = 5) +
  labs(
    title = "PCA Loadings: Store attributes",
    x = "\nPrincipal Component 1",
    y = "Principal Component 2\n"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal() +
  coord_cartesian(xlim = c(-7, 5), ylim = c(-7, 7))  # adjust as needed
p_loadings

# ------------------------------------------------------------#
# 10) Scatter of PCA scores colored by cluster (clusters in 2D)
# ------------------------------------------------------------#
p_pca <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(
    title = "PCA of Clusters",
    x = "\nPrincipal Component 1",
    y = "Principal Component 2\n"
  ) +
  theme_minimal()
p_pca

# -----------------------------------------------------------#
# 11) Biplot: scores + loadings together (interpretation aid)
# -----------------------------------------------------------#
# - Points = customers/records in the PC space, colored by cluster.
# - Arrows = variables; direction shows correlation with PCs;
#            length shows strength of association.
# Scaling note: multiplying loadings by a small factor moves text away from origin
# to avoid overlap with arrow tips; adjust 1.3–1.6 as needed for readability.
biplot <- ggplot() +
  geom_point(data = pca_data, aes(x = PC1, y = PC2, color = cluster), size = 3) +
  geom_segment(data = pca_loadings,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "blue") +
  geom_text(data = pca_loadings,
            aes(x = PC1 * 1.4, y = PC2 * 1.4, label = variable),
            size = 4) +
  labs(
    title = "Biplot of PCA",
    x = "\nPrincipal Component 1",
    y = "Principal Component 2\n"
  ) +
  theme_minimal()
biplot

###############################################################################
# Footnotes:
# - K-means assumptions: spherical, similarly sized clusters; sensitive to scale.
# - Consider standardizing features (z-scores) before k-means if units differ.
# - 'nstart' helps avoid bad local minima by trying multiple random starts.
# - PCA is unsupervised; use it here only as a visualization aid, not as the
#   basis for clustering unless you intentionally cluster on PCs.
# - When labeling clusters ("price-sensitive", "service"), justify using
#   the cluster_averages table and business context—avoid over-interpretation.
###############################################################################
