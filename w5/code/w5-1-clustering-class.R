# ============================================================================
# MKT 566, Week 5: Clustering (k-means) + PCA to see the clusters
# ============================================================================
# This script reproduces every chart and table from the clustering slides:
# the elbow curve, k-means on the office-store survey, the cluster profile
# table, the descriptor table, and the PCA map of the clusters.
#
# HOW TO RUN THIS SCRIPT (same workflow as before):
#   - Run it from the top, one step at a time: click on a line and press
#     Cmd+Enter (Mac) or Ctrl+Enter (Windows).
#   - Tables print in the terminal (the R console). Charts open in a VS Code
#     tab and are also saved as PDFs in the "figures" folder.
#   - Lines starting with # are comments: notes for humans, ignored by R.
#
# You are NOT expected to memorize this code. Read the comment above each
# block, run it, and look at the result. If you want to understand a
# specific line, select it and ask your AI assistant to explain it.
# ============================================================================

#### install libraries ####
pkgs <- c("ggplot2", "data.table", "readxl", "ggrepel", "scales")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(ggplot2)    # charts
library(data.table) # fast tables
library(readxl)     # read Excel files (.xlsx): new this week
library(ggrepel)    # chart labels that do not overlap: new this week
library(scales)     # nicer axis labels

# ---- housekeeping: safe to run without understanding -----------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
dir.create("figures", showWarnings = FALSE)
set.seed(123)
# ---------------------------------------------------------------------------

# ============================================================================
# PART 1: The data
# ============================================================================
# 40 survey respondents rated how important six store attributes are to them
# when buying office equipment, each on a 1-10 scale. We also know whether
# each respondent is a professional, their income (in $1,000s), and their age.
# The data is an Excel file, so we read it with read_excel() instead of fread().
dd <- as.data.table(read_excel("data/segmentation_office.xlsx",
                               sheet = "SegmentationData"))
head(dd)
nrow(dd)

# The six ratings are what we cluster on. The three descriptors (professional,
# income, age) stay OUT of the clustering; we use them later to describe who
# is in each segment.
ratings <- dd[, .(variety_of_choice, electronics, furniture,
                  quality_of_service, low_prices, return_policy)]

# All six ratings share the same 1-10 scale, so we can skip standardizing.
# If your variables had different units (income in $, age in years), you
# would standardize first: ratings <- scale(ratings).

# ============================================================================
# PART 2: How many clusters? The elbow curve
# ============================================================================
# For each k from 2 to 10, run k-means and record the total within-cluster
# sum of squares: how far the points sit from their cluster centers.
# nstart = 20 runs each k-means from 20 random starts and keeps the best.
wss <- sapply(2:10, function(k) {
  kmeans(ratings, centers = k, nstart = 20)$tot.withinss
})
elbow <- data.table(k = 2:10, wss = wss)
elbow

ggplot(elbow, aes(x = k, y = wss)) +
  geom_line(color = "#990000", linewidth = 1.1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "The Elbow Curve",
       x = "Number of clusters k",
       y = "Total within-cluster sum of squares") +
  theme_minimal(base_size = 14)
ggsave("figures/01-elbow.pdf", width = 7, height = 4.2)
# Reading: a huge drop from k = 2 to k = 3, small savings after. Elbow at 3.

# ============================================================================
# PART 3: Run k-means with k = 3
# ============================================================================
set.seed(123)                     # k-means starts at random; fix the seed
km <- kmeans(ratings, centers = 3, nstart = 20)
km$size                           # how many respondents in each cluster

# Attach each respondent's cluster (1, 2, or 3) back to the data.
dd[, cluster := as.factor(km$cluster)]

# ---- The profile table: what does each cluster care about? -----------------
# The average rating of each attribute, by cluster. This table IS the
# interpretation: read it row by row and name the segments.
profiles <- dd[, lapply(.SD, function(x) round(mean(x), 1)),
               by = cluster, .SDcols = names(ratings)][order(cluster)]
profiles
# Cluster 1: low_prices 8.3, return_policy 6.3  -> bargain hunters
# Cluster 2: variety 9.1, electronics 6.1, furniture 5.8 -> one-stop shoppers
# Cluster 3: quality_of_service 8.5              -> service seekers

# ---- The descriptor table: who is in each segment? -------------------------
# professional, income, and age were NOT used by k-means, which makes them
# an honest description of who ended up in each cluster: how to reach them.
dd[, .(share_professional = round(mean(professional), 2),
       income = round(mean(income), 1),
       age = round(mean(age), 1),
       n = .N), by = cluster][order(cluster)]

# ============================================================================
# PART 4: Seeing the clusters: PCA
# ============================================================================
# Six ratings = six dimensions; a screen has two. PCA finds the two
# directions along which respondents differ the most and gives every
# respondent a coordinate on each: a map that keeps as much of the
# variation as possible. Center + scale is standard practice.
pca <- prcomp(ratings, center = TRUE, scale. = TRUE)
summary(pca)   # PC1 52%, PC2 26%: the 2D map keeps 78% of the variation

# ---- Loadings: what do the two axes mean? ----------------------------------
# The loadings say how much each original rating contributes to each PC.
round(pca$rotation[, 1:2], 2)

loadings <- as.data.table(pca$rotation[, 1:2], keep.rownames = "attribute")
arrow_scale <- 3
ggplot(loadings, aes(x = PC1 * arrow_scale, y = PC2 * arrow_scale,
                     label = attribute)) +
  geom_vline(xintercept = 0, linetype = 3, color = "grey60") +
  geom_hline(yintercept = 0, linetype = 3, color = "grey60") +
  geom_segment(aes(x = 0, y = 0, xend = PC1 * arrow_scale,
                   yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.2, "cm")), color = "#990000") +
  geom_text_repel(size = 4.5) +
  labs(title = "What the Two Axes Mean (PCA loadings)",
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 14)
ggsave("figures/02-loadings.pdf", width = 7, height = 4.6)

# ---- The map: every respondent on two axes, colored by cluster -------------
scores <- data.table(pca$x[, 1:2], cluster = dd$cluster)
ggplot(scores, aes(x = PC1, y = PC2, color = cluster)) +
  geom_vline(xintercept = 0, linetype = 3, color = "grey60") +
  geom_hline(yintercept = 0, linetype = 3, color = "grey60") +
  geom_point(size = 3, alpha = 0.85) +
  scale_color_manual(values = c("#990000", "#0072B2", "#E69F00")) +
  labs(title = "The 40 Respondents on the PCA Map, Colored by Cluster",
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 14)
ggsave("figures/03-cluster-map.pdf", width = 7, height = 4.6)

# ---- Biplot: map + arrows together ------------------------------------------
ggplot() +
  geom_vline(xintercept = 0, linetype = 3, color = "grey60") +
  geom_hline(yintercept = 0, linetype = 3, color = "grey60") +
  geom_point(data = scores, aes(x = PC1, y = PC2, color = cluster),
             size = 3, alpha = 0.85) +
  scale_color_manual(values = c("#990000", "#0072B2", "#E69F00")) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1 * arrow_scale,
                   yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.2, "cm")), color = "grey30") +
  geom_text_repel(data = loadings,
                  aes(x = PC1 * arrow_scale, y = PC2 * arrow_scale,
                      label = attribute), size = 4, color = "grey30") +
  labs(title = "Biplot: Clusters and What the Axes Mean",
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 14)
ggsave("figures/04-biplot.pdf", width = 7.5, height = 4.8)

# Each cluster sits where its profile says it should: bargain hunters by the
# low_prices arrow, one-stop shoppers by variety/electronics/furniture,
# service seekers by quality_of_service.
