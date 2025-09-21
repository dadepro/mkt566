###############################################################################
# PCA Exercise: Brand Survey (4 brands X 6 attributes)
# Learning goals:
#  - Run PCA (with centering & scaling) on brand survey
#  - Read a scree plot & variance explained
#  - Interpret loadings (how items map to PCs)
#  - Visualize respondents in PC space (scores)
#  - Show a biplot (scores + loadings together)
###############################################################################

# --------------------------#
# 0) Libraries
# --------------------------#
# data.table -> fast data handling
# ggplot2    -> plotting
# readr      -> robust CSV reading (handles encoding, headers)
# ggrepel    -> non-overlapping text labels in ggplot2
library(data.table)
library(ggplot2)
library(readr)
library(ggrepel)

# ------------------------------------------------------------#
# 1) Load data
# ------------------------------------------------------------#
# Tip for students: keep raw files in a /data folder next to the script.
# If you're in RStudio, you can set the working directory to the script's folder:
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
# Read CSV (change the path if needed)
df_raw <- fread("data/perceptual_map_office.csv")
head(df_raw)


# ------------------------------------------------------------#
# 2) Select the 6 question columns (PCA needs numeric features)
# large_choice low_prices service_quality product_quality convenience preference_score
# ------------------------------------------------------------#
pca_df <- df_raw[, 2:7]

# ------------------------------------------------------------#
# 3) Run PCA (center & scale are best practice for survey items)
# ------------------------------------------------------------#
# Center = subtract the mean from each item
# Scale  = divide by the standard deviation (z-scores)
pca_fit <- prcomp(pca_df, center = TRUE, scale. = TRUE)

# Quick summary: variance explained by PC1, PC2, ..
summary(pca_fit)


# ------------------------------------------------------------#
# 4) Loadings (correlations PCA and variables: how items map onto PCs (interpretation!)
# ------------------------------------------------------------#
# Loadings = correlations between original items and PCs (for standardized PCA)
loadings <- as.data.table(pca_fit$rotation[, 1:2])
loadings[, brand_attributes := rownames(pca_fit$rotation)]
loadings

# ------------------------------------------------------------#
# 5) Scale loadings for better visibility in plots
# ------------------------------------------------------------#

arrow_scale <- 2.0   # tweak if arrows are too short/long
loadings[, `:=`(xend = PC1 * arrow_scale, yend = PC2 * arrow_scale)]

# Plot loadings alone (labels at the loading coordinates)
p_loadings <- ggplot(loadings, aes(x = xend, y = yend, label = brand_attributes)) +
  geom_segment(aes(x = 0, y = 0, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text_repel(size = 5) +
  labs(
    title = "PCA Loadings: Brand Attributes",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal()
p_loadings


# ------------------------------------------------------------#
# 6) Scores: brands in the PC space
# ------------------------------------------------------------#
scores <- as.data.table(pca_fit$x[, 1:2])  # first 2 PCs
scores[, brand := df_raw$brand]

# Plot brands in the PC1 vs PC2 space 
p_brands <- ggplot(scores, aes(x = PC1, y = PC2, label = brand)) +
  geom_point(size = 4, color = "darkred") +
  geom_text_repel(size = 5) +
  labs(
    title = "Perceptual Map of Brands (PCA)",
    x = "\nPrincipal Component 1",
    y = "Principal Component 2\n"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal()
p_brands

# ------------------------------------------------------------#
# 7) Biplot: scores + loadings together
# ------------------------------------------------------------#

p_biplot <- ggplot() +
  geom_point(data = scores, aes(x = PC1, y = PC2), color = "darkred", size = 3) +
  geom_text_repel(data = scores, aes(x = PC1, y = PC2, label = brand), size = 5) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text_repel(data = loadings,
                  aes(x = xend, y = yend, label = brand_attributes),
                  size = 4, color = "blue") +
  labs(
    title = "Perceptual Map with Attributes (Biplot)",
    x = "\nPrincipal Component 1",
    y = "Principal Component 2\n"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal()
p_biplot

