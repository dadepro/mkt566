# ============================================================================
# MKT 566, Week 5: PCA for perceptual maps (brand survey)
# ============================================================================
# This script reproduces the perceptual-map example from the slides:
# respondents rated four office-equipment brands on six attributes, and PCA
# turns the six numbers per brand into a two-dimensional map of the market.
#
# HOW TO RUN THIS SCRIPT: same as always, from the top, one step at a time
# (Cmd+Enter / Ctrl+Enter). Charts are saved as PDFs in "figures".
# ============================================================================

#### install libraries ####
pkgs <- c("ggplot2", "data.table", "ggrepel")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(ggplot2)    # charts
library(data.table) # fast tables
library(ggrepel)    # chart labels that do not overlap

# ---- housekeeping: safe to run without understanding -----------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
dir.create("figures", showWarnings = FALSE)
# ---------------------------------------------------------------------------

# ============================================================================
# PART 1: The data
# ============================================================================
# Four brands, six columns: each cell is the average rating the brand got on
# that attribute (choice, prices, service, quality, convenience, preference).
brands <- fread("data/perceptual_map_office.csv")
brands

# ============================================================================
# PART 2: PCA on the six attributes
# ============================================================================
pca <- prcomp(brands[, 2:7], center = TRUE, scale. = TRUE)
summary(pca)   # PC1 71%, PC2 27%: two axes keep 98% of the variation

# Loadings: how each attribute maps onto the two axes.
round(pca$rotation[, 1:2], 2)

# ============================================================================
# PART 3: The perceptual map (biplot: brands + attribute arrows)
# ============================================================================
scores <- data.table(pca$x[, 1:2], brand = brands$brand)
loadings <- as.data.table(pca$rotation[, 1:2], keep.rownames = "attribute")
arrow_scale <- 2

ggplot() +
  geom_vline(xintercept = 0, linetype = 3, color = "grey60") +
  geom_hline(yintercept = 0, linetype = 3, color = "grey60") +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1 * arrow_scale,
                   yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.2, "cm")), color = "grey40") +
  geom_text_repel(data = loadings,
                  aes(x = PC1 * arrow_scale, y = PC2 * arrow_scale,
                      label = attribute), size = 4.2, color = "grey30") +
  geom_point(data = scores, aes(x = PC1, y = PC2),
             color = "#990000", size = 4) +
  geom_text_repel(data = scores, aes(x = PC1, y = PC2, label = brand),
                  size = 5, fontface = "bold", color = "#990000") +
  labs(title = "Perceptual Map of the Office-Equipment Market",
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 14)
ggsave("figures/05-perceptual-map.pdf", width = 7.5, height = 5)

# Reading the map: brands close together compete head-to-head; a brand near
# an arrow is strong on that attribute; empty regions are possible gaps in
# the market.
