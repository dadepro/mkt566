# ============================================================================
# MKT 566, Week 6: PCA for perceptual maps (brand survey)
# ============================================================================
# This script reproduces the perceptual-map slides: a separate survey in
# which respondents rated four office-equipment brands on six things, and PCA
# turns the six numbers per brand into a two-axis map of the market.
#
# HOW TO RUN THIS SCRIPT: same as always, from the top, one step at a time
# (Cmd+Enter / Ctrl+Enter). Charts are saved as PDFs in "figures".
# ============================================================================

#### install libraries ####
pkgs <- c("ggplot2", "data.table")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(ggplot2)    # charts
library(data.table) # fast tables

# ---- housekeeping: safe to run without understanding -----------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
dir.create("figures", showWarnings = FALSE)
# ---------------------------------------------------------------------------

# ============================================================================
# PART 1: The data (slide "A second dataset: four brands")
# ============================================================================
# Four brands, six columns: each cell is the AVERAGE rating the brand got
# from the survey respondents. Five are store attributes (large choice, low
# prices, service quality, product quality, convenience); the sixth,
# preference, is how much respondents like the brand overall.
# Rows are brands now, not shoppers.
brands <- fread("data/perceptual_map_office.csv")
brands

# ============================================================================
# PART 2: PCA on the six ratings
# ============================================================================
pca <- prcomp(brands[, 2:7], center = TRUE, scale. = TRUE)
summary(pca)   # "Proportion of Variance": PC1 0.71, PC2 0.27

# Two axes keep 98% of the differences between the brands:
round(100 * sum(summary(pca)$importance[2, 1:2]))

# Loadings: how much each rating counts in PC1 and PC2.
round(pca$rotation[, 1:2], 2)

# ============================================================================
# PART 3: The perceptual map (brands + rating arrows)
# ============================================================================
scores <- data.table(pca$x[, 1:2], brand = brands$brand)
loadings <- as.data.table(pca$rotation[, 1:2], keep.rownames = "attribute")
# plain labels: "large_choice" -> "large choice", "preference_score" -> "preference"
loadings[, attribute := sub(" score$", "", gsub("_", " ", attribute))]
arrow_scale <- 2   # stretches the arrows so they are easy to see

# Each rating's label sits just past the tip of its own arrow.
att <- loadings[, .(x = PC1 * arrow_scale, y = PC2 * arrow_scale, label = attribute)]
att[, ang := atan2(y, x)]
att[, `:=`(lx = x + 0.12 * cos(ang), ly = y + 0.12 * sin(ang),
           hj = (1 - cos(ang)) / 2, vj = (1 - sin(ang)) / 2)]
# Brand names placed by hand, above or below each point, clear of the arrows.
bl <- scores[, .(x = PC1, y = PC2, label = brand)]
bl[, side := fifelse(label %in% c("PaperNCo", "OfficeEquipment"), -1, 1)]
bl[, `:=`(ly = y + 0.24 * side, vj = fifelse(side > 0, 0, 1),
          hj = fifelse(label == "OfficeStar", 0.85,
                       fifelse(label == "Supermarket", 0.2, 0.5)))]

ggplot() +
  geom_vline(xintercept = 0, linetype = 3, color = "grey60") +
  geom_hline(yintercept = 0, linetype = 3, color = "grey60") +
  geom_segment(data = att, aes(x = 0, y = 0, xend = x, yend = y),
               arrow = arrow(length = unit(0.2, "cm")), color = "grey45") +
  geom_text(data = att, aes(x = lx, y = ly, label = label, hjust = hj, vjust = vj),
            size = 4, color = "grey25") +
  geom_point(data = scores, aes(x = PC1, y = PC2), color = "#990000", size = 4) +
  geom_text(data = bl, aes(x = x, y = ly, label = label, hjust = hj, vjust = vj),
            color = "#990000", fontface = "bold", size = 5.2) +
  coord_fixed(clip = "off") +   # equal scales, so the arrows point the right way
  labs(title = "Perceptual Map: Four Office-Equipment Brands", x = "PC1", y = "PC2") +
  theme_minimal(base_size = 14)
ggsave("figures/10-perceptual-map.pdf", width = 8.5, height = 4.6)

# Reading the map: brands close together compete head-to-head; a brand near
# an arrow is strong on that rating; empty regions are possible gaps in the
# market. OfficeStar sits alone on the right, by service quality, product
# quality, large choice, and preference: the brand people like best.
