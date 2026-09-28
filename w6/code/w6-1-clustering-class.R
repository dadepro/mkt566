# ============================================================================
# MKT 566, Week 6: Clustering (k-means) + PCA to see the clusters
# ============================================================================
# This script reproduces every chart and table from the clustering slides,
# in the order they appear:
#   PART 1  the car-market sketch (slide "Why marketers cluster (and map)")
#   PART 2  the 60-customer example and k-means on it
#   PART 3  k-means, step by step (the click-through slide)
#   PART 4  the office-store survey (segmentation steps 1 and 2)
#   PART 5  the elbow curve (step 3)
#   PART 6  k-means, the profile table, the names, the descriptors (steps 4-7)
#   PART 7  PCA: why, how many axes, what they mean, the maps
# The brand perceptual map is in the other script, w6-1-pca-class.R.
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
# the colors used for clusters 1, 2, 3 on every slide
seg_cols <- c("1" = "#990000", "2" = "#0072B2", "3" = "#E69F00")
# ---------------------------------------------------------------------------

# ============================================================================
# PART 1: The car-market sketch
# ============================================================================
# An illustration, NOT data: the positions below are made up to show what a
# perceptual map looks like. PART 7 and w6-1-pca-class.R build real ones.
cars <- data.table(
  brand = c("Toyota", "Honda", "Kia", "Volvo", "Mercedes", "BMW", "Porsche", "Mazda"),
  x = c(-2.2, -1.7, -2.4, 1.3, 2.2, 2.1, 2.8, -0.9),
  y = c(-1.2, -0.5, 0.2, -2.0, -0.4, 1.0, 2.2, 1.3)
)
ggplot(cars, aes(x, y, label = brand)) +
  geom_vline(xintercept = 0, linetype = 3, color = "grey60") +
  geom_hline(yintercept = 0, linetype = 3, color = "grey60") +
  geom_point(color = "#990000", size = 3.5) +
  geom_text_repel(size = 4.6, fontface = "bold") +
  scale_x_continuous(limits = c(-3.4, 3.4)) +
  scale_y_continuous(limits = c(-2.9, 2.9)) +
  labs(title = "The car market, as customers see it",
       x = "<-- affordable        premium -->",
       y = "<-- practical        sporty -->") +
  theme_minimal(base_size = 14) +
  theme(axis.text = element_blank())
ggsave("figures/01-car-sketch.pdf", width = 5.8, height = 4.4)

# ============================================================================
# PART 2: Sixty customers, two numbers
# ============================================================================
# Simulated data: 60 customers in three overlapping groups (occasional
# shoppers, frequent small-basket shoppers, big spenders). For each customer:
# purchases last year and total spend last year. set.seed() makes the random
# numbers the same every time, so you get exactly the slide.
set.seed(42)
toy_spec <- list(c(6, 2.5, 300, 130),    # mean purchases, sd, mean spend, sd
                 c(15, 3, 1050, 250),
                 c(14, 3, 500, 140))
toy <- rbindlist(lapply(seq_along(toy_spec), function(i) data.table(
  purchases = pmax(0, round(rnorm(20, toy_spec[[i]][1], toy_spec[[i]][2]))),
  spend     = pmax(20, rnorm(20, toy_spec[[i]][3], toy_spec[[i]][4])))))

# The chart before k-means: how many groups do you see?
p_toy <- ggplot(toy, aes(x = purchases, y = spend)) +
  geom_point(size = 3, alpha = 0.8, color = "grey35") +
  scale_y_continuous(labels = dollar) +
  labs(title = "Customers: Purchases vs. Total Spend, Last Year",
       x = "Purchases last year", y = "Total spend last year") +
  theme_minimal(base_size = 14)
p_toy
ggsave("figures/02-toy-customers.pdf", width = 8, height = 4.4)

# k-means with k = 3. The two columns have very different units (purchases vs.
# dollars), so we standardize them first with scale(). nstart = 20 runs
# k-means from 20 random starts and keeps the best result.
km_toy <- kmeans(scale(toy[, .(purchases, spend)]), centers = 3, nstart = 20)
toy[, cluster := as.factor(km_toy$cluster)]

# The cluster centers (the X marks), in the original units: these averages
# give the groups their names on the slide.
cents <- toy[, .(customers = .N, purchases = mean(purchases),
                 spend = mean(spend)), by = cluster][order(spend)]
cents[, .(cluster, customers, purchases = round(purchases), spend = round(spend, -1))]
# lowest spend: occasional shoppers (about 6 purchases, $290)
# middle:       frequent small-basket shoppers (about 14 purchases, $560)
# highest:      big spenders (about 16 purchases, $1,180)

p_toy +
  geom_point(aes(color = cluster), size = 3, alpha = 0.85) +
  geom_point(data = cents, aes(x = purchases, y = spend), shape = 4,
             size = 6, stroke = 2, color = "black") +
  scale_color_manual(values = seg_cols, guide = "none") +
  labs(title = "The Same Customers, Colored by K-Means (k = 3)")
ggsave("figures/03-toy-kmeans.pdf", width = 8, height = 4.4)

# ============================================================================
# PART 3: K-means, step by step (optional)
# ============================================================================
# The same algorithm kmeans() uses, run one step at a time from ONE random
# start, so you can watch it: drop 3 centers, color each customer by its
# nearest center, move each center to the middle of its color, repeat until
# nobody switches. Each step is saved as a picture (figures/04-step-*.pdf).
X_toy  <- scale(toy[, .(purchases, spend)])
mu_toy <- attr(X_toy, "scaled:center")
sd_toy <- attr(X_toy, "scaled:scale")
to_orig <- function(C) data.table(purchases = C[, 1] * sd_toy[1] + mu_toy[1],
                                  spend     = C[, 2] * sd_toy[2] + mu_toy[2])

set.seed(2048)
C <- X_toy[sample(nrow(X_toy), 3), , drop = FALSE]   # step 1: 3 random centers
steps <- list(); prev <- rep(0L, nrow(X_toy))
repeat {
  # step 2: each customer takes the color of its nearest center
  dist2 <- sapply(1:3, function(k)
    rowSums((X_toy - matrix(C[k, ], nrow(X_toy), 2, byrow = TRUE))^2))
  a <- max.col(-dist2)
  steps[[length(steps) + 1]] <- list(C = C, a = a,
                                     switched = sum(a != prev & prev > 0))
  if (all(a == prev)) break                          # nobody switched: stop
  prev <- a
  # step 3: move each center to the middle of its color
  C <- t(sapply(1:3, function(k) colMeans(X_toy[a == k, , drop = FALSE])))
}
# how many customers switched color in each round (the slide: 22, then 5, then 0)
sapply(steps, `[[`, "switched")[-1]

# match the colors to the clusters of PART 2 (k-means numbers groups at random)
final_a <- steps[[length(steps)]]$a
lab <- sapply(1:3, function(k) names(which.max(table(toy$cluster[final_a == k]))))

draw_step <- function(title, subtitle = NULL, a = NULL, C = NULL, C_old = NULL,
                      links = FALSE) {
  d <- toy[, .(purchases, spend)]
  d[, col := if (is.null(a)) "grey" else lab[a]]
  p <- ggplot(d, aes(purchases, spend))
  if (links) {                     # thin lines from each customer to its center
    cc <- to_orig(C)
    d[, `:=`(cx = cc$purchases[a], cy = cc$spend[a])]
    p <- p + geom_segment(data = d, aes(xend = cx, yend = cy, color = col),
                          alpha = 0.35, linewidth = 0.4)
  }
  p <- p + geom_point(aes(color = col), size = 3, alpha = 0.85)
  if (!is.null(C_old)) {           # where the centers were, and where they move
    co <- to_orig(C_old); cn <- to_orig(C)
    arr <- data.table(x = co$purchases, y = co$spend,
                      xend = cn$purchases, yend = cn$spend)
    p <- p +
      geom_point(data = co, shape = 4, size = 6, stroke = 2, color = "grey60") +
      geom_segment(data = arr, aes(x = x, y = y, xend = xend, yend = yend),
                   arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.9)
  }
  if (!is.null(C)) {               # the centers: black-outlined colored X
    cc <- to_orig(C); cc[, col := lab]
    p <- p +
      geom_point(data = cc, shape = 4, size = 8, stroke = 4.5, color = "black") +
      geom_point(data = cc, aes(color = col), shape = 4, size = 7, stroke = 2.6)
  }
  p + scale_color_manual(values = c(seg_cols, grey = "grey35"), guide = "none") +
    scale_x_continuous(limits = c(0, max(toy$purchases) + 1)) +
    scale_y_continuous(labels = dollar, limits = c(0, max(toy$spend) * 1.05)) +
    # equal scales: the X that LOOKS nearest is the X that IS nearest
    coord_fixed(ratio = sd_toy[1] / sd_toy[2]) +
    labs(title = title, subtitle = subtitle,
         x = "Purchases last year", y = "Total spend last year") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold"))
}

frames <- list(
  draw_step("Step 1: drop 3 centers", "anywhere, at random", C = steps[[1]]$C),
  draw_step("Step 2: assign", "each customer takes the color of its nearest X",
            a = steps[[1]]$a, C = steps[[1]]$C, links = TRUE))
for (i in 2:length(steps)) {
  frames[[length(frames) + 1]] <- draw_step(
    if (i == 2) "Step 3: move" else "Step 3 again: move",
    "each X moves to the middle of its color",
    a = steps[[i - 1]]$a, C = steps[[i]]$C, C_old = steps[[i - 1]]$C)
  sw <- steps[[i]]$switched
  frames[[length(frames) + 1]] <- if (sw > 0) {
    draw_step("Step 2 again: assign", sprintf("%d customers switch color", sw),
              a = steps[[i]]$a, C = steps[[i]]$C, links = TRUE)
  } else {
    draw_step("Done: nobody switches", "k-means stops; same clusters as PART 2",
              a = steps[[i]]$a, C = steps[[i]]$C)
  }
}
for (i in seq_along(frames)) {
  ggsave(sprintf("figures/04-step-%02d.pdf", i), frames[[i]], width = 6.4, height = 6)
}
frames[[length(frames)]]   # show the last step

# ============================================================================
# PART 4: The office-store survey (segmentation steps 1 and 2)
# ============================================================================
# 40 survey respondents rated how important six store attributes are to them
# when buying office equipment, each on a 1-10 scale. We also know whether
# each respondent is a professional, their income (in $1,000s), and their age.
# The data is an Excel file, so we read it with read_excel() instead of fread().
dd <- as.data.table(read_excel("data/segmentation_office.xlsx",
                               sheet = "SegmentationData"))
head(dd)
nrow(dd)

# Step 1, choose the columns: the six ratings say what customers WANT, so we
# cluster on them. The three descriptors (professional, income, age) stay OUT
# of the clustering; we use them in step 7 to describe who is in each segment.
ratings <- dd[, .(variety_of_choice, electronics, furniture,
                  quality_of_service, low_prices, return_policy)]

# Step 2, check the units: all six ratings share the same 1-10 scale, so we
# skip standardizing and keep the averages in easy 1-10 units. If your
# columns had different units (income in $, age in years), you would
# standardize first: ratings <- scale(ratings).

# ============================================================================
# PART 5: How many clusters? The elbow curve (step 3)
# ============================================================================
# For each k from 2 to 10, run k-means and record the total within-cluster
# sum of squares: how far the points sit from their cluster centers.
set.seed(123)
wss <- sapply(2:10, function(k) {
  kmeans(ratings, centers = k, nstart = 20)$tot.withinss
})
elbow <- data.table(k = 2:10, wss = round(wss))
elbow
# From 2 to 3 clusters the spread drops from 636 to 288; from 3 to 4, only
# to 252. The curve bends at k = 3.

ggplot(elbow, aes(x = k, y = wss)) +
  geom_line(color = "#990000", linewidth = 1.1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "Total Within-Cluster Sum of Squares vs. k",
       x = "Number of clusters k", y = "Total within-cluster SS") +
  theme_minimal(base_size = 14)
ggsave("figures/05-elbow.pdf", width = 8, height = 4.2)

# ============================================================================
# PART 6: K-means with k = 3, the profile table, the names (steps 4 to 7)
# ============================================================================
# Step 4, run k-means. It starts at random, so we fix the seed.
set.seed(123)
km <- kmeans(ratings, centers = 3, nstart = 20)
km$size                           # respondents per cluster: 14, 18, 8

# Attach each respondent's cluster (1, 2, or 3) back to the data.
dd[, cluster := as.factor(km$cluster)]

# Step 5, profile: the average rating of each attribute, by cluster.
profiles <- dd[, c(list(respondents = .N),
                   lapply(.SD, function(x) round(mean(x), 1))),
               by = cluster, .SDcols = names(ratings)][order(cluster)]
profiles

# Step 6, name: read each row for what it rates high and what it rates low.
# Cluster 1: low_prices 8.3, return_policy 6.3                 -> bargain hunters
# Cluster 2: variety 9.1, electronics 6.1, furniture 5.8       -> one-stop shoppers
# Cluster 3: quality_of_service 8.5                            -> service seekers
seg_labs <- c("1: bargain hunters", "2: one-stop shoppers", "3: service seekers")

# Step 7, describe: professional, income, and age were NOT used by k-means,
# so they honestly describe who ended up in each segment, and how to reach them.
dd[, .(respondents  = .N,
       professional = paste0(round(100 * mean(professional)), "%"),
       income       = dollar(1000 * round(mean(income))),
       age          = round(mean(age))), by = cluster][order(cluster)]

# ============================================================================
# PART 7: Seeing the clusters: PCA
# ============================================================================
# Why PCA: six ratings cannot be plotted on two axes, and many ratings say
# the same thing. Example: shoppers who care about furniture also care about
# electronics (the slide's correlation of 0.69).
round(cor(dd$furniture, dd$electronics), 2)

# PCA replaces the six ratings with new axes (PC1, PC2, ...), in order of how
# much of the differences between shoppers each one keeps. Standardizing
# (scale. = TRUE) is standard practice.
pca <- prcomp(ratings, center = TRUE, scale. = TRUE)
summary(pca)   # "Proportion of Variance": PC1 0.52, PC2 0.26

# ---- How many axes to keep? -------------------------------------------------
shares <- data.table(pc = paste0("PC", 1:6),
                     share = round(100 * summary(pca)$importance[2, ]))
shares   # PC1 + PC2 = 78%: the map shows most of the story
ggplot(shares, aes(x = pc, y = share, fill = pc %in% c("PC1", "PC2"))) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(share, "%")), vjust = -0.4, size = 5) +
  scale_fill_manual(values = c(`TRUE` = "#990000", `FALSE` = "grey70"), guide = "none") +
  scale_y_continuous(limits = c(0, 60), breaks = NULL) +
  labs(title = "Share of the differences each axis keeps", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())
ggsave("figures/06-axes-to-keep.pdf", width = 5.4, height = 4.4)

# ---- What do the two axes mean? The six ratings as arrows ------------------
# The loadings: how much each rating counts in PC1 and PC2.
round(pca$rotation[, 1:2], 2)

loadings <- as.data.table(pca$rotation[, 1:2], keep.rownames = "attribute")
plain_names <- c(variety_of_choice = "variety", electronics = "electronics",
                 furniture = "furniture", quality_of_service = "quality of service",
                 low_prices = "low prices", return_policy = "return policy")
loadings[, attribute := plain_names[attribute]]

arrow_scale <- 3   # stretches the arrows so they are easy to see
ggplot(loadings, aes(x = PC1 * arrow_scale, y = PC2 * arrow_scale, label = attribute)) +
  geom_vline(xintercept = 0, linetype = 3, color = "grey60") +
  geom_hline(yintercept = 0, linetype = 3, color = "grey60") +
  geom_segment(aes(x = 0, y = 0, xend = PC1 * arrow_scale, yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.2, "cm")), color = "#990000") +
  geom_text_repel(size = 4.5, seed = 1) +
  coord_fixed() +            # equal scales, so the arrows point the right way
  labs(title = "The six ratings as arrows", x = "PC1", y = "PC2") +
  theme_minimal(base_size = 13)
ggsave("figures/07-arrows.pdf", width = 6.2, height = 4.8)
# Left: variety, electronics, furniture ("wants a big assortment").
# Up: low prices; down: quality of service ("price, not service" vs. the reverse).

# ---- The map: every shopper on PC1 and PC2, colored by cluster --------------
scores <- data.table(pca$x[, 1:2], cluster = dd$cluster)
p_map <- ggplot(scores, aes(x = PC1, y = PC2, color = cluster)) +
  geom_vline(xintercept = 0, linetype = 3, color = "grey60") +
  geom_hline(yintercept = 0, linetype = 3, color = "grey60") +
  geom_point(size = 3, alpha = 0.85) +
  scale_color_manual(values = seg_cols, labels = seg_labs, name = NULL) +
  coord_fixed(xlim = range(c(scores$PC1, loadings$PC1 * arrow_scale)) + c(-0.4, 0.4),
              ylim = range(c(scores$PC2, loadings$PC2 * arrow_scale)) + c(-0.4, 0.4)) +
  labs(title = "Shoppers on the PCA Map", x = "PC1", y = "PC2") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 2))
p_map
ggsave("figures/08-cluster-map.pdf", width = 6.2, height = 5.2)

# ---- The same map with the arrows (a biplot) --------------------------------
p_map +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1 * arrow_scale, yend = PC2 * arrow_scale),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.2, "cm")), color = "grey40") +
  geom_text_repel(data = loadings,
                  aes(x = PC1 * arrow_scale, y = PC2 * arrow_scale, label = attribute),
                  inherit.aes = FALSE, size = 3.8, color = "grey30", seed = 1)
ggsave("figures/09-biplot.pdf", width = 6.2, height = 5.2)
# Each cluster sits at the tip of the arrows its profile predicted: bargain
# hunters by low prices, one-stop shoppers by variety and furniture, service
# seekers by quality of service. PCA never saw the clusters, so this is an
# independent check that the groups are real.
