# ============================================================================
# MKT 566, Week 3: Exploratory Data Analysis -- Covariation
# ============================================================================
# This script builds every chart from the covariation slides: two continuous
# variables (scatter plots, correlation, overplotting), one categorical and
# one continuous variable (bars of averages with error bars, boxplots by
# group), and two categorical variables (heatmaps).
#
# HOW TO RUN THIS SCRIPT (same workflow as weeks 1 and 2):
#   - Run it from the top, one step at a time: click on a line and press
#     Cmd+Enter (Mac) or Ctrl+Enter (Windows).
#   - A chart is built by one long instruction that spans several lines
#     (the lines glued together by "+"). Cmd+Enter runs the whole
#     instruction at once.
#   - Each chart opens in a VS Code tab. The ggsave() line right after each
#     chart also saves it as a PDF in the "figures" folder.
#   - Lines starting with # are comments: notes for humans, ignored by R.
#
# You are NOT expected to memorize this code. Read the comment above each
# chart, run the code, and look at the result. If you want to understand a
# specific line, select it and ask your AI assistant to explain it.
# ============================================================================

#### install libraries ####
# Installs the packages this script needs, skipping any you already have.
pkgs <- c("datarium", "ggplot2", "dplyr", "tidyr", "data.table", "patchwork",
          "scales")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(datarium)   # the "marketing" dataset (ad spend on three channels)
library(ggplot2)    # charts (and the "diamonds" dataset for overplotting)
library(dplyr)      # data-manipulation verbs: group_by(), summarise()
library(tidyr)      # pivot_longer(): reshape wide data into long format
library(data.table) # fast tables; fread() reads CSVs quickly
library(patchwork)  # place charts side by side with a simple +
library(scales)     # nicer axis labels (dollar, comma, percent)

# ---- housekeeping: safe to run without understanding -----------------------
# Point R at the folder that contains this script, so "data" is found and
# the "figures" output folder lands next to it.
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
for (p in c("code", "w3/code", "mkt566/w3/code")) {
  if (!dir.exists("data") && dir.exists(file.path(p, "data"))) setwd(p)
}
if (!dir.exists("data")) {
  stop('R cannot find the "data" folder. In VS Code, use File > Open Folder ',
       'and open the unzipped "code" folder (the one that contains this script), ',
       'then run the script again from the top.')
}
dir.create("figures", showWarnings = FALSE)

# ---------------------------------------------------------------
# Reading ggplot code: the five pieces you will see every time
# ---------------------------------------------------------------
# Every chart your AI assistant writes has the same skeleton:
#   ggplot(DATA, aes(x = ..., y = ...)) +   # 1. the data, 2. which columns go where
#     geom_XXX() +                          # 3. the chart type (point, bar, boxplot...)
#     facet_wrap(~ GROUP) +                 # 4. one panel per group (optional)
#     labs(title = ..., x = ..., y = ...)   # 5. labels
# You do not need to write this from memory. You need to recognize it, so
# that when the AI adds a geom you did not ask for, you notice.

# ===============================================================
# PART 1: two continuous variables
# ===============================================================
# The datarium "marketing" dataset: 200 campaigns, ad spend on youtube,
# facebook, newspaper (thousands of $) and the resulting sales.
head(marketing)
marketing <- as.data.table(marketing)

# Feature engineering: total ad spend across the three channels.
marketing[, total_ad_spend := youtube + facebook + newspaper]

# Scatter plot: the default chart for two continuous variables. Each point
# is one campaign. geom_smooth(method = "lm") adds a fitted straight line.
p_scatter <- ggplot(marketing, aes(x = total_ad_spend, y = sales)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "grey40") +
  labs(title = "Sales vs. Total Ad Spend",
       x = "Total ad spend (thousands $)", y = "Sales (thousands of units)") +
  theme_minimal()
p_scatter
ggsave("figures/w3-1-sales-by-total-adspend-scatter.pdf", width = 6, height = 4)

# A more informative view: one panel per channel. First reshape the data
# from wide (one column per channel) to long (one row per campaign-channel),
# then facet_wrap() draws one scatter per channel.
marketing_long <- data.table(
  pivot_longer(marketing, cols = c(youtube, facebook, newspaper),
               names_to = "channel", values_to = "ad_spend")
)
p_facet <- ggplot(marketing_long, aes(x = ad_spend, y = sales, color = channel)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40") +
  scale_color_manual(values = c(youtube = "#FF0000", facebook = "#1877F2",
                                newspaper = "grey20")) +
  facet_wrap(~channel, scales = "free_x") +
  labs(title = "Sales vs. Ad Spend by Channel",
       x = "Ad spend (thousands $)", y = "Sales (thousands of units)") +
  theme_minimal() +
  theme(legend.position = "none")
p_facet
ggsave("figures/w3-1-sales-by-adspend-channel-scatter.pdf", width = 8, height = 4)

# Correlation: one number that summarizes the strength of a LINEAR
# relationship, from -1 (perfect negative) to +1 (perfect positive).
cor(marketing$total_ad_spend, marketing$sales)
cor(marketing$youtube, marketing$sales)
cor(marketing$facebook, marketing$sales)
cor(marketing$newspaper, marketing$sales)

# ---------------------------------------------------------------
# Overplotting: when you have too many points
# ---------------------------------------------------------------
# ggplot2 ships the "diamonds" dataset: 53,940 diamonds with carat and price.
# A plain scatter is a solid blob: you cannot see where the mass of points is.
p_blob <- ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  labs(title = "Price vs. Carat (53,940 diamonds)", x = "Carat", y = "Price ($)") +
  theme_minimal()
p_blob
ggsave("figures/w3-1-diamonds-overplotting.png", width = 6, height = 4, dpi = 150)

# Two fixes. Left: make points transparent (alpha) and small, and add a
# smooth curve. Right: count points in 2D bins (a heatmap of the density).
p_alpha <- ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.05, size = 0.5) +
  geom_smooth(color = "firebrick", se = FALSE) +
  labs(title = "alpha = 0.05 + smooth curve", x = "Carat", y = "Price ($)") +
  theme_minimal()
p_bins <- ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d(bins = 60) +
  scale_fill_viridis_c(labels = comma) +
  labs(title = "geom_bin2d(): count per cell", x = "Carat", y = "Price ($)",
       fill = "Diamonds") +
  theme_minimal()
p_alpha + p_bins
ggsave("figures/w3-1-diamonds-overplotting-fixed.png", width = 11, height = 4, dpi = 150)

# ===============================================================
# PART 2: one categorical and one continuous variable
# ===============================================================
# Back to the variation-case dataset: 1,000 customers of an online retailer.
df <- fread("data/marketing_eda.csv")
head(df)

# Step 1: summarize. For each gender, the average number of purchases and
# its standard error (SE = sd / sqrt(n)): how precisely we know the mean.
avg_gender <- df[, .(avg_purchases = mean(Purchases),
                     se_purchases  = sd(Purchases) / sqrt(.N),
                     n = .N), by = Gender]
avg_gender

# Step 2: a bar chart of the averages.
p_bar <- ggplot(avg_gender, aes(x = Gender, y = avg_purchases, fill = Gender)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c(F = "#E07A5F", M = "#3D5A80")) +
  labs(title = "Average Purchases by Gender", x = NULL, y = "Average purchases") +
  theme_minimal() +
  theme(legend.position = "none")
p_bar
ggsave("figures/w3-1-purchases-by-gender-bar.pdf", width = 5, height = 3.5)

# Step 3: add error bars. mean +/- 1.96 x SE is a 95% confidence interval:
# the range of values the true average could plausibly take.
p_bar_se <- p_bar +
  geom_errorbar(aes(ymin = avg_purchases - 1.96 * se_purchases,
                    ymax = avg_purchases + 1.96 * se_purchases),
                width = 0.15, linewidth = 0.7)
p_bar_se
ggsave("figures/w3-1-purchases-by-gender-bar-se.pdf", width = 5, height = 3.5)

# Step 4: add a third variable, the ad channel, with one panel per channel.
avg_gender_channel <- df[, .(avg_purchases = mean(Purchases),
                             se_purchases  = sd(Purchases) / sqrt(.N),
                             n = .N), by = .(Gender, Channel)]
p_bar_channel <- ggplot(avg_gender_channel,
                        aes(x = Gender, y = avg_purchases, fill = Gender)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = avg_purchases - 1.96 * se_purchases,
                    ymax = avg_purchases + 1.96 * se_purchases),
                width = 0.15, linewidth = 0.7) +
  scale_fill_manual(values = c(F = "#E07A5F", M = "#3D5A80")) +
  facet_wrap(~Channel, nrow = 1) +
  labs(title = "Average Purchases by Gender and Ad Channel",
       x = NULL, y = "Average purchases") +
  theme_minimal() +
  theme(legend.position = "none")
p_bar_channel
ggsave("figures/w3-1-purchases-by-gender-channel-bar.pdf", width = 9, height = 3.8)

# Averages hide distributions. A boxplot per group shows the whole
# distribution: median, spread, and outliers, for each channel at once.
p_box_channel <- ggplot(df, aes(x = reorder(Channel, Ad_Spend, median),
                                y = Ad_Spend, fill = Channel)) +
  geom_boxplot(alpha = 0.8) +
  scale_y_continuous(trans = "log10", labels = dollar) +
  labs(title = "Ad Spend by Channel (log scale)", x = NULL, y = "Ad spend ($)") +
  theme_minimal() +
  theme(legend.position = "none")
p_box_channel
ggsave("figures/w3-1-adspend-by-channel-boxplot.pdf", width = 6, height = 4)

# ===============================================================
# PART 3: two categorical variables
# ===============================================================
# Count how many customers fall in each channel x device cell...
counts_cd <- df[, .(n = .N), by = .(Channel, Device)]
counts_cd

# ...and draw the table as a heatmap: color = count, number printed on top.
p_heat_n <- ggplot(counts_cd, aes(x = Channel, y = Device, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "white", size = 5) +
  scale_fill_gradient(low = "#9ecae1", high = "#08306b") +
  labs(title = "Customers by Channel and Device", x = NULL, y = NULL,
       fill = "Customers") +
  theme_minimal()
p_heat_n
ggsave("figures/w3-1-channel-device-heatmap-counts.pdf", width = 7, height = 3.5)

# Add a third variable: the average ad spend in each cell.
avg_cd <- df[, .(avg_adspend = mean(Ad_Spend), n = .N), by = .(Channel, Device)]
p_heat_spend <- ggplot(avg_cd, aes(x = Channel, y = Device, fill = avg_adspend)) +
  geom_tile(color = "white") +
  geom_text(aes(label = dollar(round(avg_adspend))), color = "white", size = 5) +
  scale_fill_gradient(low = "#9ecae1", high = "#08306b", labels = dollar) +
  labs(title = "Average Ad Spend by Channel and Device", x = NULL, y = NULL,
       fill = "Avg. spend") +
  theme_minimal()
p_heat_spend
ggsave("figures/w3-1-channel-device-heatmap-adspend.pdf", width = 7, height = 3.5)

# The EDA loop in action: Video/Desktop stands out. Is it a real pattern or
# a few outliers in a small cell (53 customers)? Check the distribution.
ggplot(df[Channel == "Video"], aes(x = Device, y = Ad_Spend, fill = Device)) +
  geom_boxplot(alpha = 0.8) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Video channel: Ad Spend by Device", x = NULL, y = "Ad spend ($)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/w3-1-video-adspend-by-device-boxplot.pdf", width = 5, height = 3.5)
