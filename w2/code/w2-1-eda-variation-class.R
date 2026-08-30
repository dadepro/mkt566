# ============================================================================
# MKT 566, Week 2: Exploratory Data Analysis -- Variation
# ============================================================================
# This script builds every chart from the EDA slides: distributions of the
# datarium marketing dataset, boxplots for outliers (raw and log scale), and
# time-series trends with the ggplot2 economics dataset.
#
# HOW TO RUN THIS SCRIPT (same workflow as week 1):
#   - Run it from the top, one step at a time: click on a line and press
#     Cmd+Enter (Mac) or Ctrl+Enter (Windows).
#   - A chart is built by one long instruction that spans several lines
#     (the lines glued together by "+"). Cmd+Enter runs the whole
#     instruction at once, so you do not need to select the lines yourself.
#   - Each chart opens in a VS Code tab. The ggsave() line right after each
#     chart also saves it as a PDF in the "figures" folder.
#   - Lines starting with # are comments: notes for humans, ignored by R.
#
# You are NOT expected to memorize this code. Read the comment above each
# chart, run the code, and look at the result. If you want to understand a
# specific line, select it and ask your AI assistant to explain it.
# ============================================================================

#### install libraries ####
# This block installs the packages this script needs, skipping any you
# already have (so it is always safe to run).
pkgs <- c("datarium", "ggplot2", "dplyr", "tidyr", "data.table", "patchwork")

install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(datarium)   # the "marketing" dataset we explore today
library(ggplot2)    # charts (and the "economics" dataset for trends)
library(dplyr)      # data-manipulation verbs
library(tidyr)      # pivot_longer(): reshape wide data into long format
library(data.table) # fast tables; we use its [ ] filtering once below
library(patchwork)  # place charts side by side with a simple +

# ---- housekeeping: safe to run without understanding -----------------------
# These lines point R at the folder that contains this script, so the
# "figures" output folder lands next to it no matter where you unzipped
# the code folder.
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
for (p in c("code", "w2/code", "mkt566/w2/code")) {
  if (!dir.exists("data") && dir.exists(file.path(p, "data"))) setwd(p)
}
if (!dir.exists("data")) {
  stop('R cannot find the "data" folder. In VS Code, use File > Open Folder ',
       'and open the unzipped "code" folder (the one that contains this script), ',
       'then run the script again from the top.')
}

# create the figure output folder if it does not exist
dir.create("figures", showWarnings = FALSE)

# ---------------------------------------------------------------
# The data: a marketing experiment
# ---------------------------------------------------------------
# The "marketing" dataset ships with the datarium package (no CSV needed):
# 200 observations of the advertising budget spent on three channels
# (youtube, facebook, newspaper) and the resulting sales.

# Always look at the data before charting it:
head(marketing)   # the first 6 rows
nrow(marketing)   # how many observations

# ...and compute summary statistics. One line, very informative: ranges,
# skew (compare mean vs. median), and missing values would show up here.
summary(marketing)

# ---------------------------------------------------------------
# Visualizing distributions: histograms and the binwidth choice
# ---------------------------------------------------------------
# A histogram chops the variable's range into bins and counts observations
# in each. The binwidth is YOUR choice, and it changes what you see.

# 1) binwidth = 10: each bar covers 10 units of sales. Too coarse --
#    three bars, almost no information.
ggplot(marketing, aes(x = sales)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Sales", x = "Sales", y = "Frequency") +
  theme_minimal()
ggsave("figures/w2-1-eda-sales-distribution-bw10.pdf", h = 3.5, w = 5)

# 2) binwidth = 1: now the shape appears -- most values between 10 and 20,
#    with a right tail of strong weeks.
ggplot(marketing, aes(x = sales)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Sales", x = "Sales", y = "Frequency") +
  theme_minimal()
ggsave("figures/w2-1-eda-sales-distribution-bw1.pdf", h = 3.5, w = 5)

# 3) binwidth = 0.1: too fine -- mostly noise. Always try a few values.
ggplot(marketing, aes(x = sales)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Sales", x = "Sales", y = "Frequency") +
  theme_minimal()
ggsave("figures/w2-1-eda-sales-distribution-bw01.pdf", h = 3.5, w = 5)

# ---------------------------------------------------------------
# Spend across the three channels
# ---------------------------------------------------------------
# Same variable type (ad spend), three very different shapes.

# YouTube spend: spread almost uniformly across its range
p_yt <- ggplot(marketing, aes(x = youtube)) +
  geom_histogram(binwidth = 10, fill = "#FF0000", color = "white") +
  labs(title = "YouTube", x = "Spend", y = "Frequency") +
  theme_minimal()
p_yt
ggsave("figures/w2-1-eda-youtube-distribution-bw10.pdf", h = 3.5, w = 5)

# Facebook spend: flat-ish with a low mode
p_fb <- ggplot(marketing, aes(x = facebook)) +
  geom_histogram(binwidth = 5, fill = "#1877F2", color = "white") +
  labs(title = "Facebook", x = "Spend", y = "Frequency") +
  theme_minimal()
p_fb
ggsave("figures/w2-1-eda-facebook-distribution-bw5.pdf", h = 3.5, w = 5)

# Newspaper spend: right-skewed -- a few campaigns spend a lot
p_np <- ggplot(marketing, aes(x = newspaper)) +
  geom_histogram(binwidth = 5, fill = "grey20", color = "white") +
  labs(title = "Newspaper", x = "Spend", y = "Frequency") +
  theme_minimal()
p_np
ggsave("figures/w2-1-eda-newspaper-distribution-bw5.pdf", h = 3.5, w = 5)

# patchwork glues charts together with +, like in the slides
p_yt + p_fb + p_np
ggsave("figures/w2-1-eda-spend-three-channels.pdf", h = 3.2, w = 11)

# ---------------------------------------------------------------
# Looking for outliers: boxplots
# ---------------------------------------------------------------
# A boxplot flags outliers with a mathematical rule: any point beyond
# 1.5 x IQR from the box, where IQR = Q3 - Q1 (the interquartile range).

# To draw the three channels in one chart we first reshape the data from
# wide (one column per channel) to long (one row per observation-channel).
marketing_long <- data.table(
  pivot_longer(marketing, cols = everything(),
               names_to = "variable", values_to = "value")
)

# Raw scale: newspaper shows outliers, but the channels live on very
# different scales, so the small ones are squashed.
ggplot(marketing_long[variable != "sales"],
       aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1877F2", "grey20", "#FF0000")) +
  labs(title = "Boxplot of Marketing Variables", x = "Channel", y = "Ad Spend") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/w2-1-eda-marketing-boxplot.pdf", width = 6, height = 4)

# Log scale: a fair comparison, and low-spend outliers become visible too.
ggplot(marketing_long[variable != "sales"],
       aes(x = variable, y = log10(value), fill = variable)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1877F2", "grey20", "#FF0000")) +
  labs(title = "Boxplot of Marketing Variables (Log Scale)",
       x = "Channel", y = "log Ad Spend") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/w2-1-eda-marketing-boxplot-log.pdf", width = 6, height = 4)

# ---------------------------------------------------------------
# Visualizing trends: the economics dataset
# ---------------------------------------------------------------
# The second face of variation: how a variable evolves over time.
# The "economics" dataset ships with ggplot2: monthly US indicators from
# FRED (https://fred.stlouisfed.org/). unemploy = unemployed, in thousands.

head(economics)

# A line chart is the default view for a time series.
ggplot(data = economics) +
  geom_line(mapping = aes(x = date, y = unemploy)) +
  labs(title = "Unemployment over time", x = "Date",
       y = "Unemployed (thousands)")
ggsave("figures/w2-1-eda-economics-unemployment.png", width = 6, height = 4)

# Add a fitted line to summarize the long-run directional trend.
ggplot(data = economics) +
  geom_line(mapping = aes(x = date, y = unemploy)) +
  labs(title = "Unemployment over time", x = "Date",
       y = "Unemployed (thousands)") +
  geom_smooth(mapping = aes(x = date, y = unemploy),
              method = "lm", color = "blue")
ggsave("figures/w2-1-eda-economics-unemployment-trend.png", width = 6, height = 4)

# Annotate events: dashed lines at the unemployment peaks that followed
# US recessions. Each spike has a cause -- annotations turn a line into
# a story.
ggplot(data = economics) +
  geom_line(mapping = aes(x = date, y = unemploy)) +
  labs(title = "Unemployment over time", x = "Date",
       y = "Unemployed (thousands)") +
  geom_vline(xintercept = as.Date(c("1975-03-01", "1983-01-01", "1992-08-01",
                                    "2003-08-01", "2009-11-01")),
             color = "red", linetype = "dashed")
ggsave("figures/w2-1-eda-economics-unemployment-peaks.png", width = 6, height = 4)
