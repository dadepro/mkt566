# ============================================================================
# MKT 566, Week 1: from default plot to presentation-ready, in 6 steps
# ============================================================================
# We build ONE chart, the most fundamental relationship in marketing (the
# demand curve: price vs. units sold), and improve it one step at a time.
# Each step repeats the previous code and adds a single thing, so compare
# consecutive steps to see exactly what changed.
#
# HOW TO RUN: click on a line and press Cmd+Enter (Mac) / Ctrl+Enter
# (Windows); the multi-line chart instructions (the lines glued by "+") run
# as one. Charts open in a VS Code tab. Lines starting with # are comments:
# notes for humans, ignored by R. If a line puzzles you, select it and ask
# your AI assistant.
# ============================================================================

# library() switches on an installed package for this session.
library(ggplot2)   # the charting package
library(ggthemes)  # extra ready-made chart looks (theme_few, used below)

# ---- housekeeping: safe to run without understanding -----------------------
# These lines point R at the folder that contains this script, so that file
# paths like "data/store-sales.csv" work no matter where you unzipped the
# code folder. If they stop the script with an error, follow the message.

# point R at the folder that contains this script, so "data/..." paths work
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
for (p in c("code", "w1/code", "mkt566/w1/code")) {
  if (!dir.exists("data") && dir.exists(file.path(p, "data"))) setwd(p)
}
if (!dir.exists("data")) {
  stop('R cannot find the "data" folder. In VS Code, use File > Open Folder ',
       'and open the unzipped "code" folder (the one that contains this script), ',
       'then run the script again from the top.')
}

# create the figure output folder if it does not exist
dir.create("chart-types", showWarnings = FALSE)

# Two years of weekly store-level sales of two products (Chapman & Feit).
# read.csv() loads the file into a data frame (R's table) named store.df.
store.df <- read.csv("data/store-sales.csv")
head(store.df)  # print the first 6 rows, to see what the data looks like
str(store.df)   # list every column and its type

# We visualize the most fundamental relationship in marketing — the demand
# curve: price (p1price) vs. weekly unit sales (p1sales) of product 1.

# Step 0: the default plot.
# How to read this: ggplot(data = ...) starts a chart from that dataset;
# geom_point() says "draw dots"; aes(x = ..., y = ...) says which column
# goes on which axis. The "+" glues the pieces together.
ggplot(data = store.df) +
  geom_point(mapping = aes(x = p1price, y = p1sales))

# What is wrong with it?
# - axis labels are cryptic (what is p1price?) and units are unclear
# - overplotting: prices take a few discrete values, points pile up
# - fonts are small, labels sit too close to the axes

# Step 1: label the axes (names + units).
# labs() sets human-readable labels, so the reader never has to decode
# column names. Always say the units ($, units, %).
ggplot(data = store.df) +
  geom_point(mapping = aes(x = p1price, y = p1sales)) +
  labs(x = "Price of P1 ($)", y = "Weekly Sales of P1 (units)")

# Step 2: larger fonts, more breathing room.
# theme() controls appearance details; element_text(size = ...) enlarges
# the fonts. The "\n" inside a label inserts a blank line: a cheap way to
# push the label away from the axis.
ggplot(data = store.df) +
  geom_point(mapping = aes(x = p1price, y = p1sales)) +
  labs(x = "\nPrice of P1 ($)", y = "Weekly Sales of P1 (units)\n") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12))
ggsave("chart-types/beautify-step2.png", width = 5, height = 3.5)
# ggsave() saves the most recent chart to a file (sizes are in inches).

# Step 3: a cleaner theme.
# theme_few() (from the ggthemes package) replaces the default grey
# background with a minimal look: less ink on decoration, more on data.
ggplot(data = store.df) +
  geom_point(mapping = aes(x = p1price, y = p1sales)) +
  labs(x = "\nPrice of P1 ($)", y = "Weekly Sales of P1 (units)\n") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12)) +
  theme_few()
ggsave("chart-types/beautify-step3.png", width = 5, height = 3.5)

# Step 4: fix the overplotting — jitter + transparency.
# Prices take only a few distinct values, so dots stack on top of each other
# and one visible dot can hide fifty. Two fixes, combined:
#   - geom_jitter() nudges each dot sideways by a tiny random amount
#     (width = 0.02, in $) so stacked dots spread out;
#   - alpha = 0.4 makes dots 60% transparent, so dense areas look darker.
ggplot(data = store.df) +
  geom_jitter(mapping = aes(x = p1price, y = p1sales),
              width = 0.02, alpha = 0.4) +
  labs(x = "\nPrice of P1 ($)", y = "Weekly Sales of P1 (units)\n") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12)) +
  theme_few()
ggsave("chart-types/beautify-step4.png", width = 5, height = 3.5)

# Step 5: add the trend.
# geom_smooth(method = "lm") overlays the best-fitting straight line
# ("lm" = linear model) with a grey band for its uncertainty.
ggplot(data = store.df) +
  geom_jitter(mapping = aes(x = p1price, y = p1sales),
              width = 0.02, alpha = 0.4) +
  labs(x = "\nPrice of P1 ($)", y = "Weekly Sales of P1 (units)\n") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12)) +
  geom_smooth(mapping = aes(x = p1price, y = p1sales),
              method = "lm", color = "blue") +
  theme_few()
ggsave("chart-types/beautify-step5.png", width = 5, height = 3.5)

# Step 6: add a title.
# A good title states the chart's takeaway, not a description of the axes.
# The downward slope is a demand curve: how much sales fall when price
# rises is the price elasticity — we will estimate it with regression in
# week 4.
ggplot(data = store.df) +
  geom_jitter(mapping = aes(x = p1price, y = p1sales),
              width = 0.02, alpha = 0.4) +
  labs(x = "\nPrice of P1 ($)", y = "Weekly Sales of P1 (units)\n",
       title = "Weekly P1 Sales Decline with Price") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12)) +
  geom_smooth(mapping = aes(x = p1price, y = p1sales),
              method = "lm", color = "blue") +
  theme_few()
ggsave("chart-types/beautify-step6.png", width = 5, height = 3.5)
