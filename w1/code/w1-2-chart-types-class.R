# ============================================================================
# MKT 566, Week 1: A tour of chart types in R
# ============================================================================
# This script builds every chart from the data-viz slides, one section per
# chart type, using real store sales data plus a few simulated marketing
# datasets.
#
# HOW TO RUN THIS SCRIPT (the workflow for the whole course):
#   - Run it from the top, one step at a time: click on a line and press
#     Cmd+Enter (Mac) or Ctrl+Enter (Windows).
#   - A chart is built by one long instruction that spans several lines
#     (the lines glued together by "+"). Cmd+Enter runs the whole
#     instruction at once, so you do not need to select the lines yourself.
#   - Each chart opens in a VS Code tab. The ggsave() line right after each
#     chart also saves it as a PDF in the "chart-types" folder.
#   - Lines starting with # are comments: notes for humans, ignored by R.
#
# You are NOT expected to memorize this code. Read the comment above each
# chart, run the code, and look at the result. If you want to understand a
# specific line, select it and ask your AI assistant to explain it.
# ============================================================================

#### install libraries ####
# Packages are add-ons to R, like apps on a phone. This block installs the
# ones this script needs, skipping any you already have. Installing happens
# once per computer, so if you followed the setup guide you can run it and
# nothing will happen.
pkgs <- c(
  "ggplot2","dplyr","forcats","tidyr","treemapify","scales",
  "data.table","ggthemes","patchwork","RColorBrewer",
  "sf","rnaturalearth","rnaturalearthdata"
)

install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
# install.packages() puts a package on your computer (once); library()
# switches it on for the current session (every time you restart R).
library(ggplot2)      # the charting package we use all semester
library(dplyr)        # data-manipulation verbs: arrange, mutate, group_by...
library(forcats)      # tools for categorical variables (e.g. reordering bars)
library(tidyr)        # reshaping tables (used once, for the stacked area)
library(treemapify)   # treemaps
library(scales)       # nicer axis labels (thousands separators, percents)
library(data.table)   # a fast cousin of the data frame (used once, way below)
library(ggthemes)     # extra ready-made looks for charts (theme_few below)
library(patchwork)    # place two charts side by side with a simple +
library(RColorBrewer) # ready-made color palettes
# maps
library(sf)
library(rnaturalearth)

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

# ---------------------------------------------------------------
# The data
# ---------------------------------------------------------------
# read.csv() reads a spreadsheet-style file into a "data frame": R's table,
# with rows = observations and columns = variables. We give each table a
# name (store.df, df_skills) so later lines can refer to it.
#   store.df:  two years of weekly, store-level sales of two products
#              (real data from Chapman & Feit's marketing analytics book)
#   df_skills: number of U.S. job postings mentioning each coding skill
# The simulated datasets loaded later were made by w1-2-simulate-datasets.R.
store.df  <- read.csv("data/store-sales.csv")
df_skills <- read.csv("data/skills-postings.csv")
# Tip: run head(store.df) to see the first rows, str(store.df) for a summary.

# ---------------------------------------------------------------
# HOW TO READ GGPLOT CODE (the same pattern repeats in every chart)
# ---------------------------------------------------------------
#   ggplot(data, aes(x = ..., y = ...))  start a chart: pick the dataset and
#                                        say which column goes on which axis
#                                        ("aes" is short for aesthetics)
#   + geom_col() / geom_line() / ...     pick the chart type: bars, lines,
#                                        points, tiles, areas...
#   + labs(...)                          title and axis labels
#   + theme_minimal() / theme_few() ...  the overall look
# The "+" glues layers together, bottom layer first.
# One quirk you will see in labels: "\n" inserts a blank line, a cheap way
# to push a label a little away from the axis.

# ----------------------------------------------------------------------------
# 0) ANSCOMBE'S QUARTET: why look at data at all
# ----------------------------------------------------------------------------
#    Four datasets with identical means, variances, correlation (r = 0.82),
#    and regression line (y = 3 + 0.5x), but very different shapes.
#    The lesson: summary statistics can hide what is going on. Plot first.
#    (anscombe is a small dataset built into R; the code below just stacks
#    its four x,y pairs into one table with a "set" label for the panels.)
ans <- data.frame(
  set = rep(c("I", "II", "III", "IV"), each = 11),
  x   = c(anscombe$x1, anscombe$x2, anscombe$x3, anscombe$x4),
  y   = c(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4)
)
ggplot(ans, aes(x, y)) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick",
              linewidth = .7, fullrange = TRUE) +   # the fitted line
  geom_point(size = 2, color = "steelblue") +       # the actual data
  facet_wrap(~set) +   # facet_wrap() draws one small panel per value of set
  labs(x = "\nx", y = "y\n") +
  theme_few()
ggsave("chart-types/anscombe.pdf", width = 6, height = 4)
# ggsave() saves the most recent chart to a file (sizes are in inches).

# ----------------------------------------------------------------------------
# 1) BAR PLOT: comparing counts across categories
# ----------------------------------------------------------------------------
#    The workhorse. One bar per skill, bar height = number of job postings.
#    X: skill; Y: count; purpose: quick comparison across categories.
ggplot(df_skills, aes(x = skill, y = count)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "U.S. Job Postings by Skill",
    x     = NULL,
    y     = "Number of Postings\n"
  ) +
  theme_minimal()
ggsave("chart-types/bar-plot.pdf", w = 5, h = 3.5)

# reorder barplot, much more useful
# Same chart with bars sorted largest to smallest. Almost always do this:
# alphabetical order carries no information, sorted order tells the story.
# fct_reorder(skill, -count) = "order the skills by count, descending".
ggplot(df_skills, aes(x = fct_reorder(skill, -count), y = count)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "U.S. Job Postings by Skill",
    x     = NULL,
    y     = "Number of Postings\n"
  ) +
  theme_minimal()
ggsave("chart-types/bar-plot-reordered.pdf", w = 5, h = 3.5)

# ----------------------------------------------------------------------------
# 2) PARETO CHART: bars for counts + line for cumulative share
# ----------------------------------------------------------------------------
#    Answers: "how much of the total do the top categories account for?"
#    Left Y: count; Right Y: cumulative %; purpose: identify the "vital few"
#    skills contributing most of the demand (the 80/20 heuristic).
#
#    First, prepare the data. %>% is the "pipe": it hands the result of one
#    step to the next step. Read it as "then": take df_skills, THEN sort it,
#    THEN add two new columns.
df_pareto <- df_skills %>%
  arrange(desc(count)) %>%   # sort rows, biggest count first
  mutate(                    # mutate() adds new columns:
    cum_pct = cumsum(count) / sum(count) * 100,  # running % of the total
    skill   = fct_reorder(skill, -count)  # order bars descending
  )

# One chart, two y-axes. ggplot only allows a second axis that is a rescaled
# copy of the first, so we shrink the percentages onto the bar scale
# (cum_pct * max(count) / 100) and let sec_axis() undo the math to label
# the right-hand axis.
ggplot(df_pareto, aes(x = skill, y = count)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(y = cum_pct * max(count) / 100, group = 1),
            color = "firebrick", linewidth = 1) +
  geom_point(aes(y = cum_pct * max(count) / 100),
             color = "firebrick", size = 2) +
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(
      ~ . / max(df_pareto$count) * 100,
      name = "Cumulative %"
    )
  ) +
  labs(title = "Pareto of Coding-Skill Demand") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, max(df_pareto$count) * 1.05))
ggsave("chart-types/pareto-chart.pdf", w = 5, h = 3.5)

# ----------------------------------------------------------------------------
# 2b) THE 80/20 RULE: cumulative revenue share across customers
# ----------------------------------------------------------------------------
#     Rank customers from biggest spender to smallest, then plot the share of
#     total revenue as customers are added one by one. The steep start IS
#     revenue concentration: a few customers generate most of the revenue.
#     This picture is the case for segmentation and customer lifetime value.
customers <- read.csv("data/customer-revenue.csv")
rev <- sort(customers$revenue, decreasing = TRUE)  # revenues, biggest first
d <- data.frame(
  pct_customers = (1:nrow(customers)) / nrow(customers) * 100,
  pct_revenue   = cumsum(rev) / sum(rev) * 100     # running % of revenue
)
share20 <- round(d$pct_revenue[d$pct_customers == 20])  # share of the top 20%
ggplot(d, aes(pct_customers, pct_revenue)) +
  geom_line(linewidth = 1, color = "steelblue") +
  # the dashed guide lines and the note are drawn "by hand":
  geom_segment(aes(x = 20, xend = 20, y = 0, yend = share20), linetype = "dashed") +
  geom_segment(aes(x = 0, xend = 20, y = share20, yend = share20), linetype = "dashed") +
  annotate("text", x = 24, y = share20 - 7, hjust = 0, size = 4,
           label = paste0("Top 20% of customers\n= ", share20, "% of revenue")) +
  labs(title = "Customer Revenue Concentration (simulated)",
       x = "\n% of customers, ranked by revenue", y = "Cumulative % of revenue\n") +
  theme_few()
ggsave("chart-types/pareto-customers.pdf", w = 5, h = 3.5)

# ----------------------------------------------------------------------------
# 3) TREEMAP: rectangles whose area is proportional to the numbers
# ----------------------------------------------------------------------------
#    Each rectangle is a skill; its AREA is its share of postings. A quick
#    space-filling overview of "who is big, who is small", handy when there
#    are too many categories for a bar chart.
ggplot(df_skills, aes(area = count, fill = skill, label = skill)) +
  geom_treemap() +
  geom_treemap_text(reflow = TRUE, colour = "white") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("chart-types/treemap.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 4) PIE CHART: shares of a whole
# ----------------------------------------------------------------------------
#    Fine for two or three slices, hard to read beyond that (try comparing
#    the small slices by eye). In ggplot a pie is literally a single stacked
#    bar (geom_col) bent into a circle by coord_polar().
df_pie <- df_skills %>%
  arrange(desc(count)) %>%
  mutate(
    pct = count / sum(count) * 100,   # each skill's share, in percent
    legend_lbl = paste0(skill, " (", round(pct, 1), "%)")  # legend text
  )

ggplot(df_pie, aes(x = 1, y = pct, fill = legend_lbl)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +   # wrap the bar around a circle
  theme_void() +               # theme_void() = no axes at all
  labs(
    title = "Market Share of Coding Skills",
    fill  = NULL
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10)
  )
ggsave("chart-types/pie-chart.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 5) WATERFALL: how the parts add up to the total
# ----------------------------------------------------------------------------
#    Each bar starts where the previous one ended, so you see every skill's
#    increment AND the running total at the same time. In business decks
#    this is the classic "revenue bridge" chart (start, gains, losses, end).
df_wf <- df_skills %>%
  arrange(desc(count)) %>%
  mutate(
    cum   = cumsum(count),          # running total after adding this skill
    prev  = lag(cum, default = 0),  # lag() = the previous row's value
    type  = "gain",
    skill = factor(skill, levels = skill)  # lock in order
  )
df_wf$idx <- as.numeric(df_wf$skill)  # bar positions 1, 2, 3, ...

# geom_rect() draws each bar from its four corners: from prev up to cum.
ggplot(df_wf, aes(x = idx)) +
  geom_rect(aes(
    xmin = idx - 0.4,
    xmax = idx + 0.4,
    ymin = prev,
    ymax = cum,
    fill = type
  )) +
  geom_text(aes(y = cum + max(cum) * 0.02, label = comma(count)), size = 3) +
  scale_fill_manual(values = c(gain = "forestgreen", loss = "firebrick")) +
  scale_x_continuous(breaks = df_wf$idx, labels = levels(df_wf$skill)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.12))) +
  labs(
    title = "Waterfall Chart of Coding Skill Demand",
    x = NULL, y = "Cumulative Count", fill = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
ggsave("chart-types/waterfall-chart.pdf", width = 5, height = 3.5)

# -----------------------------------------
# Store data: collapse to one row per week
# -----------------------------------------
# store.df has one row per store per week. For the time-series charts below
# we want one row per week, so we group_by(Week) and summarise() each group
# into totals and averages. This group-then-summarise move is one you will
# use constantly in this course.
weekly <- store.df %>%
  group_by(Week) %>%   # split the rows into one group per week...
  summarise(           # ...then collapse each group to a single row:
    TotalP1    = sum(p1sales, na.rm = TRUE),
    AvgP1      = mean(p1sales, na.rm = TRUE),
    AvgP1price = mean(p1price, na.rm = TRUE),
    TotalP2    = sum(p2sales, na.rm = TRUE)
  ) %>%
  arrange(Week) %>%
  mutate(CumTotalP1 = cumsum(TotalP1))  # running total across the year
# (na.rm = TRUE tells R to ignore missing values instead of returning NA.)

# ----------------------------------------------------------------------------
# 6) LINE CHART: one number over time
# ----------------------------------------------------------------------------
#    THE chart for time series. X: week; Y: total P1 sales; purpose: see
#    trend, seasonality, and unusual spikes at a glance.
ggplot(weekly, aes(x = Week, y = TotalP1)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +  # x labels every 4 weeks
  labs(
    title = "Weekly Sales of P1",
    x = "\nWeek of Year",
    y = "Total P1 Sales\n"
  ) +
  theme_bw()
ggsave("chart-types/line-chart.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 7) STACKED AREA: how a total splits into parts over time
# ----------------------------------------------------------------------------
#    The two products are stacked: the top edge is total sales, each band's
#    thickness is that product's contribution.
#    ggplot wants "long" data for this: one row per week PER product, not one
#    row per week with two sales columns. pivot_longer() does that reshape
#    (-Week means "reshape every column except Week").
weekly_long <- weekly %>%
  select(Week, TotalP1, TotalP2) %>%   # keep only the columns we need
  pivot_longer(-Week, names_to = "Series", values_to = "Sales")

ggplot(weekly_long, aes(x = Week, y = Sales, fill = Series)) +
  geom_area(alpha = 0.7) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  labs(
    title = "Weekly Sales: P1 vs P2",
    x = "Week of Year",
    y = "Sales"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("chart-types/stacked-area-chart.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 8) BAR + LINE COMBO: two related series with very different scales
# ----------------------------------------------------------------------------
#    Bars: average weekly sale (left axis). Line: cumulative sales for the
#    year (right axis). The cumulative total is hundreds of times larger, so
#    we shrink it onto the bar scale ("ratio") and let sec_axis() undo the
#    math on the right axis. Use two-axis charts sparingly: they are easy
#    to misread.
ratio <- max(weekly$AvgP1, na.rm = TRUE) / max(weekly$CumTotalP1, na.rm = TRUE)

ggplot(weekly, aes(x = Week)) +
  geom_bar(aes(y = AvgP1), stat = "identity", fill = "steelblue") +
  geom_line(aes(y = CumTotalP1 * ratio), color = "firebrick", linewidth = 0.5) +
  geom_point(aes(y = CumTotalP1 * ratio), color = "firebrick", size = 0.5) +
  scale_x_continuous(breaks = seq(1, 52, by = 4), "\nWeek") +
  scale_y_continuous(
    name = "Avg P1 Sale\n",
    labels = comma,
    sec.axis = sec_axis(
      ~ . / ratio,
      name   = "Cumulative P1 Sales\n",
      labels = comma,
      breaks = pretty_breaks(n = 5)
    )
  ) +
  labs(title = "Avg vs. Cumulative P1 Sales by Week") +
  theme_minimal()
ggsave("chart-types/bar-line-combo.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 9) HISTOGRAM: the distribution of one variable
# ----------------------------------------------------------------------------
#    Chop p1sales into 30 equal-width bins and count observations per bin.
#    Look for: the typical value, the spread, skewness, outliers.
ggplot(store.df, aes(x = p1sales)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Histogram of P1 Sales",
    x     = "P1 Sales",
    y     = "Frequency\n"
  ) +
  theme_minimal()
ggsave("chart-types/histogram.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 10) DENSITY: a smoothed histogram
# ----------------------------------------------------------------------------
#     Same information as the histogram, drawn as a smooth curve: easier to
#     see the shape (and to compare groups), at the cost of hiding counts.
ggplot(store.df, aes(x = p1sales)) +
  geom_density(fill = "red", alpha = 0.6) +
  labs(
    title = "Density Estimate of P1 Sales",
    x     = "P1 Sales",
    y     = "Density"
  ) +
  theme_minimal()
ggsave("chart-types/density-plot.pdf", width = 5, height = 3.5)

# Flag promoted vs not (for grouping in the next plots).
# ifelse(condition, value_if_yes, value_if_no) works row by row; factor()
# marks the result as a categorical variable.
store.df <- store.df %>%
  mutate(PromoFlag = factor(ifelse(p1prom > 0, "Promoted", "Not Promoted")))

# ----------------------------------------------------------------------------
# 11) BOXPLOT: compare distributions across groups
# ----------------------------------------------------------------------------
#     How to read a box: the middle line is the median, the box covers the
#     middle 50% of the data, the whiskers cover most of the rest, and the
#     red dots are outliers. Here: sales in promotion weeks vs. other weeks.
ggplot(store.df, aes(x = PromoFlag, y = p1sales, fill = PromoFlag)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  scale_fill_manual(values = c("Not Promoted" = "grey70", "Promoted" = "steelblue")) +
  labs(
    title = "P1 Sales by Promotion Status",
    x     = "",
    y     = "P1 Sales"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("chart-types/boxplot-by-promo.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 12) VIOLIN + BOX: the full shape plus the summary
# ----------------------------------------------------------------------------
#     The violin's width shows how common each sales level is (the full
#     distribution); the slim boxplot inside adds the median and quartiles.
ggplot(store.df, aes(x = PromoFlag, y = p1sales, fill = PromoFlag)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) +
  scale_fill_manual(values = c("Not Promoted" = "grey80", "Promoted" = "navy")) +
  labs(
    title = "Violin Plot of P1 Sales by Promotion Status",
    x     = "",
    y     = "P1 Sales"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("chart-types/violin-boxplot.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 12b) ONLINE RATINGS: a distribution that is far from bell-shaped
# ----------------------------------------------------------------------------
#      Star ratings pile up at 5 and at 1 (a "J shape"): delighted and angry
#      customers write reviews, lukewarm ones stay silent. The average
#      rating (about 3.8 here) describes almost nobody. Another reason to
#      look at distributions, not just means.
ratings <- read.csv("data/online-ratings.csv")
ratings$stars <- factor(ratings$stars)  # treat 1-5 as categories, not numbers
ggplot(ratings, aes(x = stars)) +
  geom_bar(fill = "steelblue") +   # geom_bar counts the rows in each category
  labs(
    title = "Distribution of Online Review Ratings (simulated marketplace)",
    x     = "\nStar rating",
    y     = "Number of reviews\n"
  ) +
  theme_few()
ggsave("chart-types/ratings-jshape.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 13) SCATTER PLOT: the relationship between two variables
# ----------------------------------------------------------------------------
#     One dot per store-week. Do weeks with high P1 sales tend to have high
#     or low P2 sales? alpha = 0.6 makes dots semi-transparent, so places
#     where many dots overlap show up darker.
ggplot(store.df, aes(x = p1sales, y = p2sales)) +
  geom_point(alpha = 0.6) +
  labs(
    x = "Sales of P1",
    y = "Sales of P2"
  ) +
  theme_bw()
ggsave("chart-types/scatter-plot.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 14) BUBBLE CHART: a third variable as dot size
# ----------------------------------------------------------------------------
#     Weekly total P1 sales over time, with dot size = average price that
#     week. Check: are the big-sales weeks the small-dot (low-price) weeks?
ggplot(weekly, aes(x = Week, y = TotalP1, size = AvgP1price)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  labs(
    title = "Weekly P1 Sales (size = Avg. Price)",
    x     = "Week of Year",
    y     = "Total P1 Sales"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("chart-types/bubble-chart.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 14b) AD SPEND: diminishing returns to advertising
# ----------------------------------------------------------------------------
#      Each dot is a week. geom_smooth(method = "loess") draws a flexible
#      curve that follows the data, with no straight-line assumption. The
#      curve is concave: doubling ad spend does not double sales. A scatter
#      plus a smoother is the first diagnostic for any budget question.
ad <- read.csv("data/ad-spend.csv")
ggplot(ad, aes(spend, sales)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "firebrick") +
  labs(
    title = "Weekly Ad Spend vs. Incremental Sales (simulated)",
    x = "\nWeekly ad spend ($000)",
    y = "Incremental sales ($000)\n"
  ) +
  theme_few()
ggsave("chart-types/adspend.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 15) CHOROPLETH MAP: color countries by a number
# ----------------------------------------------------------------------------
#     Three steps: (1) total P1 sales by country (the `country` column holds
#     2-letter ISO codes); (2) get every country's shape from the
#     rnaturalearth package; (3) left_join() attaches our sales number to
#     each shape by matching the country codes. Countries we have no sales
#     for get NA and are drawn in grey. geom_sf() draws the shapes.
p1sales.sum <- store.df %>%
  group_by(country) %>%
  summarise(x = sum(p1sales, na.rm = TRUE)) %>%
  mutate(country = toupper(country))  # match the map's UPPERCASE codes

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a2, name, geometry)

world_join <- world %>%
  left_join(p1sales.sum, by = c("iso_a2" = "country"))

ggplot(world_join) +
  geom_sf(aes(fill = x), color = NA) +
  scale_fill_gradientn(
    colours = brewer.pal(7, "Greens"),
    na.value = "grey90",
    name = "P1 sales"
  ) +
  labs(title = "Total P1 Sales by Country\n") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "right",
        plot.margin = margin(5.5, 30, 5.5, 5.5),
        legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8))
ggsave("chart-types/choropleth-map.pdf", width = 6, height = 4)

# ----------------------------------------------------------------------------
# 16) HEATMAP: a table painted with color
# ----------------------------------------------------------------------------
#     Rows: stores. Columns: deciles of P1 sales (decile 1 = the lowest 10%
#     of sales-weeks, decile 10 = the highest). Color = how many of that
#     store's weeks fall in each decile, so one glance shows which stores
#     lean high or low.
#     (This block uses data.table, a faster cousin of the data frame. cut()
#     assigns each week to its decile bucket; .N counts rows per group.)
store.dt <- as.data.table(store.df)
store.dt[, p1sales_decile := cut(
  p1sales,
  breaks = quantile(p1sales, probs = seq(0, 1, 0.1), na.rm = TRUE),
  include.lowest = TRUE
)]
heatmap_data <- store.dt[, .(count = .N), by = .(storeNum, p1sales_decile)]

ggplot(heatmap_data, aes(x = p1sales_decile, y = factor(storeNum), fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue", "Number of sales") +
  labs(
    title = "Heatmap of P1 Sales Deciles by Store",
    x     = "\nP1 Sales Decile",
    y     = "Store Number"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("chart-types/heatmap.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# 16b) COHORT RETENTION HEATMAP: the marketing classic
# ----------------------------------------------------------------------------
#      Each row is a cohort: the customers acquired in a given month. Moving
#      right, the numbers show the % of them still active as months pass.
#      Compare rows to see whether newer cohorts retain better than older
#      ones; every subscription business lives and dies by this chart.
cohort_df <- read.csv("data/cohort-retention.csv")
ggplot(cohort_df,
       aes(x = months_since,
           y = fct_reorder(cohort, -cohort_month),  # newest cohort at the bottom
           fill = retained_pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = retained_pct), size = 3) +  # print the % in each cell
  scale_fill_gradient(low = "white", high = "steelblue", name = "% retained") +
  scale_x_continuous(breaks = 0:11) +
  labs(
    title = "Customer Retention by Acquisition Cohort (simulated)",
    x     = "\nMonths since acquisition",
    y     = NULL
  ) +
  theme_minimal()
ggsave("chart-types/cohort-retention.pdf", width = 6, height = 4)

# ----------------------------------------------------------------------------
# GROUPING vs FACETING: two ways to compare subgroups
# ----------------------------------------------------------------------------
# Option 1, grouping: both years in ONE panel, told apart by color.
# stat_summary(fun = "mean") plots, for each week, the AVERAGE sales across
# stores instead of a separate dot for every store.
ggplot(store.df, aes(x = Week, y = p1sales, group = factor(Year), color = factor(Year))) +
  stat_summary(geom = "line", fun = "mean") +
  scale_color_manual("Year", values = c("blue", "red")) +
  labs(
    title = "P1 Sales by Year",
    x     = "Week of Year",
    y     = "P1 Sales"
  ) +
  theme_bw() +
  theme(legend.position = "top")
ggsave("chart-types/grouping-example.pdf", width = 5, height = 3.5)

# Option 2, faceting: one small panel per year, same axes. Better when lines
# overlap too much or there are many groups.
ggplot(store.df, aes(x = Week, y = p1sales)) +
  stat_summary(geom = "line", fun = "mean") +
  facet_wrap(~ factor(Year), ncol = 2,
             labeller = labeller(`factor(Year)` = c(`1` = "Year 1", `2` = "Year 2"))) +
  labs(
    title = "P1 Sales by Year",
    x     = "Week of Year",
    y     = "P1 Sales"
  ) +
  theme_bw()
ggsave("chart-types/facet-example.pdf", width = 6, height = 3)

# ----------------------------------------------------------------------------
# ANNOTATION: point at the story inside the chart
# ----------------------------------------------------------------------------
# geom_vline() draws a vertical reference line; annotate() places text at
# the coordinates you give it. Mark events (campaign launches, price
# changes) so the reader does not have to find the story alone.
ggplot(weekly, aes(x = Week, y = TotalP1)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 26, linetype = "dashed", color = "firebrick") +
  annotate("text", x = 27, y = max(weekly$TotalP1) * 1.035, hjust = 0,
           label = "Campaign launch (illustrative)", color = "firebrick", size = 4) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  scale_y_continuous(expand = expansion(mult = c(.05, .1))) +
  labs(x = "\nWeek of Year", y = "Total P1 Sales\n") +
  theme_few()
ggsave("chart-types/annotate-example.pdf", width = 5, height = 3.5)

# ----------------------------------------------------------------------------
# HOW TO MISLEAD WITH A CHART: the truncated y-axis
# ----------------------------------------------------------------------------
# The same two numbers, plotted twice. Left: the y-axis starts at 80.5, so a
# 1.2-point gap looks like a landslide. Right: the axis starts at 0 and the
# difference nearly disappears. coord_cartesian(ylim = ...) is what zooms
# the axis. Bar charts should almost always start at zero; when you see one
# that does not, ask why.
d <- data.frame(brand = c("Our brand", "Competitor"), sat = c(82.3, 81.1))
p1 <- ggplot(d, aes(brand, sat, fill = brand)) +
  geom_col(width = .6) +
  coord_cartesian(ylim = c(80.5, 82.6)) +
  scale_fill_manual(values = c("steelblue", "grey70")) +
  labs(title = "What the sales deck shows", x = NULL, y = "Satisfaction (%)") +
  theme_few() + theme(legend.position = "none")
p2 <- ggplot(d, aes(brand, sat, fill = brand)) +
  geom_col(width = .6) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(values = c("steelblue", "grey70")) +
  labs(title = "What the data says", x = NULL, y = "Satisfaction (%)") +
  theme_few() + theme(legend.position = "none")
p1 + p2   # patchwork: side by side
ggsave("chart-types/misleading-axis.pdf", width = 8, height = 3.8)
