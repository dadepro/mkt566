#### install libraries ####
pkgs <- c(
  "ggplot2","dplyr","forcats","tidyr","treemapify","scales",
  "data.table","ggthemes","patchwork","RColorBrewer",
  "sf","rnaturalearth","rnaturalearthdata"
)

install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)
library(treemapify)
library(scales)
library(data.table)
library(ggthemes)
library(patchwork)
library(RColorBrewer)
# maps
library(sf)
library(rnaturalearth)

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
# Data: real store sales (Chapman & Feit) + simulated marketing
# datasets (see w1-2-simulate-datasets.R for how they were made)
# ---------------------------------------------------------------
store.df  <- read.csv("data/store-sales.csv")
df_skills <- read.csv("data/skills-postings.csv")

# 0) ANSCOMBE'S QUARTET — why visualize at all
#    Four datasets with identical means, variances, correlation (r = 0.82),
#    and regression line (y = 3 + 0.5x) — but very different shapes.
ans <- data.frame(
  set = rep(c("I", "II", "III", "IV"), each = 11),
  x   = c(anscombe$x1, anscombe$x2, anscombe$x3, anscombe$x4),
  y   = c(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4)
)
ggplot(ans, aes(x, y)) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick",
              linewidth = .7, fullrange = TRUE) +
  geom_point(size = 2, color = "steelblue") +
  facet_wrap(~set) +
  labs(x = "\nx", y = "y\n") +
  theme_few()
ggsave("chart-types/anscombe.pdf", width = 6, height = 4)

# 1) BAR PLOT — absolute frequency of postings by skill
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
ggplot(df_skills, aes(x = fct_reorder(skill, -count), y = count)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "U.S. Job Postings by Skill",
    x     = NULL,
    y     = "Number of Postings\n"
  ) +
  theme_minimal()
ggsave("chart-types/bar-plot-reordered.pdf", w = 5, h = 3.5)

# 2) PARETO CHART — bars for counts + line for cumulative share
#    Left Y: count; Right Y: cumulative %; purpose: identify "vital few" skills
#    contributing most of the demand (80/20 heuristic).
df_pareto <- df_skills %>%
  arrange(desc(count)) %>%
  mutate(
    cum_pct = cumsum(count) / sum(count) * 100,
    skill   = fct_reorder(skill, -count)  # order bars descending
  )

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

# 2b) THE 80/20 RULE — cumulative revenue share across customers
#     Purpose: revenue concentration; the case for segmentation and CLV.
customers <- read.csv("data/customer-revenue.csv")
rev <- sort(customers$revenue, decreasing = TRUE)
d <- data.frame(
  pct_customers = (1:nrow(customers)) / nrow(customers) * 100,
  pct_revenue   = cumsum(rev) / sum(rev) * 100
)
share20 <- round(d$pct_revenue[d$pct_customers == 20])
ggplot(d, aes(pct_customers, pct_revenue)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_segment(aes(x = 20, xend = 20, y = 0, yend = share20), linetype = "dashed") +
  geom_segment(aes(x = 0, xend = 20, y = share20, yend = share20), linetype = "dashed") +
  annotate("text", x = 24, y = share20 - 7, hjust = 0, size = 4,
           label = paste0("Top 20% of customers\n= ", share20, "% of revenue")) +
  labs(title = "Customer Revenue Concentration (simulated)",
       x = "\n% of customers, ranked by revenue", y = "Cumulative % of revenue\n") +
  theme_few()
ggsave("chart-types/pareto-customers.pdf", w = 5, h = 3.5)

# 3) TREEMAP — area ∝ count; each rectangle is a skill
#    Purpose: space-filling overview to see relative share visually.
ggplot(df_skills, aes(area = count, fill = skill, label = skill)) +
  geom_treemap() +
  geom_treemap_text(reflow = TRUE, colour = "white") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("chart-types/treemap.pdf", width = 5, height = 3.5)

# 4) PIE CHART — share of postings by skill
#    Purpose: quick composition view (use sparingly for many categories).
df_pie <- df_skills %>%
  arrange(desc(count)) %>%
  mutate(
    pct = count / sum(count) * 100,
    legend_lbl = paste0(skill, " (", round(pct, 1), "%)")
  )

ggplot(df_pie, aes(x = 1, y = pct, fill = legend_lbl)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(
    title = "Market Share of Coding Skills",
    fill  = NULL
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10)
  )
ggsave("chart-types/pie-chart.pdf", width = 5, height = 3.5)

# 5) WATERFALL — running (cumulative) total as each skill is added
#    Bars start at previous total and extend to new cumulative; purpose: show
#    incremental contribution of each skill to the grand total.
df_wf <- df_skills %>%
  arrange(desc(count)) %>%
  mutate(
    cum   = cumsum(count),
    prev  = lag(cum, default = 0),
    type  = "gain",
    skill = factor(skill, levels = skill)  # lock in order
  )
df_wf$idx <- as.numeric(df_wf$skill)

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
# Store data: weekly aggregations and plots
# -----------------------------------------
weekly <- store.df %>%
  group_by(Week) %>%
  summarise(
    TotalP1    = sum(p1sales, na.rm = TRUE),
    AvgP1      = mean(p1sales, na.rm = TRUE),
    AvgP1price = mean(p1price, na.rm = TRUE),
    TotalP2    = sum(p2sales, na.rm = TRUE)
  ) %>%
  arrange(Week) %>%
  mutate(CumTotalP1 = cumsum(TotalP1))

# 6) LINE CHART — weekly total sales of P1 over time
#    X: week; Y: total P1 sales; purpose: trend/seasonality.
ggplot(weekly, aes(x = Week, y = TotalP1)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  labs(
    title = "Weekly Sales of P1",
    x = "\nWeek of Year",
    y = "Total P1 Sales\n"
  ) +
  theme_bw()
ggsave("chart-types/line-chart.pdf", width = 5, height = 3.5)

# 7) STACKED AREA — composition of total sales by product (P1 vs P2)
#    X: week; Y: sales; fill: series; purpose: compare contributions over time.
weekly_long <- weekly %>%
  select(Week, TotalP1, TotalP2) %>%
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

# 8) BAR + LINE COMBO — bars: average weekly P1 sale; line: cumulative P1 (rescaled)
#    Left Y: AvgP1; Right Y: cumulative P1; purpose: level vs. running total.
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

# 9) HISTOGRAM — distribution of P1 sales across observations
#    Purpose: shape/spread of p1sales (skewness, outliers).
ggplot(store.df, aes(x = p1sales)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Histogram of P1 Sales",
    x     = "P1 Sales",
    y     = "Frequency\n"
  ) +
  theme_minimal()
ggsave("chart-types/histogram.pdf", width = 5, height = 3.5)

# 10) DENSITY — smoothed distribution of P1 sales
#     Purpose: compare to histogram; highlights modes.
ggplot(store.df, aes(x = p1sales)) +
  geom_density(fill = "red", alpha = 0.6) +
  labs(
    title = "Density Estimate of P1 Sales",
    x     = "P1 Sales",
    y     = "Density"
  ) +
  theme_minimal()
ggsave("chart-types/density-plot.pdf", width = 5, height = 3.5)

# Flag promoted vs not (for grouping in next plots)
store.df <- store.df %>%
  mutate(PromoFlag = factor(ifelse(p1prom > 0, "Promoted", "Not Promoted")))

# 11) BOXPLOT BY PROMO — distribution of P1 sales by promotion status
#     Purpose: compare medians/spread; shows outliers.
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

# 12) VIOLIN + BOX — shape + summary stats by promotion
#     Purpose: shows full distribution (kernel density) plus quartiles.
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

# 12b) ONLINE RATINGS — J-shaped distribution of review stars
#      Purpose: real-world distributions are often not bell-shaped; the mean
#      rating hides the polarization of who chooses to review.
ratings <- read.csv("data/online-ratings.csv")
ratings$stars <- factor(ratings$stars)
ggplot(ratings, aes(x = stars)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Online Review Ratings (simulated marketplace)",
    x     = "\nStar rating",
    y     = "Number of reviews\n"
  ) +
  theme_few()
ggsave("chart-types/ratings-jshape.pdf", width = 5, height = 3.5)

# 13) SCATTER — relationship between P1 and P2 sales
#     X: p1sales; Y: p2sales; purpose: visual correlation/association.
ggplot(store.df, aes(x = p1sales, y = p2sales)) +
  geom_point(alpha = 0.6) +
  labs(
    x = "Sales of P1",
    y = "Sales of P2"
  ) +
  theme_bw()
ggsave("chart-types/scatter-plot.pdf", width = 5, height = 3.5)

# 14) BUBBLE CHART — weekly total P1 sales with size as avg price
#     Purpose: verify whether more sales happen at lower prices.
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

# 14b) AD SPEND — diminishing returns to advertising
#      Purpose: scatter + smoother as the first diagnostic for budget questions;
#      the response curve is concave (doubling spend does not double sales).
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

# 15) CHOROPLETH MAP — total P1 sales by country
#     Purpose: geographic distribution of sales (`country` are ISO2 codes).
p1sales.sum <- store.df %>%
  group_by(country) %>%
  summarise(x = sum(p1sales, na.rm = TRUE)) %>%
  mutate(country = toupper(country))

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

# 16) HEATMAP — counts of observations by store × decile of P1 sales
#     Purpose: compare sales mix across stores.
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

# 16b) COHORT RETENTION HEATMAP — the marketing classic
#      Rows: acquisition cohorts; columns: months since signup; fill: % retained.
cohort_df <- read.csv("data/cohort-retention.csv")
ggplot(cohort_df,
       aes(x = months_since,
           y = fct_reorder(cohort, -cohort_month),
           fill = retained_pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = retained_pct), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue", name = "% retained") +
  scale_x_continuous(breaks = 0:11) +
  labs(
    title = "Customer Retention by Acquisition Cohort (simulated)",
    x     = "\nMonths since acquisition",
    y     = NULL
  ) +
  theme_minimal()
ggsave("chart-types/cohort-retention.pdf", width = 6, height = 4)

# GROUPING vs FACETING
# plot sales of P1 by year in one panel...
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

# ...or use facets to separate by year
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

# ANNOTATION EXAMPLE — mark an event on a time series
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

# HOW TO MISLEAD WITH A CHART — truncated vs. full y-axis
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
