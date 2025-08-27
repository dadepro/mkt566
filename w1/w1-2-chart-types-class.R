#### install libraries ####
pkgs <- c(
  "ggplot2","dplyr","forcats","treemap","tidyr",
  "rworldmap","RColorBrewer","data.table","treemapify",
  "scales","sf","rnaturalearth"
)

install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(treemap)
library(tidyr)  
library(rworldmap)    
library(RColorBrewer) 
library(data.table)
library(treemapify)
library(scales)
#maps
library(sf)
library(rnaturalearth)

#set directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# -----------------------------
# Data for skills visualizations
# -----------------------------
df_skills <- data.frame(
  skill = c("SQL", "Python", "R", "SAS", "Matlab", "SPSS", "Stata"),
  count = c(107130, 66976, 48772, 25644, 11464, 3717, 1624)
)

# 1) BAR PLOT — absolute frequency of postings by skill
#    X: skill; Y: count; purpose: quick comparison across categories.
ggplot(df_skills, aes(x = skill, y = count)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "U.S. Job Postings by Skill",
    x     = NULL,
    y     = "Number of Postings"
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
#    Left Y: count; Right Y: cumulative %; purpose: identify “vital few” skills
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
  arrange(desc(count)) %>%             # order from largest to smallest
  mutate(
    cum    = cumsum(count),            # running total
    prev   = lag(cum, default = 0),    # previous total
    delta  = count,                    # change
    type   = ifelse(delta >= 0, "gain", "loss"),
    skill  = factor(skill, levels = skill)  # lock in order
  )

# numeric index for positioning
df_wf$idx <- as.numeric(df_wf$skill)

ggplot(df_wf, aes(x = idx)) +
  geom_rect(aes(
    xmin = idx - 0.4,
    xmax = idx + 0.4,
    ymin = prev,
    ymax = cum,
    fill = type
  )) +
  geom_text(aes(
    y = cum + max(cum) * 0.02,
    label = comma(count)       # pretty numbers
  ), size = 3) +
  scale_fill_manual(values = c(gain = "forestgreen", loss = "firebrick")) +
  # relabel numeric x ticks with the factor levels (skill names)
  scale_x_continuous(
    breaks = df_wf$idx,
    labels = levels(df_wf$skill)
  ) +
  # no scientific notation; add a little headroom for labels
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
# Retail data: weekly aggregations and plots
# -----------------------------------------
store.df <- read.csv("http://goo.gl/QPDdMl")
head(store.df)
weekly <- store.df %>%
  group_by(Week) %>%
  summarise(
    TotalP1 = sum(p1sales, na.rm = TRUE),  # total sales of product 1 per week
    AvgP1   = mean(p1sales, na.rm = TRUE), # average sale of product 1 per week
    AvgP1price = mean(p1price, na.rm = TRUE),  # average price of product 1
    TotalP2 = sum(p2sales, na.rm = TRUE),  # total sales of product 2 per week
    AvgP2   = mean(p2sales, na.rm = TRUE), # average sale of product 1 per week
    numStores = n_distinct(storeNum),  # number of stores with sales
    numObs = n()  # number of observations (rows) per week
  ) %>%
  arrange(Week)

# 6) LINE CHART — weekly total sales of P1 over time
#    X: week; Y: total P1 sales; purpose: trend/seasonality.
ggplot(weekly, aes(x = Week, y = TotalP1)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  labs(
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
#    Left Y: AvgP1; Right Y: cumulative P1; purpose: compare central tendency vs. total progress.
weekly <- weekly %>%
  mutate(CumTotalP1 = cumsum(TotalP1))

# scale factor to put CumTotalP1 on the AvgP1 scale
ratio <- max(weekly$AvgP1, na.rm = TRUE) / max(weekly$CumTotalP1, na.rm = TRUE)

ggplot(weekly, aes(x = Week)) +
  geom_bar(aes(y = AvgP1), stat = "identity", fill = "steelblue") +
  geom_line(aes(y = CumTotalP1 * ratio), color = "firebrick", size = 0.5) +
  geom_point(aes(y = CumTotalP1 * ratio), color = "firebrick", size = 0.5) +
  scale_x_continuous(breaks = seq(1, 52, by = 4), "\nWeek") +
  scale_y_continuous(
    name = "Avg P1 Sale\n",
    labels = comma,  # left axis
    sec.axis = sec_axis(
      ~ . / ratio,
      name   = "Cumulative P1 Sales\n",
      labels = comma,                 # <- right axis not exponential
      breaks = pretty_breaks(n = 5)   # optional: nicer tick positions
    )
  ) +
  labs(title = "Avg vs. Cumulative P1 Sales by Week") +
  theme_minimal()
ggsave("chart-types/bar-line-combo.pdf", width = 5, height = 3.5)

# 9) HISTOGRAM — distribution of P1 sales across observations
#    Purpose: shape/spread of p1sales (skewness, outliers).
ggplot(store.df, aes(x = p1sales)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
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
  mutate(
    PromoFlag = factor(ifelse(p1prom > 0, "Promoted", "Not Promoted"))
  )

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

# 13) SCATTER — relationship between P1 and P2 sales
#     X: p1sales; Y: p2sales; purpose: visual correlation/association between product sales.
ggplot(store.df, aes(x = p1sales, y = p2sales)) +
  geom_point(alpha = 0.6) +
  labs(
    x     = "Sales of P1",
    y     = "Sales of P2"
  ) +
  theme_bw()
ggsave("chart-types/scatter-plot.pdf", width = 5, height = 3.5)

# 14) Bubble chart: Weekly total p1 sales with size as avg price
      # purpose: verify whether more sales happens for lower prices.
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


# 15) CHOROPLETH MAP — total P1 sales by country (requires ISO2 codes in `country`)
#     Purpose: geographic distribution of sales (be sure `country` are ISO2 codes; otherwise map join fails).
p1sales.sum <- aggregate(store.df$p1sales,
                         by = list(country = store.df$country), sum)
p1sales.map <- joinCountryData2Map(p1sales.sum,
                                   joinCode = "ISO2",
                                   nameJoinColumn = "country")
mapCountryData(p1sales.map, nameColumnToPlot = "x",
               mapTitle = "Total P1 sales by Country",
               colourPalette = brewer.pal(7, "Greens"),
               catMethod = "fixedWidth", addLegend = TRUE)

# 1) aggregate sales by ISO-2 country code
p1sales.sum <- store.df %>%
  group_by(country) %>%                 # e.g., "US", "FR", "DE"
  summarise(x = sum(p1sales, na.rm = TRUE)) %>%
  mutate(country = toupper(country))    # just in case

# 2) world polygons with ISO-2 codes
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a2, name, geometry)

# 3) join data → map
world_join <- world %>%
  left_join(p1sales.sum, by = c("iso_a2" = "country"))

# 4) plot
ggplot(world_join) +
  geom_sf(aes(fill = x), color = NA) +
  scale_fill_gradientn(
    colours = brewer.pal(7, "Greens"),
    na.value = "grey90",
    name = "P1 sales"
  ) +
  labs(title = "Total P1 sales by Country\n") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "right", 
        plot.margin = margin(5.5, 30, 5.5, 5.5),   # more right margin
        legend.title = element_text(size = 9),
        legend.text  = element_text(size = 8)
  )
ggsave("chart-types/choropleth-map.pdf", width = 6, height = 4)

# 16) HEATMAP — counts of observations by store × decile of P1 sales
#     X: p1sales decile; Y: storeNum; fill: count; purpose: compare sales mix across stores.
store.dt <- as.data.table(store.df)
store.dt[, p1sales_decile := cut(
  p1sales,
  breaks = quantile(p1sales, probs = seq(0, 1, 0.1), na.rm = TRUE),
  include.lowest = TRUE
)]
heatmap_data <- store.dt[, .(count = .N),
                         by = .(storeNum, p1sales_decile)]
heatmap_data_wide <- dcast(heatmap_data, storeNum ~ p1sales_decile,
                           value.var = "count", fill = 0)

ggplot(melt(heatmap_data_wide, id.vars = "storeNum"),
       aes(x = variable, y = storeNum, fill = value)) +
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


# FACET EXAMPLE (Slide 37)
# plot sales of P1 by year
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
ggsave("chart-types/groupinge-example.pdf", width = 5, height = 3.5)

# use facet to separate by year
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
