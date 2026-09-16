# ============================================================================
# MKT 566, Week 4: Regression (OLS)
# ============================================================================
# This script reproduces every chart and every regression from the OLS
# slides: the fitted line on the marketing data, how least squares chooses
# the line, reading lm() output (coefficients, standard errors, p-values,
# R-squared), logs and elasticities, several predictors at once, and then
# the Airbnb data: price vs. reviews, and a categorical predictor (city).
#
# HOW TO RUN THIS SCRIPT (same workflow as weeks 1 to 3):
#   - Run it from the top, one step at a time: click on a line and press
#     Cmd+Enter (Mac) or Ctrl+Enter (Windows).
#   - A chart is built by one long instruction that spans several lines
#     (the lines glued together by "+"). Cmd+Enter runs the whole
#     instruction at once.
#   - Regression results print in the terminal (the R console), not in a
#     chart tab. Scroll up if a table went by too fast.
#   - Lines starting with # are comments: notes for humans, ignored by R.
#
# You are NOT expected to memorize this code. Read the comment above each
# block, run it, and look at the result. If you want to understand a
# specific line, select it and ask your AI assistant to explain it.
# ============================================================================

#### install libraries ####
# Installs the packages this script needs, skipping any you already have.
pkgs <- c("datarium", "ggplot2", "data.table", "patchwork", "scales",
          "stargazer")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(datarium)   # the "marketing" dataset (ad spend on three channels)
library(ggplot2)    # charts
library(data.table) # fast tables; fread() reads CSVs quickly
library(patchwork)  # place charts side by side with a simple +
library(scales)     # nicer axis labels (dollar, comma)
library(stargazer)  # regression tables you can put in a report

# ---- housekeeping: safe to run without understanding -----------------------
# Point R at the folder that contains this script, so "data" is found and
# the "figures" output folder lands next to it.
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
dir.create("figures", showWarnings = FALSE)
set.seed(566)
options(scipen = 10)    # print 8439.11, not 8.439e+03 (p-values stay as 2e-16)
# ---------------------------------------------------------------------------

# ============================================================================
# PART 1: From a line to a model (the marketing data from week 3)
# ============================================================================
# 200 ad campaigns: spend on youtube, facebook, newspaper and sales.
# The package stores spend in thousands of dollars and sales in thousands
# of units. We multiply everything by 1,000 so the data are in dollars and
# units, and every coefficient reads "per $1".
data("marketing", package = "datarium")
marketing[] <- lapply(marketing, function(v) v * 1000)
head(marketing)

# ---- The line we drew last week --------------------------------------------
# geom_smooth(method = "lm") asks ggplot for the regression line.
ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(labels = comma) +   # 100,000 instead of 1e+05
  scale_y_continuous(labels = comma) +
  labs(title = "Sales vs. YouTube Ad Spend",
       x = "YouTube ad spend ($)",
       y = "Sales (units)") +
  theme_minimal()
ggsave("figures/01-sales-youtube-line.pdf", width = 6, height = 4)

# ---- Estimating the regression: lm() ---------------------------------------
# "sales ~ youtube" reads "sales as a function of youtube".
model <- lm(sales ~ youtube, data = marketing)
summary(model)

# The two numbers that define the line: intercept and slope.
coef(model)
# Reading: with no YouTube spend, predicted sales are about 8,400 units.
# Each extra $1 on YouTube is associated with 0.0475 more units sold.

# Coefficients with their standard errors, t-values, and p-values.
round(coef(summary(model)), 4)

# ---- Which line? Least squares ---------------------------------------------
# OLS picks the line with the smallest sum of squared errors (the vertical
# distances from the points to the line). Compare a made-up line (A) with
# the OLS line (B).
b <- coef(model)
draw_line <- function(a, slope, title) {
  d <- as.data.table(marketing)
  d[, yhat := a + slope * youtube]
  ggplot(d, aes(x = youtube, y = sales)) +
    geom_segment(aes(xend = youtube, yend = yhat), color = "steelblue", alpha = 0.5) +
    geom_point(alpha = 0.4, color = "darkgreen") +
    geom_abline(intercept = a, slope = slope, color = "firebrick", linewidth = 1.1) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    labs(title = title, x = "YouTube ad spend ($)", y = "Sales (units)") +
    theme_minimal(base_size = 13)
}
sse_a <- sum((marketing$sales - (5000 + 0.08 * marketing$youtube))^2)  # line A
sse_b <- sum(resid(model)^2)                                             # OLS line
draw_line(5000, 0.08, paste("Line A\nsum of squared errors =", comma(round(sse_a)))) +
  draw_line(b[1], b[2], paste("Line B (OLS)\nsum of squared errors =", comma(round(sse_b))))
ggsave("figures/02-which-line.pdf", width = 10, height = 4)

# ---- R-squared ---------------------------------------------------------------
# R-squared = 1 - (errors with the line) / (errors with only the mean).
sst <- sum((marketing$sales - mean(marketing$sales))^2)
1 - sse_b / sst          # same number as "Multiple R-squared" in summary()
summary(model)$r.squared

# The picture: errors around the mean vs. errors around the line (40
# campaigns, so the segments are visible).
sub <- as.data.table(marketing)[sample(.N, 40)]
sub[, ybar := mean(marketing$sales)]
sub[, yhat := predict(model, sub)]
p_mean <- ggplot(sub, aes(youtube, sales)) +
  geom_segment(aes(xend = youtube, yend = ybar), color = "steelblue", alpha = 0.6) +
  geom_hline(yintercept = mean(marketing$sales), color = "grey30", linewidth = 1) +
  geom_point(color = "darkgreen") +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
  labs(title = "Errors if we only knew the mean", x = "YouTube spend ($)", y = "Sales (units)") +
  theme_minimal(base_size = 13)
p_line <- ggplot(sub, aes(youtube, sales)) +
  geom_segment(aes(xend = youtube, yend = yhat), color = "steelblue", alpha = 0.6) +
  geom_abline(intercept = b[1], slope = b[2], color = "firebrick", linewidth = 1) +
  geom_point(color = "darkgreen") +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
  labs(title = "Errors with the regression line", x = "YouTube spend ($)", y = NULL) +
  theme_minimal(base_size = 13)
p_mean + p_line
ggsave("figures/03-r-squared.pdf", width = 10, height = 4)

# ============================================================================
# PART 2: Interpreting the coefficient
# ============================================================================

# ---- The same relationship three ways: levels, log-level, log-log ----------
# log() inside the formula transforms the variable on the fly.
m_level  <- lm(sales ~ youtube, data = marketing)
m_loglev <- lm(log(sales) ~ youtube, data = marketing)
m_loglog <- lm(log(sales) ~ log(youtube), data = marketing)

# stargazer() prints several models side by side. type = "text" prints to
# the console (type = "html" makes a table for an R Markdown report).
# no.space = TRUE removes the blank lines between rows. digits.extra adds
# decimals to a coefficient that would otherwise round to 0.0000.
stargazer(m_level, m_loglev, m_loglog, type = "text",
          column.labels = c("level-level", "log-level", "log-log"),
          omit.stat = c("f", "ser", "adj.rsq"), digits = 4, digits.extra = 4,
          no.space = TRUE)
# Reading:
#   level-level: $1 more on YouTube    -> 0.0475 more units
#   log-level:   $1 more on YouTube    -> about 0.0003% more sales (100 * 0.000003)
#   log-log:     1% more YouTube spend -> 0.36% more sales (the elasticity)

# ---- More than one predictor -----------------------------------------------
# Each coefficient is now "holding the other predictors fixed".
m_all <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
stargazer(m_level, m_all, type = "text",
          omit.stat = c("f", "ser", "adj.rsq"), digits = 3, no.space = TRUE)
# Newspaper's coefficient is about zero and not significant: once YouTube
# and Facebook are in the model, newspaper spend adds nothing. Campaigns
# that spend on newspaper also spend on Facebook:
cor(marketing$newspaper, marketing$facebook)

# ============================================================================
# PART 3: The Airbnb data
# ============================================================================
# About 50,000 listings: nightly price, size, star rating, number of
# reviews, city, room type.
airbnb <- fread("data/airbnb.csv")
names(airbnb)
head(airbnb[, .(price, bedrooms, star_rating, reviews_count, city, room_type)])  # a few columns
nrow(airbnb)
table(airbnb$city)      # note: only 18 listings in New York City

# ---- Price vs. number of reviews -------------------------------------------
# Predict first: what sign do you expect for the slope?
ggplot(airbnb, aes(x = reviews_count, y = price)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  scale_y_continuous(labels = dollar) +
  labs(title = "Price vs. Number of Reviews",
       x = "Number of reviews", y = "Nightly price") +
  theme_minimal()
ggsave("figures/04-airbnb-price-reviews.pdf", width = 6, height = 4)

# Both variables are heavily skewed. Compare median and max:
summary(airbnb$price)
summary(airbnb$reviews_count)

# ---- The regression ----------------------------------------------------------
m1 <- lm(price ~ reviews_count, data = airbnb)
summary(m1)
# Reading: each extra review is associated with a price 35 cents lower.
# Three stars (significant, because N is 50,000) but R-squared = 0.002:
# reviews explain 0.2% of the variation in price. Significant, not useful.

# A table for a report.
stargazer(m1, type = "text",
          title = "Regression of Price on Number of Reviews",
          dep.var.labels = "Nightly price (dollars)",
          covariate.labels = "Number of reviews",
          omit.stat = c("f", "ser", "adj.rsq"), digits = 2, no.space = TRUE)

# ============================================================================
# PART 4: Categorical predictors
# ============================================================================

# ---- How R stores text: factors --------------------------------------------
# A factor is a categorical variable stored as numbers with labels.
city <- factor(c("Miami", "Austin", "Miami", "Boston"))
city
as.integer(city)        # Austin = 1, Boston = 2, Miami = 3 (alphabetical)

# ---- Price by city -----------------------------------------------------------
# lm() turns "city" into dummy variables (0/1) and drops one: the base
# level (Austin, first alphabetically). Each coefficient is the difference
# from Austin; the constant is Austin's average price.
m_city <- lm(price ~ city, data = airbnb)
stargazer(m_city, type = "text", title = "Regression of Price on City",
          dep.var.labels = "Nightly price (dollars)",
          omit.stat = c("f", "ser", "adj.rsq"), digits = 2, no.space = TRUE)
# New York is "not significant" only because there are 18 listings: its
# standard error is about $39, versus $2-3 for the other cities.

# ---- Choosing the base level -----------------------------------------------
# relevel() picks which city the others are compared to. Same fit, same
# R-squared, different comparison.
airbnb$city <- relevel(factor(airbnb$city), ref = "Los Angeles")
m_city2 <- lm(price ~ city, data = airbnb)
stargazer(m_city, m_city2, type = "text",
          column.labels = c("base: Austin", "base: Los Angeles"),
          dep.var.labels = "Nightly price (dollars)",
          omit.stat = c("f", "ser", "adj.rsq"), digits = 2, no.space = TRUE)

# ---- Try it yourself before Tuesday ----------------------------------------
# Replace reviews_count with bedrooms. Predict the slope first. Then explain
# back what the intercept means.
# m2 <- lm(price ~ bedrooms, data = airbnb)
# summary(m2)
