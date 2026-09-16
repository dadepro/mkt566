# ============================================================================
# MKT 566, Week 4: Regression (OLS), part 2: the Airbnb data
# ============================================================================
# This script reproduces the second half of the OLS slides: the Airbnb
# listings data (about 50,000 listings in four cities), price vs. number of
# reviews, and city as a categorical predictor (factors, dummy variables,
# the base level). The marketing-data half is in w4-1-ols-class.R.
#
# HOW TO RUN THIS SCRIPT (same workflow as weeks 1 to 3):
#   - Run it from the top, one step at a time: click on a line and press
#     Cmd+Enter (Mac) or Ctrl+Enter (Windows).
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
pkgs <- c("ggplot2", "data.table", "scales", "stargazer")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(ggplot2)    # charts
library(data.table) # fast tables; fread() reads CSVs quickly
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
# ---------------------------------------------------------------------------

# ============================================================================
# PART 1: The Airbnb data
# ============================================================================
# About 50,000 listings: nightly price, size, star rating, number of
# reviews, city, room type.
airbnb <- fread("data/airbnb.csv")
names(airbnb)
head(airbnb[, .(price, bedrooms, star_rating, reviews_count, city, room_type)])  # a few columns
nrow(airbnb)
table(airbnb$city)

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
# PART 2: Categorical predictors
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
# All three differences are precisely estimated: each city has thousands
# of listings, so the standard errors are $2-3.

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
