# ============================================================================
# MKT 566, Week 4: Regression for Yes/No Outcomes (Logit)
# ============================================================================
# This script reproduces every chart and model from the logit slides: a 0/1
# outcome ("gem" listings), why a straight line breaks for it, the logistic
# curve, a logit on price, the full logit model with several predictors,
# odds ratios, and predicted probabilities.
#
# HOW TO RUN THIS SCRIPT (same workflow as before):
#   - Run it from the top, one step at a time: click on a line and press
#     Cmd+Enter (Mac) or Ctrl+Enter (Windows).
#   - Regression results print in the terminal (the R console). Charts open
#     in a VS Code tab and are also saved as PDFs in the "figures" folder.
#   - Lines starting with # are comments: notes for humans, ignored by R.
#
# You are NOT expected to memorize this code. Read the comment above each
# block, run it, and look at the result. If you want to understand a
# specific line, select it and ask your AI assistant to explain it.
# ============================================================================

#### install libraries ####
pkgs <- c("ggplot2", "data.table", "scales", "stargazer")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(ggplot2)    # charts
library(data.table) # fast tables; fread() reads CSVs quickly
library(scales)     # percent() and comma() for axis labels
library(stargazer)  # regression tables you can put in a report

# ---- housekeeping: safe to run without understanding -----------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
dir.create("figures", showWarnings = FALSE)
set.seed(566)
# ---------------------------------------------------------------------------

# ============================================================================
# PART 1: A yes/no outcome
# ============================================================================
airbnb <- fread("data/airbnb.csv")

# A "gem" is a listing rated 4.5 or more with more than 20 reviews.
# The & means both conditions must hold; as.integer() turns TRUE/FALSE into 1/0.
airbnb[, gem := as.integer(star_rating >= 4.5 & reviews_count > 20)]
table(airbnb$gem)
mean(airbnb$gem)        # the share of gems: about 20%

# ---- Share of gems by price bin --------------------------------------------
# cut() puts each price into a bin; the mean of a 0/1 variable is a share.
airbnb[, pbin := cut(price, c(0, 50, 100, 150, 200, 300, 500, 1000, Inf),
                     labels = c("0-50", "50-100", "100-150", "150-200", "200-300",
                                "300-500", "500-1000", "1000+"))]
bins <- airbnb[, .(n = .N, share = mean(gem)), by = pbin][order(pbin)]
bins
ggplot(bins, aes(x = pbin, y = share)) +
  geom_col(fill = "#990000", alpha = 0.85) +
  geom_text(aes(label = percent(share, accuracy = 1)), vjust = -0.4) +
  scale_y_continuous(labels = percent, limits = c(0, 0.27)) +
  labs(title = "Share of Gems by Nightly Price", x = "Nightly price ($)", y = "Share of gems") +
  theme_minimal(base_size = 14)
ggsave("figures/01-gem-share-by-price.pdf", width = 8, height = 4.2)

# ---- The straight line: OLS on a 0/1 outcome (linear probability model) ----
lpm <- lm(gem ~ price, data = airbnb)
round(coef(lpm), 6)
# Reading: intercept 0.217 = a 21.7% chance at price zero; each extra dollar
# lowers the predicted probability by 0.009 percentage points.

# The chart: gems (1) and non-gems (0) against price, with the OLS line.
# geom_jitter() spreads the 0s and 1s vertically a little so they are visible.
sub <- airbnb[price <= 3000]
ggplot(sub, aes(x = price, y = gem)) +
  geom_jitter(height = 0.04, width = 0, alpha = 0.08, color = "darkgreen") +
  geom_abline(intercept = coef(lpm)[1], slope = coef(lpm)[2], color = "firebrick", linewidth = 1.1) +
  geom_hline(yintercept = c(0, 1), linetype = 3, color = "grey40") +
  labs(title = "Gem (0/1) vs. Price, with the OLS line", x = "Nightly price ($)", y = "Gem") +
  theme_minimal(base_size = 13)
ggsave("figures/02-lpm-line.pdf", width = 6, height = 4)

# Where the line breaks: predicted "probabilities" below zero.
pred <- predict(lpm)
sum(pred < 0)                        # listings predicted below 0
round(min(pred), 2)                  # the lowest prediction
-coef(lpm)[1] / coef(lpm)[2]         # the price at which the line crosses zero

# ============================================================================
# PART 2: Logistic regression
# ============================================================================

# ---- The logistic (S) curve ------------------------------------------------
# p = 1 / (1 + exp(-z)) turns any number z into a probability between 0 and 1.
zz <- data.table(z = seq(-6, 6, by = 0.05))
zz[, p := 1 / (1 + exp(-z))]
ggplot(zz, aes(z, p)) +
  geom_hline(yintercept = c(0, 1), linetype = 3, color = "grey40") +
  geom_line(color = "#990000", linewidth = 1.3) +
  labs(title = "The logistic (S) curve", x = "z = b0 + b1 * X", y = "p") +
  theme_minimal(base_size = 14)
ggsave("figures/03-logistic-curve.pdf", width = 6, height = 4)

# ---- Logit on price ----------------------------------------------------------
# glm() works like lm(); family = binomial asks for a logit.
lg1 <- glm(gem ~ price, data = airbnb, family = binomial)
round(coef(lg1), 5)
# Reading: each dollar multiplies the odds of being a gem by exp(-0.00081) = 0.9992.
exp(coef(lg1)["price"])              # odds ratio per dollar
exp(100 * coef(lg1)["price"])        # odds ratio per $100: about 0.92, an 8% drop

# Predicted probabilities at a few prices: the reading a manager understands.
grid_prices <- data.frame(price = c(50, 100, 200, 500, 1000, 5000))
grid_prices$p_gem <- round(predict(lg1, grid_prices, type = "response"), 3)
grid_prices

# The logit curve vs. the OLS line, with the observed share in each bin.
grid <- data.table(price = seq(0, 3000, by = 10))
grid[, logit := predict(lg1, grid, type = "response")]
grid[, line := predict(lpm, grid)]
mids <- data.table(price = c(25, 75, 125, 175, 250, 400, 750, 1500),
                   share = bins$share, n = bins$n)
ggplot() +
  geom_hline(yintercept = 0, linetype = 3, color = "grey40") +
  geom_line(data = grid, aes(price, line, color = "OLS line"), linewidth = 1) +
  geom_line(data = grid, aes(price, logit, color = "Logit curve"), linewidth = 1.3) +
  geom_point(data = mids, aes(price, share, size = n), color = "darkgreen", alpha = 0.8) +
  scale_color_manual(values = c("OLS line" = "firebrick", "Logit curve" = "#0072B2"), name = NULL) +
  scale_size_area(max_size = 9, guide = "none") +
  scale_y_continuous(labels = percent) +
  labs(title = "Probability of Being a Gem vs. Price", x = "Nightly price ($)", y = "P(gem)") +
  theme_minimal(base_size = 14) + theme(legend.position = "top")
ggsave("figures/04-logit-vs-line.pdf", width = 9, height = 4.3)

# ---- Fit: log-likelihood and pseudo-R-squared ------------------------------
logLik(lg1)                                   # closer to zero is better
1 - lg1$deviance / lg1$null.deviance          # McFadden pseudo-R2 (tiny here)

# ============================================================================
# PART 3: The full logit model
# ============================================================================
m_logit <- glm(gem ~ price + guests_included + city + room_type,
               data = airbnb, family = binomial)
stargazer(m_logit, type = "text", omit.stat = c("f", "ser", "aic", "bic"),
          digits = 4, no.space = TRUE, single.row = TRUE)   # SE next to each coefficient

# Odds ratios: exp() of each coefficient. Below 1 lowers the odds, above 1 raises them.
round(exp(coef(m_logit)), 3)
# Reading: $100 more -> odds x 0.84 (16% lower); one more guest -> x 1.12;
# Boston or LA vs. Austin -> x 1.18; Miami -> x 0.82; shared room -> x 0.49.

# Predicted probabilities for listings you describe yourself.
examples <- data.frame(price = c(100, 100, 300, 100), guests_included = 2,
                       city = c("Austin", "Boston", "Austin", "Austin"),
                       room_type = c("Entire home", "Entire home", "Entire home", "Shared room"))
examples$p_gem <- round(predict(m_logit, examples, type = "response"), 3)
examples

# Fit of the full model vs. the price-only model.
logLik(m_logit)
1 - m_logit$deviance / m_logit$null.deviance

# The accuracy trap: predicting "not a gem" for everyone is right 80% of the time.
p_hat <- predict(m_logit, type = "response")
mean((p_hat > 0.5) == airbnb$gem)   # accuracy of the model at a 0.5 cutoff
1 - mean(airbnb$gem)                 # accuracy of always saying "no"
# Week 9 (classifiers) is about doing better than this.
