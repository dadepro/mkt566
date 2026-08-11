# =============================================================
# MKF 566: Marketing Analytics
# Instructor’s Solutions
# Causality with Observational Data — SOLUTIONS (with Stargazer tables)
# -------------------------------------------------------------
# WHAT THIS FILE CONTAINS
# - Fully worked estimators for each scenario: Welch t-test and OLS.
# - Detailed comments explaining interpretation and common pitfalls.
# - Stargazer tables for clean, readable regression outputs.
# =============================================================

set.seed(123)  # reproducibility

# ------- Package setup -------
install_if_missing <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
}
install_if_missing(c("stargazer"))
library(stargazer)

# Welch t-test (difference in means)
diff_in_means <- function(y, d) t.test(y ~ d)

# ------- Global DGP parameters -------
N     <- 5000
beta0 <- 10
b_ps  <- 1.2
b_mob <- 3.0
sigma <- 3.5
tau   <- 2.0

# Observed pre-treatment covariates
price_sensitivity <- rnorm(N, 0, 1)
mobile            <- rbinom(N, 1, 0.5)

# =============================================================
# SCENARIO 1: RANDOMIZED COUPON (Clean RCT)
# -------------------------------------------------------------
# Expectation: Unbiased naive and OLS estimators; controls improve precision.
# =============================================================
Coupon <- rbinom(N, 1, 0.5)  # random assignment

# Potential & observed outcomes
eps1   <- rnorm(N, 0, sigma)
Sales0 <- beta0 + b_ps*price_sensitivity + b_mob*mobile + eps1
Sales1 <- Sales0 + tau
Sales  <- ifelse(Coupon==1, Sales1, Sales0)

cat("\n=== SCENARIO 1: RCT (SOLUTIONS) ===\n")

# 1) Welch t-test (diff-in-means)
ttest1 <- diff_in_means(Sales, Coupon)
print(ttest1)
diff_value <- ttest1$estimate[2] - ttest1$estimate[1]
cat(sprintf("Difference in means (Coupon=1 minus Coupon=0): %.3f\n", diff_value))

# INTERPRETATION:
# - 'estimate' shows mean difference Sales|Coupon=1 minus Sales|Coupon=0.
# - Expect close to 2.0 and statistically significant with N=5000.

# 2) OLS without and with controls
ols1a <- lm(Sales ~ Coupon)
ols1b <- lm(Sales ~ Coupon + price_sensitivity + mobile)

stargazer(ols1a, ols1b, type = "text",
          title = "Scenario 1: Effect of Coupon on Sales (Randomized)",
          dep.var.labels = "Sales",
          keep.stat = c("n","rsq","adj.rsq","f"),
          covariate.labels = c("Coupon", "Price Sensitivity", "Mobile (1=Yes)"))

cat("Notes:\n- Randomization => unbiased E[beta_Coupon].\n- Adding strong predictors (price_sensitivity, mobile) reduces SE on Coupon.\n")

# Balance checks (should show similar distributions across groups)
print(t.test(price_sensitivity ~ Coupon))
print(t.test(mobile ~ Coupon))


# =============================================================
# SCENARIO 2: SELECTION ON OBSERVABLES (Bias + FIXABLE)
# -------------------------------------------------------------
# Expectation: Naive is biased; OLS controlling for the selection drivers is ~unbiased.
# =============================================================
g0 <- -0.2; g_ps <- 1.0; g_mob <- 0.4
linpred2  <- g0 + g_ps*price_sensitivity + g_mob*mobile
p_coupon2 <- 1/(1+exp(-linpred2))
Coupon2   <- rbinom(N, 1, p_coupon2)

eps2     <- rnorm(N, 0, sigma)
Sales0_2 <- beta0 + b_ps*price_sensitivity + b_mob*mobile + eps2
Sales1_2 <- Sales0_2 + tau
Sales_2  <- ifelse(Coupon2==1, Sales1_2, Sales0_2)

cat("\n=== SCENARIO 2: Selection on Observables (SOLUTIONS) ===\n")

# 1) Naive diff-in-means (biased because Coupon2 depends on covariates)
ttest2 <- diff_in_means(Sales_2, Coupon2)
print(ttest2)
diff_value <- ttest2$estimate[2] - ttest2$estimate[1]
cat(sprintf("Difference in means (Coupon2=1 minus Coupon2=0): %.3f\n", diff_value))

# 2) OLS controlling for observed covariates that drive selection
ols1 <- lm(Sales_2 ~ Coupon2)
ols2 <- lm(Sales_2 ~ Coupon2 + price_sensitivity + mobile)

stargazer(ols1, ols2, type = "text",
          title = "Scenario 2: Adjusted OLS under Selection on Observables",
          dep.var.labels = "Sales",
          keep.stat = c("n","rsq","adj.rsq","f"),
          covariate.labels = c("Coupon", "Price Sensitivity", "Mobile (1=Yes)"))

cat("Notes:\n- Naive estimator mixes treatment effect with baseline differences between groups.\n- Conditional on price_sensitivity and mobile, we compare like-with-like and recover ~tau.\n")

# Balance checks (should show different distributions across groups)
print(t.test(price_sensitivity ~ Coupon2))
print(t.test(mobile ~ Coupon2))


# =============================================================
# SCENARIO 3: UNOBSERVED CONFOUNDING (Bias NOT fixable using only observable controls)
# -------------------------------------------------------------
# Expectation: Even with observed controls, omission of intent_to_buy biases the Coupon coefficient.
# =============================================================
intent_to_buy <- rbinom(N, 1, 0.5)  # latent demand (unobserved in practice)

# Treatment assignment depends on observed variables (price_sensitivity and mobile) and unobserved variable intent_to_buy
g_int <- 1.0
linpred3  <- (-0.2) + 1.0*price_sensitivity + 0.4*mobile + g_int*intent_to_buy
p_coupon3 <- 1/(1+exp(-linpred3))
Coupon3   <- rbinom(N, 1, p_coupon3)

# Outcome also depends on intent_to_buy
b_int <- 3.0
eps3     <- rnorm(N, 0, sigma)
Sales0_3 <- beta0 + b_ps*price_sensitivity + b_mob*mobile + b_int*intent_to_buy + eps3
Sales1_3 <- Sales0_3 + tau
Sales_3  <- ifelse(Coupon3==1, Sales1_3, Sales0_3)

cat("\n=== SCENARIO 3: Unobserved Confounding (SOLUTIONS) ===\n")

# 1) Naive OLS
ols3_naive <- lm(Sales_3 ~ Coupon3)

# 2) OLS with observed covariates only (still omits intent_to_buy)
ols3_adj <- lm(Sales_3 ~ Coupon3 + price_sensitivity + mobile)

stargazer(ols3_naive, ols3_adj, type = "text",
          title = "Scenario 3: OLS w/ and w/o controls (intent_to_buy omitted)",
          dep.var.labels = "Sales",
          keep.stat = c("n","rsq","adj.rsq","f"),
          covariate.labels = c("Coupon", "Price Sensitivity", "Mobile (1=Yes)"))

cat("Notes:\n- Both models are biased because Coupon3 is correlated with the omitted intent_to_buy.")


# If we *could* observe intent_to_buy, bias disappears:
ols3_full <- lm(Sales_3 ~ Coupon3 + price_sensitivity + mobile + intent_to_buy)
stargazer(ols3_full, type = "text",
          title = "Scenario 3 (What-if): Including intent_to_buy",
          dep.var.labels = "Sales",
          keep.stat = c("n","rsq","adj.rsq","f"),
          covariate.labels = c("Coupon", "Price Sensitivity", "Mobile (1=Yes)", "Intent to Buy"))
