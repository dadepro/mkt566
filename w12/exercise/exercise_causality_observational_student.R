# =============================================================
# MKT 566: Marketing Analytics
# Causality — Classroom Exercises
# -------------------------------------------------------------
# READ ME
# - This script *generates synthetic data* for three causal scenarios using realistic variables
# - Your job: run the requested t-tests and regressions, then answer the questions.
# - Every block is heavily commented to explain WHY we do each step.
# - Keep set.seed() so your results match your classmates’ within small randomness.
# -------------------------------------------------------------
# Variables (realistic marketing context)
#   Sales            : numeric outcome (e.g., daily revenue per user)
#   Coupon           : treatment assignment (0/1 offer exposure)
#   price_sensitivity: observed covariate (higher => more sensitive to price)
#   mobile           : observed covariate (1 if mobile device)
#   intent_to_buy    : latent demand (only in Scenario 3; NOT observed by analyst)
# =============================================================
library(stargazer)

set.seed(123)  # Reproducibility: same random draw each run

# -------------------------------------------------------------
# Helper: Welch t-test for difference in means
#   Why Welch? It's robust to unequal variances between groups.
#   We will use this to estimate the "naive" treatment effect
#   as E[Sales|Coupon=1] - E[Sales|Coupon=0].
# -------------------------------------------------------------
diff_in_means <- function(outcome, treatment) t.test(outcome ~ treatment)
# Example of use:
x <- rnorm(100)
d <- rbinom(100, 1, 0.5)
ttest = diff_in_means(x, d)
# print t-test results
print(ttest)
# Access difference in means:
diff_value_example <- ttest$estimate[2] - ttest$estimate[1]
cat(sprintf("Example diff in means (d=1 minus d=0): %.3f\n", diff_value_example))

# -------------------------------------------------------------
# Global parameters for data generation
#   - These govern the *true* DGP (data generating process)
#   - tau is the TRUE causal effect of Coupon on Sales we try to recover
# -------------------------------------------------------------
N     <- 5000   # sample size
beta0 <- 10     # baseline sales intercept
b_ps  <- 1.2    # effect of price_sensitivity on baseline Sales
b_mob <- 3.0    # effect of mobile on baseline Sales
sigma <- 3.5    # noise SD in Sales (idiosyncratic shocks)
tau   <- 2.0    # TRUE treatment effect of Coupon on Sales

# Observed, pre-treatment covariates
price_sensitivity <- rnorm(N, 0, 1)   # continuous covariate
mobile            <- rbinom(N, 1, 0.5)  # 50/50 mobile vs desktop

# =============================================================
# SCENARIO 1: RANDOMIZED COUPON (Clean RCT)
# -------------------------------------------------------------
# KEY IDEA:
# - Coupon is assigned at random (independent of covariates).
# =============================================================

# 1) Randomize treatment
Coupon <- rbinom(N, 1, 0.5)  # Bernoulli(0.5) assignment

# 2) Generate potential outcomes
#    Sales0: outcome if Coupon=0 (control world)
#    Sales1: outcome if Coupon=1 (treated world) = Sales0 + tau
eps1   <- rnorm(N, 0, sigma)
Sales0 <- beta0 + b_ps*price_sensitivity + b_mob*mobile + eps1
Sales1 <- Sales0 + tau

# 3) Reveal observed outcome: we observe Y=Sales depending on actual Coupon
Sales  <- ifelse(Coupon==1, Sales1, Sales0)

cat("\n================ SCENARIO 1: RCT ================\n")
cat("Data generated (outcome is Sales and treatment Coupon). Complete the TODOs below.\n")

# TODO 1A: Compare means of price_sensitivity and mobile across Coupon groups using t-test

# TODO 1B: Estimate t-test of Sales by Coupon

# TODO 1C: Estimate OLS without and with observable controls (price_sensitivity, mobile)

# QUESTIONS FOR STUDENTS:
# Q1: Are price_sensitivity and mobile balanced (i.e, have the same means) across Coupon groups? Why?
# Q2: Are estimates with t-test and OLS close to the true treatment effect of 2.0? 
# Q3: Which OLS (with vs without controls) has lower SE? Why?

# =============================================================
# SCENARIO 2: SELECTION ON OBSERVABLES (Bias + FIXABLE)
# -------------------------------------------------------------
# KEY IDEA:
# - Coupon assignment is NOT randomized; more price-sensitive and mobile users
#   are more likely to receive Coupon (targeting).
# =============================================================

# 1) Make assignment depend on observed X's via a logistic model
g0 <- -0.2; g_ps <- 1.0; g_mob <- 0.4
linpred2  <- g0 + g_ps*price_sensitivity + g_mob*mobile       # higher values -> higher p(Coupon=1)
p_coupon2 <- 1/(1+exp(-linpred2))
Coupon2   <- rbinom(N, 1, p_coupon2)

# 2) Generate outcomes with the same tau
eps2     <- rnorm(N, 0, sigma)
Sales0_2 <- beta0 + b_ps*price_sensitivity + b_mob*mobile + eps2
Sales1_2 <- Sales0_2 + tau
Sales_2  <- ifelse(Coupon2==1, Sales1_2, Sales0_2)

cat("\n================ SCENARIO 2: Selection on Observables ================\n")
cat("Data generated (outcome is Sales2 and treatment Coupon2). Complete the TODOs below.\n")

# TODO 2A: Compare means of price_sensitivity and mobile across Coupon2 groups using t-test

# TODO 2B: Estimate naive t-test of Sales2 by Coupon2

# TODO 2C: Estimate OLS with and without observable controls (price_sensitivity, mobile)

# QUESTIONS FOR STUDENTS:
# Q1: Are price_sensitivity and mobile balanced across Coupon2 groups? Why?
# Q2: Does the t-test show bias relative to 2.0? Why?
# Q3: Which OLS estimate recovers the true treatment effect of 2.0: Why?


# =============================================================
# SCENARIO 3: UNOBSERVED CONFOUNDING (Bias NOT fixable)
# -------------------------------------------------------------
# KEY IDEA:
# - Coupon assignment depends on *unobserved* intent_to_buy (latent demand)
#   in addition to observed covariates (price_sensitivity and mobile).
# - intent_to_buy also directly increases Sales.
# - This illustrates the limits of regression adjustment without the right data/design.
# =============================================================

# 1) Simulate latent demand (not observed by analyst)
intent_to_buy <- rbinom(N, 1, 0.5)

# 2) Treatment assignment depends on price_sensitivity, mobile, and intent_to_buy
g_int <- 1.0  # strength of intent_to_buy on assignment
linpred3  <- (-0.2) + 1.0*price_sensitivity + 0.4*mobile + g_int*intent_to_buy
p_coupon3 <- 1/(1+exp(-linpred3))
Coupon3   <- rbinom(N, 1, p_coupon3)

# 3) Outcome depends on price_sensitivity, mobile, AND intent_to_buy
b_int <- 3.0  # strength of intent_to_buy on Sales0
eps3     <- rnorm(N, 0, sigma)
Sales0_3 <- beta0 + b_ps*price_sensitivity + b_mob*mobile + b_int*intent_to_buy + eps3
Sales1_3 <- Sales0_3 + tau
Sales_3  <- ifelse(Coupon3==1, Sales1_3, Sales0_3)

cat("\n================ SCENARIO 3: Unobserved Confounding ================\n")
cat("Data generated (outcome is Sales3 and treatment Coupon3). Complete the TODOs below.\n")

# TODO 3A: Estimate OLS without controls

# TODO 3B: Estimate OLS with observable controls (price_sensitivity, mobile)

# QUESTIONS FOR STUDENTS:
# Q1: Do either OLS recover the true effect of 2.0? Why or why not?
# Q2: Which variable would we need to add to fix the bias?
# Q3: If we could measure intent_to_buy and add it, what would happen to the bias?
# Q4: Estimate OLS including intent_to_buy to verify your answer to Q3.
