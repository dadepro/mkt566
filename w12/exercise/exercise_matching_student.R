# =============================================================
# MKT 566: Marketing Analytics
# Causal inference: Matching Exercise (STUDENT VERSION)
# =============================================================

set.seed(123)

install_if_missing <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
}
install_if_missing(c("MatchIt", "stargazer", "cobalt"))
library(MatchIt)
library(stargazer)
library(cobalt)

# DGP: selection on observables
N <- 5000
beta0 <- 10; b_ps <- 1.2; b_mob <- 3.0; sigma <- 3.5; tau <- 2.0
price_sensitivity <- rnorm(N, 0, 1)
mobile <- rbinom(N, 1, 0.5)

g0 <- -0.2; g_ps <- 1.0; g_mob <- 0.5
linpred <- g0 + g_ps*price_sensitivity + g_mob*mobile
p_coupon <- 1/(1+exp(-linpred))
Coupon <- rbinom(N, 1, p_coupon)

eps <- rnorm(N, 0, sigma)
Sales0 <- beta0 + b_ps*price_sensitivity + b_mob*mobile + eps
Sales1 <- Sales0 + tau
Sales <- ifelse(Coupon==1, Sales1, Sales0)

dat <- data.frame(Sales, Coupon, price_sensitivity, mobile)

cat("Data is ready (outcome is Sales and treatment Coupon). Complete the TODOs below.\n")

cat("\n=== PART 1: Baselines ===\n")
# TODO: OLS (with/without controls)

cat("\n=== PART 2: Propensity score estimation ===\n")
# TODO: use matchit on price_sensitivity and mobile with nearest neighbor matching and caliper=0.01 (see ?matchit for the function helper)


cat("\n=== PART 3: Estimate ATT on matched data ===\n")
# TODO: get matched data & weights; estimate ATT via OLS and adding weights as argument to lm();

# QUESTIONS:
# 1) Did balance improve after matching?
# 2) Is ATT closer to tau=2.0 than the naive estimate?
# 3) Why can matching fail under unobserved confounding?
