# =============================================================
# Matching Exercise (SOLUTIONS)
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

cat("\n=== PART 1: Baselines ===\n")
print(t.test(Sales ~ Coupon))
ols_naive <- lm(Sales ~ Coupon, data = dat)
ols_adj   <- lm(Sales ~ Coupon + price_sensitivity + mobile, data = dat)
stargazer(ols_naive, ols_adj, type = "text",
          title = "Naive vs Adjusted OLS",
          dep.var.labels = "Sales",
          covariate.labels = c("Coupon", "Price Sensitivity", "Mobile (1=Yes)"),
          keep.stat = c("n","rsq","adj.rsq","f"))

cat("\n=== PART 2: Propensity score estimation & matching ===\n")

m.out <- matchit(Coupon ~ price_sensitivity + mobile, data = dat,
                 method = "nearest", distance = "logit", caliper = 0.01)

print(summary(m.out))
love.plot(m.out, binary = "std", threshold = 0.1, var.order = "adjusted")

cat("\n=== PART 3: ATT on matched data ===\n")
d.m <- match.data(m.out)
att_lm <- lm(Sales ~ Coupon, data = d.m, weights = d.m$weights)
print(summary(att_lm))

att_dim <- with(d.m, weighted.mean(Sales[Coupon==1], weights[Coupon==1]) -
                     weighted.mean(Sales[Coupon==0], weights[Coupon==0]))
cat(sprintf("Weighted diff-in-means ATT: %.3f\n", att_dim))
cat("True tau = 2.0\n")

cat("\nTakeaways: Matching improves balance and moves the estimate toward the true causal effect under selection-on-observables.\n")
