
## ---- 0) Setup ------------------------------------------------------------
set.seed(123)

#setwd as script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)

## A tiny helper to keep output clean
hr <- function(title) cat("\n", paste0(rep("=", nchar(title) + 4), collapse=""),
                          "\n= ", title, " =\n",
                          paste0(rep("=", nchar(title) + 4), collapse=""), "\n", sep="")

## ---- 1) Simulate a small, realistic churn dataset ------------------------
# Goal: create features that *intuitively* relate to churn.
n <- 3000

df <- data.frame(
  tenure_months      = pmax(1, round(rnorm(n, mean = 24, sd = 12))),     # longer tenure -> safer
  logins_last30      = pmax(0, round(rnorm(n, mean = 12, sd = 5))),      # fewer logins -> riskier
  tickets_30d        = pmax(0, rpois(n, lambda = 0.6)),                  # more tickets -> riskier
  email_engagement   = pmin(1, pmax(0, rbeta(n, 2, 5))),                 # 0..1, higher -> safer
  price_increase_60d = rbinom(n, 1, 0.18),                               # had price hike -> riskier
  plan_downgrade     = rbinom(n, 1, 0.12),                               # downgraded -> riskier
  region             = factor(sample(c("North","South","West","East"),
                                     n, TRUE, prob = c(.28,.22,.30,.20)))
)

# Create a "true" risk score (log-odds), then simulate churn from it.
# (You can skim the comments to see the intended effects.)
log_odds <- -2.0 +
  0.25 * df$plan_downgrade +
  0.30 * df$price_increase_60d +
  0.35 * (df$tickets_30d >= 2) +
  0.03 * pmax(0, 15 - df$logins_last30) +   # fewer than ~15 logins -> higher risk
  (-0.015) * df$tenure_months +             # longer tenure -> lower risk
  (-0.9)  * df$email_engagement             # higher engagement -> lower risk

# small regional differences
log_odds <- log_odds + c(North=0, South=0.08, West=-0.05, East=0.03)[df$region]

# Convert log-odds to probability and draw churn (0/1)
p <- 1 / (1 + exp(-log_odds))
df$churn <- rbinom(n, 1, p)

hr("Quick data check")
cat("Rows:", nrow(df), "   Churn rate:", round(mean(df$churn), 3), "\n")
head(df, 3)



# save original df for later use
write.csv(df, "data/churn_data.csv", row.names = FALSE)
