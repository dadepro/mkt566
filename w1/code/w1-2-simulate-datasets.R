# Simulate the small marketing datasets used in the w1-2 data-viz slides.
# Run once; writes CSVs into data/.
# (data/store-sales.csv is real data from Chapman & Feit and is not created here.)
#
# STUDENTS: you do NOT need to run this script. The files it creates are
# already in the data folder. It is included so you can see exactly how the
# simulated datasets were made, and so you can regenerate them if you ever
# delete one by accident.

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

# create the data output folder if it does not exist
dir.create("data", showWarnings = FALSE)

# set.seed() pins down the random numbers, so everyone who runs this script
# gets exactly the same "random" data.
set.seed(566)

# 1) U.S. job postings by coding skill (approximate counts)
skills <- data.frame(
  skill = c("SQL", "Python", "R", "SAS", "Matlab", "SPSS", "Stata"),
  count = c(107130, 66976, 48772, 25644, 11464, 3717, 1624)
)
write.csv(skills, "data/skills-postings.csv", row.names = FALSE)

# 2) Customer revenue: lognormal, heavily concentrated (80/20 rule)
customers <- data.frame(
  customer_id = 1:500,
  revenue = round(rlnorm(500, meanlog = 6, sdlog = 1.9), 2)
)
write.csv(customers, "data/customer-revenue.csv", row.names = FALSE)

# 3) Online review ratings: J-shaped (extremes over-represented)
ratings <- data.frame(
  stars = sample(1:5, 3000, replace = TRUE, prob = c(.20, .05, .06, .14, .55))
)
write.csv(ratings, "data/online-ratings.csv", row.names = FALSE)

# 4) Weekly ad spend and incremental sales: concave response (diminishing returns)
spend <- runif(200, 0, 100)
sales <- 30 * sqrt(spend) + rnorm(200, 0, 25)
ad <- data.frame(spend = round(spend, 1), sales = round(sales, 1))
write.csv(ad, "data/ad-spend.csv", row.names = FALSE)

# 5) Cohort retention: monthly acquisition cohorts, exponential decay
decay <- runif(12, 3, 7)
cohort <- do.call(rbind, lapply(1:12, function(c) {
  k <- 0:(12 - c)
  data.frame(
    cohort_month = c,
    cohort       = paste(month.abb[c], "2025"),
    months_since = k,
    retained_pct = pmax(0, pmin(100, round(100 * exp(-k / decay[c]) +
                                             rnorm(length(k), 0, 2))))
  )
}))
write.csv(cohort, "data/cohort-retention.csv", row.names = FALSE)
