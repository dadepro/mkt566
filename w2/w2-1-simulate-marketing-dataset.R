# ---- Synthetic marketing dataset for EDA (≈1000 rows) ----
set.seed(42)
n <- 1000

CustomerID <- 1:n
Age        <- sample(18:65, n, replace = TRUE)
Gender     <- sample(c("F","M"), n, replace = TRUE, prob = c(0.52, 0.48))
Device     <- sample(c("Mobile","Desktop"), n, replace = TRUE, prob = c(0.65, 0.35))
Channel    <- sample(c("Search","Social","Display","Email","Video"),
                     n, replace = TRUE, prob = c(0.35, 0.25, 0.20, 0.05, 0.15))

# Skewed ad spend (Gamma) + inject a few high-spend outliers
Ad_Spend <- rgamma(n, shape = 2, scale = 100)  # mean ~200, right-skewed
out_idx  <- sample(1:n, 12)                    # a dozen outliers
Ad_Spend[out_idx] <- Ad_Spend[out_idx] * runif(length(out_idx), 3, 6)
Ad_Spend <- round(Ad_Spend, 2)

# Clicks driven by spend, channel, and device (Poisson)
channel_click_mult <- c(Search = 1.2, Social = 1.1, Display = 0.9, Email = 0.8, Video = 1.0)
device_click_mult  <- c(Mobile = 1.15, Desktop = 0.9)
lambda_clicks <- pmax(1, (Ad_Spend / 10) * channel_click_mult[Channel] * device_click_mult[Device])
Clicks <- rpois(n, lambda = lambda_clicks)

# Purchases depend on clicks, spend, and channel (Poisson)
channel_conv_boost <- c(Search = 0.45, Social = 0.30, Display = 0.25, Email = 0.05, Video = 0.20)
lambda_purch <- pmax(0.01, 0.02 * Clicks + 0.001 * Ad_Spend + channel_conv_boost[Channel])
Purchases <- rpois(n, lambda = lambda_purch)

# Optional: revenue per purchase with lognormal noise
Rev_per_Purchase <- round(rlnorm(n, meanlog = log(35), sdlog = 0.5), 2)
Revenue <- round(Purchases * Rev_per_Purchase, 2)

df <- data.frame(
  CustomerID, Age, Gender, Device, Channel,
  Ad_Spend, Clicks, Purchases, Revenue
)

# Save to CSV
write.csv(df, "data/marketing_eda.csv", row.names = FALSE)


################### Let' analyze the dataset by exploring variation

