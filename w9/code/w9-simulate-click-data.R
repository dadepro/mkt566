# Simulate ad-click data with ~30% CTR and adjustable difficulty of prediction
simulate_ad_clicks <- function(n = 5000, ctr = 0.30,
                               difficulty = c("easy","medium","hard"),
                               seed = 123) {
  set.seed(seed)
  difficulty <- match.arg(difficulty)
  
  # Difficulty controls signal strength and noise level
  pars <- switch(difficulty,
                 "easy"   = list(signal_scale = 1.3, noise_sd = 0.15),
                 "medium" = list(signal_scale = 1.0, noise_sd = 0.35),
                 "hard"   = list(signal_scale = 0.6, noise_sd = 0.60)
  )
  s <- pars$signal_scale; noise_sd <- pars$noise_sd
  
  # Features (simple, realistic)
  device       <- factor(sample(c("mobile","desktop"), n, TRUE, c(0.6, 0.4)))
  ad_position  <- factor(sample(c("top","middle","bottom"), n, TRUE, c(0.5, 0.35, 0.15)))
  user_segment <- factor(sample(c("new","returning"), n, TRUE, c(0.4, 0.6)))
  pages_viewed <- pmax(1, round(rnorm(n, mean = 5, sd = 2)))
  time_on_site <- pmax(5, round(rlnorm(n, meanlog = 4.2, sdlog = 0.5)))  # seconds, skewed
  topic_match  <- rbinom(n, 1, 0.45)                                     # ad matches page/user
  past_ctr_hi  <- rbinom(n, 1, 0.30)                                     # historically high clicker
  
  # Center numeric features so the intercept calibration works well
  pv_c <- pages_viewed - mean(pages_viewed)
  tos_c <- time_on_site - mean(time_on_site)
  
  # Latent linear signal (bigger s => easier; add Gaussian noise)
  linear <- s * (
    0.45 * (device == "mobile") +
      0.95 * (ad_position == "top") +
      0.35 * (ad_position == "middle") +
      0.40 * (user_segment == "returning") +
      0.10 * pv_c +
      0.0007 * tos_c +
      1.20 * topic_match +
      1.50 * past_ctr_hi
  ) + rnorm(n, 0, noise_sd)
  
  # Choose intercept b0 so that mean(plogis(b0 + linear)) ≈ ctr
  f <- function(b0) mean(plogis(b0 + linear)) - ctr
  b0 <- uniroot(f, c(-10, 10))$root
  
  p <- plogis(b0 + linear)
  click <- rbinom(n, 1, p)
  
  data.frame(
    click = click,
    device, ad_position, user_segment,
    pages_viewed, time_on_site, topic_match, past_ctr_hi
  )
}


df   <- simulate_ad_clicks(n = 5000, ctr = 0.30, difficulty = "easy")
#save df to data folder
write.csv(df, "data/ad_click_data.csv", row.names = FALSE)
