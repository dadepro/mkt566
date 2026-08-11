library(ggplot2)
library(broom)
library(data.table)
#setwd as the directory where this file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

set.seed(123)
# 1. Simulate data
n <- 100
df <- data.frame(
  x = runif(n, 0, 10)
)
df$y <- 1.5 + 0.8 * df$x + rnorm(n, sd = 2)

# 2. Fit OLS
mod <- lm(y ~ x, data = df)
coef_mod <- coef(mod)            # intercept and slope
df <- df %>% mutate(
  yhat = predict(mod),           # fitted values
  resid = y - yhat               # residuals
)

# 3. Pick 3 points to illustrate residuals
set.seed(1)
pts <- df[sample(n, 3), ]

# 4. Plot
ggplot(df, aes(x, y)) +
  # data points
  geom_point(color = "steelblue", size = 2) +
  # fitted line
  geom_abline(
    intercept = coef_mod[1],
    slope     = coef_mod[2],
    color     = "firebrick",
    size      = 1
  ) +
  labs(
    title = "Illustration of an OLS Fit",
    x = "Predictor X",
    y = "Outcome Y"
  ) +
  theme_minimal(base_size = 14)
#save
ggsave("w3-1-linear-reg-ols-fit.pdf", width = 6, height = 4)



# logistic
# 1. Load data and fit a logistic model predicting transmission (am: 0 = auto, 1 = manual)
data(mtcars)
mtcars$am <- factor(mtcars$am, labels = c("Automatic","Manual"))

mod <- glm(am ~ wt, 
           data   = mtcars, 
           family = binomial)

summary(mod)

mtcars2 <- mtcars %>%
  mutate(high_hp = ifelse(hp > 150, 1, 0))

# 2. Fit the logistic model
mod_hp <- glm(high_hp ~ wt, 
              data   = mtcars2,
              family = binomial)
summary(mod_hp)
