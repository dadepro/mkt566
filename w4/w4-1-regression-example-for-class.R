library(ggplot2)
library(data.table)
library(scales)
library(stargazer)
library(rstudioapi)

#setwd as the directory where this file is saved
setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/airbnb-case"))


airbnb = fread("w4-airbnb-case.csv.gz")
head(airbnb)

#scatter plot of price vs review_count (looks like price = exp(-\betaX))
ggplot(data = airbnb) +
  geom_point(mapping = aes(x = reviews_count, y = price), alpha = 0.5) +
  labs(title = "Price vs Number of Reviews", x = "\nNumber of Reviews", y = "Price\n") +
  scale_x_continuous(breaks = seq(0, max(airbnb$reviews_count, na.rm = TRUE), 100)) +
  scale_y_continuous(labels = dollar_format()) +
  # geom_smooth(mapping = aes(x = reviews_count, y = price), method = "lm", se = FALSE, color = "blue") +
  #increase font size
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  theme_minimal()
ggsave("w4-airbnb-price-reviews-count.pdf", width = 6, height = 4)

# regress price on number of reviews
m1 = lm(price ~ reviews_count, data = airbnb)
summary(m1)
stargazer(m1, type = "text", title = "Regression of Price on Number of Reviews", 
          dep.var.labels = "Price", covariate.labels = "Number of Reviews", 
          omit.stat = c("f", "ser", "adj.rsq"), digits = 2) 

# Categorical variable: city
# Regress price against city (categorical)
m1 = lm(price ~ city, data = airbnb)
stargazer(m1, type = "text", title = "Regression of Price on City", 
          dep.var.labels = "Price", 
          omit.stat = c("f", "ser", "adj.rsq"), digits = 2)

# relevel city to make New York the base category
# Convert to factor
airbnb$city <- as.factor(airbnb$city)
airbnb$city = relevel(airbnb$city, ref = "New York City")
m1 = lm(price ~ city, data = airbnb)
stargazer(m1, type = "text", title = "Regression of Price on City", 
          dep.var.labels = "Price", 
          omit.stat = c("f", "ser", "adj.rsq"), digits = 2)


