library(ggplot2)
library(data.table)
#setwd as the directory where this file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# barplot of average price by cut for diamonds
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = price), stat = "summary", fun = "mean") +
  labs(title = "Average Price by Cut", x = "Cut", y = "Average Price")
ggsave("w3-1-eda-diamonds-barplot-cut-price.png", width = 6, height = 4)

# use boxplot
ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = cut, y = price)) +
  labs(title = "Price by Cut", x = "Cut", y = "Price") +
  scale_y_continuous(labels = scales::dollar_format())
ggsave("w3-1-eda-diamonds-boxplot-cut-price.png", width = 6, height = 4)

# scatter plot average price by carat
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price)) +
  labs(title = "Price by Carat", x = "Carat", y = "Price") +
  scale_x_continuous(breaks = seq(0, 5, 0.5)) +
  scale_y_continuous(labels = scales::dollar_format())
ggsave("w3-1-eda-diamonds-scatter-carat-price.png", width = 6, height = 4)

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 1/50) +
  labs(title = "Price by Carat", x = "Carat", y = "Price") +
  scale_x_continuous(breaks = seq(0, 5, 0.5)) +
  scale_y_continuous(labels = scales::dollar_format())
ggsave("w3-1-eda-diamonds-scatter-carat-price-trasp.png", width = 6, height = 4)

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1))) +
  coord_cartesian(xlim = c(0, 3)) +
  scale_x_continuous(breaks = seq(0, 3, 0.2)) +
  scale_y_continuous(labels = scales::dollar_format()) +  
  labs(title = "Price by Carat", x = "Carat", y = "Price")
ggsave("w3-1-eda-diamonds-boxplot-carat-price.png", width = 6, height = 4)
