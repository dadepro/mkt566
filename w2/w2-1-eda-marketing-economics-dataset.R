library(datarium)
library(ggplot2)
library(tidyr)
library(data.table)
#set working directory to be the directory of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# let take a quick look at the data
head(marketing)
nrow(marketing)
#plot distribution of sales using different bandwidths

# 1) bandwidth = 10
ggplot(marketing, aes(x = sales)) + 
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Sales", x = "Sales", y = "Frequency") +
  theme_minimal()
ggsave("figures/w2-1-eda-sales-distribution-bw10.pdf", h = 3.5, w = 5)

# 2) bandwidth = 1
ggplot(marketing, aes(x = sales)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Sales", x = "Sales", y = "Frequency") +
  theme_minimal()
ggsave("figures/w2-1-eda-sales-distribution-bw1.pdf", h = 3.5, w = 5)

# 3) bandwidth = 0.1
ggplot(marketing, aes(x = sales)) + 
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Sales", x = "Sales", y = "Frequency") +
  theme_minimal()
ggsave("figures/w2-1-eda-sales-distribution-bw01.pdf", h = 3.5, w = 5)

#plot distribution of youtube spend
ggplot(marketing, aes(x = youtube)) + 
  geom_histogram(binwidth = 10, fill = "#FF0000", color = "black", alpha = 0.7) +
  labs(title = "Distribution of YouTube Marketing Spend", x = "YouTube Marketing Spend", y = "Frequency") +
  theme_minimal()
ggsave("figures/w2-1-eda-youtube-distribution-bw10.pdf", h = 3.5, w = 5)

#plot distribution of facebook spend
ggplot(marketing, aes(x = facebook)) + 
  geom_histogram(binwidth = 5, fill = "#1877F2", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Facebook Marketing Spend", x = "Facebook Marketing Spend", y = "Frequency") +
  theme_minimal()
ggsave("figures/w2-1-eda-facebook-distribution-bw5.pdf", h = 3.5, w = 5)

#plot distribution of newspaper spend
ggplot(marketing, aes(x = newspaper)) + 
  geom_histogram(binwidth = 5, fill = "black", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Newspaper Marketing Spend", x = "Newspaper Marketing Spend", y = "Frequency") +
  theme_minimal()
ggsave("figures/w2-1-eda-newspaper-distribution-bw5.pdf", h = 3.5, w = 5)

################## directly look for outliers using boxplot
# create boxplot of all ad spend variables
marketing_long = data.table(pivot_longer(marketing, cols = everything(), names_to = "variable", values_to = "value"))
ggplot(marketing_long[variable != "sales"], aes(x = variable, y = value, fill = variable)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("#1877F2", "black", "#FF0000")) +
  labs(title = "Boxplot of Marketing Variables", x = "Channel", y = "Ad Spend") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/w2-1-eda-marketing-boxplot.pdf", width = 6, height = 4)

# same but use log
ggplot(marketing_long[variable != "sales"], aes(x = variable, y = log10(value), fill = variable)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("#1877F2", "black", "#FF0000")) +
  labs(title = "Boxplot of Marketing Variables (Log Scale)", x = "Channel", y = "log Ad Spend") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("figures/w2-1-eda-marketing-boxplot-log.pdf", width = 6, height = 4)


#################### Visualizing trends ########################

# visualizing trends
ggplot(data = economics) +
  geom_line(mapping = aes(x = date, y = unemploy)) +
  labs(title = "Unemployment over time", x = "Date", y = "Unemployed (thousands)")
ggsave("figures/w2-1-eda-economics-unemployment.png", width = 6, height = 4)

# add vertical lines for unemployment peaks
ggplot(data = economics) +
  geom_line(mapping = aes(x = date, y = unemploy)) +
  labs(title = "Unemployment over time", x = "Date", y = "Unemployed (thousands)") +
  geom_vline(xintercept = as.Date(c("1975-03-01", "1983-01-01", "1992-08-01", "2003-08-01", "2009-11-01")), 
             color = "red", linetype = "dashed")
ggsave("figures/w2-1-eda-economics-unemployment-peaks.png", width = 6, height = 4)

# add long run trend line
ggplot(data = economics) +
  geom_line(mapping = aes(x = date, y = unemploy)) +
  labs(title = "Unemployment over time", x = "Date", y = "Unemployed (thousands)") +
  geom_vline(xintercept = as.Date(c("1975-03-01", "1983-01-01", "1992-08-01", "2003-08-01", "2009-11-01")), 
             color = "red", linetype = "dashed") +
  geom_smooth(mapping = aes(x = date, y = unemploy), method = "lm", color = "blue")
ggsave("figures/w2-1-eda-economics-unemployment-trend.png", width = 6, height = 4)

