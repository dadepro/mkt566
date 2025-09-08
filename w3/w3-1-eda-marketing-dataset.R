library(datarium)
library(ggplot2)
library(tidyr)
library(data.table)
#set working directory to be the directory of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the data
head(marketing)
marketing <- as.data.table(marketing)

### Explore the relationship between two continuous variables: sales and ad spend

# scarter plot sales by ad spend
#row sum ad spend
marketing[, total_ad_spend := youtube + facebook + newspaper]
#plot total ad spend vs sales
ggplot(marketing, aes(x = total_ad_spend, y = sales)) + 
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  labs(title = "Sales vs Total Ad Spend", x = "\nTotal Ad Spend", y = "Sales\n") +
  theme_minimal() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size=16, hjust = 0.5))
ggsave("figures/w3-1-eda-sales-by-total-adspend-scatter.pdf", width = 6, height = 4)

# Scatter plot sales by ad spend for all three channels
# First convert the dataset to long format
marketing_long = data.table(pivot_longer(marketing, cols = -sales, names_to = "channel", values_to = "ad_spend"))
# bar plot of sales vs ad spend by channel
ggplot(marketing_long, aes(x = ad_spend, y = sales, color = channel)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  labs(title = "Sales vs Ad Spend by Channel", x = "\nAd Spend", y = "Sales\n") +
  theme_minimal() +
  scale_color_manual(values = c("youtube" = "#FF0000", "facebook" = "#1877F2", "newspaper" = "black")) + 
  facet_wrap(~channel, scales = "free_x") +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size=16, hjust = 0.5),
        strip.text = element_text(size=14))
ggsave("figures/w3-1-eda-sales-by-adspend-channel-scatter.pdf", width = 8, height = 4)



##### use the simulated marketing dataset we explored last week

#### Explore the relationship between one categorical and one continuous variable

df = fread("data/marketing_eda.csv")
head(df)

# barplot of averages purchases by gender
avg_purchases_gender <- df[, .(
  avg_purchases = mean(Purchases),                     # mean
  se_purchases  = sd(Purchases) / sqrt(.N)             # standard error
), by = Gender]

ggplot(avg_purchases_gender, aes(x = Gender, y = avg_purchases)) + 
  geom_bar(stat = "identity", fill = c("#1E90FF", "#FF69B4")) +
  labs(title = "Avg. Purchases by Gender", x = "\nGender", y = "Avg. Purchases\n") +
  theme_minimal() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size=16, hjust = 0.5)) 
ggsave("figures/w3-1-purchases-by-gender-barplot.pdf", width = 5, height = 3.5)

# add error bars to the plot
ggplot(avg_purchases_gender, aes(x = Gender, y = avg_purchases)) + 
  geom_bar(stat = "identity", fill = c("#1E90FF", "#FF69B4")) +
  geom_errorbar(aes(
    ymin = avg_purchases - 1.96 * se_purchases,
    ymax = avg_purchases + 1.96 * se_purchases
  ),
  width = 0.2,            # controls horizontal width of the error bar ends
  colour = "black",
  linewidth = 0.8
  ) +
  labs(title = "Avg. Purchases by Gender", x = "\nGender", y = "Avg. Purchases\n") +
  theme_minimal() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size=16, hjust = 0.5)) 
ggsave("figures/w3-1-purchases-by-gender-barplot-se.pdf", width = 5, height = 3.5)

# do the same plot but facet it by channel
avg_purchases_gender <- df[, .(
  avg_purchases = mean(Purchases),                     # mean
  se_purchases  = sd(Purchases) / sqrt(.N)             # standard error
), by = .(Gender, Channel)]

ggplot(avg_purchases_gender, aes(x = Gender, y = avg_purchases)) + 
  geom_bar(stat = "identity", aes(fill = Gender)) +
  scale_fill_manual(values = c("#1E90FF", "#FF69B4")) +
  labs(title = "Avg. Purchases by Gender Broken Down by Ad Channel", x = "\nGender", y = "Avg. Purchases\n") +
  facet_wrap(~Channel) +
  theme_minimal() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size=16, hjust = 0.5),
        strip.text = element_text(size=14),
        legend.position = "none") 
ggsave("figures/w3-1-purchases-by-gender-channel-barplot.pdf", width = 8, height = 6)

#### Explore the relationship between two categorical variables: ad spend by channel and device

# create an heatmap of ad spend by channel and device
avg_adspend_channel_device <- df[, .(
  avg_adspend = mean(Ad_Spend)                     # mean
), by = .(Channel, Device)]

# heatmap
ggplot(avg_adspend_channel_device, aes(x = Channel, y = Device, fill = avg_adspend)) + 
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_text(aes(label = round(avg_adspend, 1)), color = "white", size = 5) +
  labs(title = "Avg. Ad Spend by Channel and Device", x = "\nChannel", y = "Device\n", fill = "Avg. Ad Spend") +
  theme_minimal() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size=16, hjust = 0.5),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))
ggsave("figures/w3-1-adspend-by-channel-device-heatmap.pdf", width = 7, height = 4)
 