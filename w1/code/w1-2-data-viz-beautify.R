library(ggplot2)
library(ggthemes)

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

# create the figure output folder if it does not exist
dir.create("chart-types", showWarnings = FALSE)

# Two years of weekly store-level sales of two products (Chapman & Feit)
store.df <- read.csv("data/store-sales.csv")
head(store.df)
str(store.df)

# We visualize the most fundamental relationship in marketing — the demand
# curve: price (p1price) vs. weekly unit sales (p1sales) of product 1.

# Step 0: the default plot
ggplot(data = store.df) +
  geom_point(mapping = aes(x = p1price, y = p1sales))

# What is wrong with it?
# - axis labels are cryptic (what is p1price?) and units are unclear
# - overplotting: prices take a few discrete values, points pile up
# - fonts are small, labels sit too close to the axes

# Step 1: label the axes (names + units)
ggplot(data = store.df) +
  geom_point(mapping = aes(x = p1price, y = p1sales)) +
  labs(x = "Price of P1 ($)", y = "Weekly Sales of P1 (units)")

# Step 2: larger fonts, more breathing room
ggplot(data = store.df) +
  geom_point(mapping = aes(x = p1price, y = p1sales)) +
  labs(x = "\nPrice of P1 ($)", y = "Weekly Sales of P1 (units)\n") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12))
ggsave("chart-types/beautify-step2.png", width = 5, height = 3.5)

# Step 3: a cleaner theme
ggplot(data = store.df) +
  geom_point(mapping = aes(x = p1price, y = p1sales)) +
  labs(x = "\nPrice of P1 ($)", y = "Weekly Sales of P1 (units)\n") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12)) +
  theme_few()
ggsave("chart-types/beautify-step3.png", width = 5, height = 3.5)

# Step 4: fix the overplotting — jitter + transparency
ggplot(data = store.df) +
  geom_jitter(mapping = aes(x = p1price, y = p1sales),
              width = 0.02, alpha = 0.4) +
  labs(x = "\nPrice of P1 ($)", y = "Weekly Sales of P1 (units)\n") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12)) +
  theme_few()
ggsave("chart-types/beautify-step4.png", width = 5, height = 3.5)

# Step 5: add the trend
ggplot(data = store.df) +
  geom_jitter(mapping = aes(x = p1price, y = p1sales),
              width = 0.02, alpha = 0.4) +
  labs(x = "\nPrice of P1 ($)", y = "Weekly Sales of P1 (units)\n") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12)) +
  geom_smooth(mapping = aes(x = p1price, y = p1sales),
              method = "lm", color = "blue") +
  theme_few()
ggsave("chart-types/beautify-step5.png", width = 5, height = 3.5)

# Step 6: add a title
# A downward-sloping demand curve: how much sales fall when price rises is the
# price elasticity — we will estimate it with regression in week 4.
ggplot(data = store.df) +
  geom_jitter(mapping = aes(x = p1price, y = p1sales),
              width = 0.02, alpha = 0.4) +
  labs(x = "\nPrice of P1 ($)", y = "Weekly Sales of P1 (units)\n",
       title = "Weekly P1 Sales Decline with Price") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12)) +
  geom_smooth(mapping = aes(x = p1price, y = p1sales),
              method = "lm", color = "blue") +
  theme_few()
ggsave("chart-types/beautify-step6.png", width = 5, height = 3.5)
