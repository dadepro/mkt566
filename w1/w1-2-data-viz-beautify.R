library(ggplot2)
library(ggthemes)
library(data.table)

#setwd as the directory where this file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

head(mpg)
str(mpg)

# original plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

#now change y and x label name
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(x = "Engine Displacement (L)", y = "Highway (MPG)")

#increase label size
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(x = "Engine Displacement (L)", y = "Highway (MPG)") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

#increase breaks font size
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(x = "\nEngine Displacement (L)", y = "Highway (MPG)\n") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
ggsave("w1-data-viz-1.png", width = 5, height = 3.5)

# set theme_few()
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(x = "\nEngine Displacement (L)", y = "Highway (MPG)\n") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  theme_few()
ggsave("w1-data-viz-1-few.png", width = 5, height = 3.5)

# add geom_smooth
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(x = "\nEngine Displacement (L)", y = "Highway (MPG)\n") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  geom_smooth(mapping = aes(x = displ, y = hwy), method = "lm", color = "blue") +
  theme_few()
ggsave("w1-data-viz-2.png", width = 5, height = 3.5)

# add title
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(x = "\nEngine Displacement (L)", y = "Highway (MPG)\n", 
       title = "Engine Displacement vs. Highway MPG") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  geom_smooth(mapping = aes(x = displ, y = hwy), method = "lm", color = "blue") +
  theme_few()
ggsave("w1-data-viz-3.png", width = 5, height = 3.5)
