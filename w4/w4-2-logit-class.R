library(data.table)
library(stargazer)
library(rstudioapi)

#setwd as the directory where this file is saved
setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/airbnb-case"))


airbnb = fread("w4-airbnb-case.csv.gz")
head(airbnb)

airbnb[, gem:=as.integer(star_rating>=4.5 & reviews_count>20)]
table(airbnb$gem)

#predict probability of being a gem using logistic regression
m_logit = glm(gem ~ price + guests_included + city + room_type, data = airbnb, family = binomial)
stargazer(m_logit, type = "text", 
          omit.stat = c("f", "ser", "aic", "bic"))
