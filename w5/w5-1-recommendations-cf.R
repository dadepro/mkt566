rm(list = ls())

library(ggplot2)
library(data.table)
library(readxl)
#setwd as the directory where this file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# collaborative filtering
# install.packages("sparklyr")
# library(sparklyr)
Sys.setenv(SPARK_HOME = "/Users/dproserp/spark/spark-3.5.1-bin-hadoop3")
Sys.setenv(JAVA_HOME = "/opt/homebrew/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home")
library(SparkR, lib.loc = file.path(Sys.getenv("SPARK_HOME"), "R", "lib"))
sparkR.session(master = "local[*]")

## 1) Load ratings (MovieLens small)
data <- fread("data/movielens/ratings.csv", header = TRUE, sep = ",",
              na.strings = c("NA", "NaN", ""))
data <- data[, .(userId = as.integer(userId),
                 movieId = as.integer(movieId),
                 rating = as.numeric(rating))]

## 2) Send to Spark
df <- createDataFrame(data, schema = c("userId", "movieId", "rating"))

## 3) Train/test split (80/20) with a fixed seed
splits <- randomSplit(df, weights = c(0.8, 0.2), seed = 42L)
training <- splits[[1]]
test     <- splits[[2]]

## 4) Train ALS (explicit feedback)
## note: coldStartStrategy="drop" removes NaN preds for cold-start users/items
model <- spark.als(
  data = training,
  maxIter = 10,
  regParam = 0.1,
  rank = 20,
  userCol = "userId",
  itemCol = "movieId",
  ratingCol = "rating"
)

## 5) Predict on TEST ONLY
predictions <- predict(model, test)
head(predictions)

# 6) Compute RMSE between rating and predictions for the test dataset
# Drop rows with NaN predictions
pred_clean <- where(predictions, !isnan(predictions$prediction))

# Compute RMSE
pred_se <- withColumn(pred_clean, "se",
                      (pred_clean$prediction - pred_clean$rating) * 
                        (pred_clean$prediction - pred_clean$rating)
)

rmse_val <- collect(agg(pred_se, rmse = sqrt(mean(pred_se$se))))$rmse
print(rmse_val)


## Done
sparkR.stop()

