############################################################
## Exercise: Predict Ad Click Probability (Logistic Reg)  ##
############################################################
set.seed(123)

library(pROC)
library(ggplot2)

#raed data
df <- read.csv("data/ad_click_data.csv")

# what is the probability of click
mean(df$click)

## 2) Split the data in train and test
test = sample(nrow(df), size = 0.3 * nrow(df))
train = df[-test, ]
test  = df[test, ]


## 3) Fit logistic regression (use all features)
logit_fit <- glm(click ~ device + ad_position + user_segment + pages_viewed + time_on_site + topic_match + past_ctr_hi, data = train, family = binomial())

## 4) Predict probabilities & classify at threshold 0.5
test$p_hat <- predict(logit_fit, newdata = test, type = "response")
test$y_hat <- as.integer(test$p_hat >= 0.5)

## 5) Metrics: Accuracy, Precision, Recall, F1
TP <- sum(test$y_hat==1 & test$click==1)
FP <- sum(test$y_hat==1 & test$click==0)
FN <- sum(test$y_hat==0 & test$click==1)
TN <- sum(test$y_hat==0 & test$click==0)

accuracy  <- (TP + TN) / (TP + FP + FN + TN)
precision <- ifelse((TP + FP)==0, NA, TP/(TP+FP))
recall    <- TP/(TP+FN)
f1        <- ifelse(is.na(precision) | (precision+recall)==0, NA, 2*precision*recall/(precision+recall))

cat("\nMetrics @ threshold = 0.50\n")
cat(sprintf("Accuracy : %.3f\nPrecision: %.3f\nRecall   : %.3f\nF1       : %.3f\n",
            accuracy, precision, recall, f1))

## 6) Interpretability: odds ratios (exp(beta))
or <- exp(coef(logit_fit))
cat("\nOdds Ratios (exp(beta)):\n"); print(round(or, 3))

## 7) Quick “business” read-out for the class
cat("\nClassroom takeaway:\n",
    "- Probabilities (p_hat) tell you how likely a user is to click the ad.\n",
    "- You can raise the threshold (>0.5) to waste fewer impressions on low-likelihood users (↑precision, ↓recall),\n",
    "  or lower it (<0.5) to catch more potential clickers (↑recall, ↓precision).\n",
    "- Odds ratios: values >1 increase click odds (e.g., 'top' position), <1 decrease them.\n", sep="")



## 8) Plot ROC curve
roc_obj <- roc(response = test$click, predictor = test$p_hat, quiet = TRUE)
fpr <- 1 - roc_obj$specificities   # x-axis: False Positive Rate (specificities is True Negative Rate)
tpr <- roc_obj$sensitivities       # y-axis: True Positive Rate

ggplot(data = data.frame(FPR = fpr, TPR = tpr), aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve", x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)") +
  theme_minimal(base_size = 13)

# 9) compute auc
auc_val <- as.numeric(auc(roc_obj))
cat(sprintf("\nAUC-ROC: %.3f\n", auc_val))


## 10) (Optional) Try a different threshold for your business goal
thr <- 0.4  # e.g., if you prefer more recall
y_hat2 <- as.integer(test$p_hat >= thr)
TP2 <- sum(y_hat2==1 & test$click==1); FP2 <- sum(y_hat2==1 & test$click==0)
FN2 <- sum(y_hat2==0 & test$click==1); TN2 <- sum(y_hat2==0 & test$click==0)
acc2 <- (TP2 + TN2) / (TP2 + FP2 + FN2 + TN2)
prec2 <- ifelse((TP2 + FP2)==0, NA, TP2/(TP2+FP2))
rec2  <- TP2/(TP2+FN2)
f12   <- ifelse(is.na(prec2) | (prec2+rec2)==0, NA, 2*prec2*rec2/(prec2+rec2))
cat(sprintf("\nMetrics @ threshold = %.2f  ->  Accuracy=%.3f  Precision=%.3f  Recall=%.3f  F1=%.3f\n",
            thr, acc2, prec2, rec2, f12))


### show the threhsold on the ROC curve
# Build a tidy table with FPR, TPR, and the matching THRESHOLD
roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,         # x-axis
  TPR = roc_obj$sensitivities,             # y-axis
  threshold = roc_obj$thresholds           # cutoff that produced this point
)

# Pick a few FPR targets to annotate (change these as you like)
fpr_targets <- c(0.01, 0.05, 0.10, 0.20, 0.30, 0.50)
idx <- sapply(fpr_targets, function(x) which.min(abs(roc_df$FPR - x)))
lab_df <- unique(roc_df[idx, ])

# Plot ROC + labeled thresholds
ggplot(roc_df, aes(FPR, TPR)) +
  geom_line(size = 1) +
  geom_point(data = lab_df, size = 2) +
  geom_text(
    data = lab_df,
    aes(label = sprintf("t=%.2f", threshold)),
    vjust = -3, size = 3.5
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "False Positive Rate", y = "True Positive Rate (Recall)",
       title = "ROC with Threshold Annotations") +
  theme_minimal(base_size = 13)


### create three models with different variables to compare ROC curve
logit_fit1 <- glm(click ~ device + ad_position + user_segment, data = train, family = binomial())
logit_fit2 <- glm(click ~ pages_viewed + time_on_site + topic_match + past_ctr_hi, data = train, family = binomial())
logit_fit3 <- glm(click ~ device + ad_position + user_segment + pages_viewed + time_on_site + topic_match + past_ctr_hi, data = train, family = binomial())
test$p_hat1 <- predict(logit_fit1, newdata = test, type = "response")
test$p_hat2 <- predict(logit_fit2, newdata = test, type = "response")
test$p_hat3 <- predict(logit_fit3, newdata = test, type = "response")

#comptue accuracy for each model at threshold 0.5
test$y_hat1 <- as.integer(test$p_hat1 >= 0.5)
test$y_hat2 <- as.integer(test$p_hat2 >= 0.5)
test$y_hat3 <- as.integer(test$p_hat3 >= 0.5)
acc1 <- sum(test$y_hat1 == test$click) / nrow(test)
acc2 <- sum(test$y_hat2 == test$click) / nrow(test)
acc3 <- sum(test$y_hat3 == test$click) / nrow(test)
cat(sprintf("\nAccuracy @ threshold = 0.50\nModel 1: %.3f\nModel 2: %.3f\nModel 3: %.3f\n",
            acc1, acc2, acc3))

roc_obj1 <- roc(response = test$click, predictor = test$p_hat1, quiet = TRUE)
roc_obj2 <- roc(response = test$click, predictor = test$p_hat2, quiet = TRUE)
roc_obj3 <- roc(response = test$click, predictor = test$p_hat3, quiet = TRUE)
# Plot ROC curves for all three models
ggplot() +
  geom_line(aes(x = 1 - roc_obj1$specificities, y = roc_obj1$sensitivities, color = "Model 1"), size = 1) +
  geom_line(aes(x = 1 - roc_obj2$specificities, y = roc_obj2$sensitivities, color = "Model 2"), size = 1) +
  geom_line(aes(x = 1 - roc_obj3$specificities, y = roc_obj3$sensitivities, color = "Model 3"), size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "False Positive Rate", y = "True Positive Rate (Recall)",
       title = "ROC Curves for Different Logistic Regression Models",
       color = "Models") +
  theme_minimal(base_size = 13)

#compute auc
auc1 <- as.numeric(auc(roc_obj1))
auc2 <- as.numeric(auc(roc_obj2))
auc3 <- as.numeric(auc(roc_obj3))
cat(sprintf("\nAUC-ROC:\nModel 1: %.3f\nModel 2: %.3f\nModel 3: %.3f\n",
            auc1, auc2, auc3))

#compute accuracy for each model at threshold 0.5
test$y_hat1 <- as.integer(test$p_hat1 >= 0.5)
test$y_hat2 <- as.integer(test$p_hat2 >= 0.5)
test$y_hat3 <- as.integer(test$p_hat3 >= 0.5)
acc1 <- sum(test$y_hat1 == test$click) / nrow(test)
acc2 <- sum(test$y_hat2 == test$click) / nrow(test)
acc3 <- sum(test$y_hat3 == test$click) / nrow(test)
cat(sprintf("\nAccuracy @ threshold = 0.50\nModel 1: %.3f\nModel 2: %.3f\nModel 3: %.3f\n",
            acc1, acc2, acc3))

#compute precision and recall for each model at threshold 0.5
precall <- function(y_true, y_pred) {
  TP <- sum(y_pred==1 & y_true==1)
  FP <- sum(y_pred==1 & y_true==0)
  FN <- sum(y_pred==0 & y_true==1)
  precision <- ifelse((TP + FP)==0, NA, TP/(TP+FP))
  recall    <- TP/(TP+FN)
  return(c(precision, recall))
}

precall1 <- precall(test$click, test$y_hat1)
precall2 <- precall(test$click, test$y_hat2)
precall3 <- precall(test$click, test$y_hat3)
cat(sprintf("\nPrecision & Recall @ threshold = 0.50\nModel 1: Precision=%.3f, Recall=%.3f\nModel 2: Precision=%.3f, Recall=%.3f\nModel 3: Precision=%.3f, Recall=%.3f\n",
            precall1[1], precall1[2],
            precall2[1], precall2[2],
            precall3[1], precall3[2]))

