# ============================================================================
# MKT 566, Week 9: Classifiers, a churn model end to end
# ============================================================================
# This script reproduces every chart and table from the classifiers slides,
# in the order they appear:
#   PART 1  the data: 3,000 customers of a subscription business
#   PART 2  the train/test split (slide "The fix: hold some data out")
#   PART 3  two models: a logit and a random forest
#   PART 4  the scores on the test set (the histogram slide)
#   PART 5  the confusion matrix and the metrics at the 0.5 default
#   PART 6  the threshold dial: precision vs. recall (the sweep chart)
#   PART 7  ROC curves and AUC, logit vs. forest
#   PART 8  what drives churn: odds ratios from the logit
#   PART 9  what the forest relied on: feature importance
#   PART 10 acting on the scores: top-K targeting
#
# HOW TO RUN THIS SCRIPT (same workflow as before):
#   - Run it from the top, one step at a time: click on a line and press
#     Cmd+Enter (Mac) or Ctrl+Enter (Windows).
#   - Tables print in the terminal (the R console). Charts open in a VS Code
#     tab and are also saved as PDFs in the "figures" folder.
#   - Lines starting with # are comments: notes for humans, ignored by R.
#
# You are NOT expected to memorize this code. Read the comment above each
# block, run it, and look at the result. If you want to understand a
# specific line, select it and ask your AI assistant to explain it.
# ============================================================================

#### install libraries ####
pkgs <- c("ggplot2", "data.table", "scales", "randomForest", "pROC")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(ggplot2)      # charts
library(data.table)   # fast tables
library(scales)       # nicer axis labels
library(randomForest) # the random forest model: new this week
library(pROC)         # ROC curves and AUC: new this week

# ---- housekeeping: safe to run without understanding -----------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
dir.create("figures", showWarnings = FALSE)
usc_red <- "#990000"; blue <- "#0072B2"
# the data sits in data/ next to this script (the downloaded zip); in the
# course repo it sits one folder up, in w9/data/
data_path <- if (file.exists("data/churn_data.csv")) {
  "data/churn_data.csv"
} else {
  file.path("..", "data", "churn_data.csv")
}
# ---------------------------------------------------------------------------

# ============================================================================
# PART 1: The data
# ============================================================================
# One row per customer: six features plus the region, and the outcome we
# want to predict: churn (1 = canceled, 0 = stayed).
churn <- fread(data_path)
churn[, region := factor(region)]

head(churn)
nrow(churn)                      # 3,000 customers
mean(churn$churn)                # the churn rate: about 18%

# ============================================================================
# PART 2: The train/test split
# ============================================================================
# We grade the model on customers it has NEVER seen. Split 70/30 before
# fitting anything, keeping the churn rate similar in both parts
# (that is what the stratified sampling below does).
set.seed(123)   # so everyone gets the same split
idx_pos  <- which(churn$churn == 1)
idx_neg  <- which(churn$churn == 0)
train_id <- sort(c(sample(idx_pos, floor(0.7 * length(idx_pos))),
                   sample(idx_neg, floor(0.7 * length(idx_neg)))))
train <- churn[train_id]
test  <- churn[-train_id]

nrow(train); nrow(test)          # about 2,100 train, 900 test
mean(train$churn); mean(test$churn)   # similar churn rates: the split is fair

# ============================================================================
# PART 3: Two models
# ============================================================================
# The baseline: a logit (week 5), fit on the training set only.
logit_fit <- glm(churn ~ ., data = train, family = binomial())
summary(logit_fit)

# The challenger: a random forest (300 trees, each on a random sample).
set.seed(123)
rf_fit <- randomForest(factor(churn) ~ ., data = train,
                       ntree = 300, importance = TRUE)
rf_fit

# ============================================================================
# PART 4: The scores on the test set
# ============================================================================
# Each model gives every TEST customer a predicted churn probability.
test[, p_logit := predict(logit_fit, newdata = test, type = "response")]
test[, p_rf    := predict(rf_fit, newdata = test, type = "prob")[, "1"]]

# The histogram from the slides: scores, colored by what actually happened.
ggplot(test, aes(x = p_logit, fill = factor(churn))) +
  geom_histogram(bins = 40, alpha = 0.75, position = "identity", color = "white") +
  scale_fill_manual(values = c("0" = "grey65", "1" = usc_red),
                    labels = c("stayed", "churned"), name = NULL) +
  scale_x_continuous(labels = percent) +
  labs(title = "Predicted churn probability on the test set",
       x = "Predicted probability of churn (logit)", y = "Customers") +
  theme_minimal(base_size = 14) + theme(legend.position = "top")
ggsave("figures/01-score-histogram.pdf", width = 8, height = 4.4)

# ============================================================================
# PART 5: The confusion matrix and the three metrics
# ============================================================================
# A helper we will reuse: count the four boxes at a given threshold, then
# compute accuracy, precision, and recall from them.
conf_metrics <- function(y, p, thr = 0.5) {
  yhat <- as.integer(p >= thr)
  TP <- sum(yhat == 1 & y == 1); FP <- sum(yhat == 1 & y == 0)
  FN <- sum(yhat == 0 & y == 1); TN <- sum(yhat == 0 & y == 0)
  data.table(threshold = thr, TP = TP, FP = FP, FN = FN, TN = TN,
             accuracy  = (TP + TN) / length(y),
             precision = ifelse(TP + FP == 0, NA, TP / (TP + FP)),
             recall    = ifelse(TP + FN == 0, NA, TP / (TP + FN)))
}

# Both models at the 0.5 default: the metrics table from the slides.
# Note the pattern: accuracy looks great, recall is poor. With a rare
# outcome, few customers score above 0.5, so the models are "shy".
rbind(logit  = conf_metrics(test$churn, test$p_logit),
      forest = conf_metrics(test$churn, test$p_rf))

# ============================================================================
# PART 6: The threshold dial
# ============================================================================
# Score the SAME logit at every threshold from 5% to 60% and watch
# precision and recall trade off.
sweep <- rbindlist(lapply(seq(0.05, 0.60, by = 0.01), function(t)
  conf_metrics(test$churn, test$p_logit, t)))
sweep <- sweep[!is.na(precision)]

ggplot(melt(sweep[, .(threshold, precision, recall)], id.vars = "threshold"),
       aes(x = threshold, y = value, color = variable)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c(precision = usc_red, recall = blue),
                     labels = c("precision (offers not wasted)",
                                "recall (churners caught)"), name = NULL) +
  scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
  labs(title = "Precision and recall as the threshold moves (logit, test set)",
       x = "Threshold: predict churn when score is above...", y = NULL) +
  theme_minimal(base_size = 14) + theme(legend.position = "top")
ggsave("figures/02-threshold-dial.pdf", width = 8, height = 4.4)

# ============================================================================
# PART 7: ROC curves and AUC
# ============================================================================
roc_logit <- roc(test$churn, test$p_logit, quiet = TRUE)
roc_rf    <- roc(test$churn, test$p_rf,    quiet = TRUE)
auc(roc_logit)   # about 0.82
auc(roc_rf)      # about 0.79: the simple model wins here

roc_dt <- rbind(
  data.table(fpr = 1 - roc_logit$specificities, tpr = roc_logit$sensitivities,
             model = sprintf("logit (AUC = %.2f)", auc(roc_logit))),
  data.table(fpr = 1 - roc_rf$specificities, tpr = roc_rf$sensitivities,
             model = sprintf("random forest (AUC = %.2f)", auc(roc_rf))))
ggplot(roc_dt, aes(x = fpr, y = tpr, color = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = setNames(c(usc_red, blue), unique(roc_dt$model)),
                     name = NULL) +
  scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
  labs(title = "ROC curves on the test set",
       x = "Stayers wrongly flagged (false positive rate)",
       y = "Churners caught (recall)") +
  theme_minimal(base_size = 14) + theme(legend.position = "top")
ggsave("figures/03-roc-curves.pdf", width = 8, height = 4.4)

# ============================================================================
# PART 8: What drives churn: odds ratios from the logit
# ============================================================================
# exp(coefficient) = how one feature multiplies the odds of churn,
# holding the others fixed. Above 1 raises risk, below 1 protects.
or <- data.table(term = names(coef(logit_fit)),
                 odds = exp(coef(logit_fit)))[term != "(Intercept)"]
or_labels <- c(
  tenure_months      = "tenure (per extra month)",
  logins_last30      = "logins last 30 days (per extra login)",
  tickets_30d        = "support tickets (per extra ticket)",
  email_engagement   = "email engagement (0 to 1)",
  price_increase_60d = "price increase in last 60 days (yes vs. no)",
  plan_downgrade     = "plan downgrade (yes vs. no)")
or <- or[term %in% names(or_labels)]       # keep the chart readable: no region dummies
or[, label := or_labels[term]]
or <- or[order(odds)]
or[, label := factor(label, levels = label)]
or

ggplot(or, aes(x = odds, y = label)) +
  geom_col(fill = usc_red, width = 0.65) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  geom_text(aes(label = sprintf("%.2f", odds)), hjust = -0.15, size = 3.4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Odds ratios from the logit",
       x = "Odds ratio (dashed line = no effect)", y = NULL) +
  theme_minimal(base_size = 13)
ggsave("figures/04-odds-ratios.pdf", width = 7, height = 4.8)

# ============================================================================
# PART 9: What the forest relied on: feature importance
# ============================================================================
# No coefficients here, so we ask a blunter question: how much WORSE do
# predictions get if we scramble one feature?
plain_names <- c(
  tenure_months      = "tenure (months)",
  logins_last30      = "logins, last 30 days",
  tickets_30d        = "support tickets, last 30 days",
  email_engagement   = "email engagement (0 to 1)",
  price_increase_60d = "price increase, last 60 days",
  plan_downgrade     = "plan downgrade",
  region             = "region")
imp <- data.table(term = rownames(importance(rf_fit)),
                  gain = importance(rf_fit)[, "MeanDecreaseAccuracy"])
imp[, label := plain_names[term]]
imp <- imp[order(gain)]
imp[, label := factor(label, levels = label)]
imp

ggplot(imp, aes(x = gain, y = label)) +
  geom_col(fill = blue, width = 0.65) +
  labs(title = "Random forest: feature importance",
       x = "Importance (drop in accuracy if the feature is shuffled)", y = NULL) +
  theme_minimal(base_size = 13)
ggsave("figures/05-feature-importance.pdf", width = 7, height = 4.8)

# ============================================================================
# PART 10: Acting on the scores: top-K targeting
# ============================================================================
# Suppose the retention team can contact 10% of the test customers.
# Skip the threshold: rank by score and take the top K.
K <- ceiling(0.10 * nrow(test))
top_rf <- test[order(-p_rf)][1:K]

mean(top_rf$churn)                     # precision at the top 10%
sum(top_rf$churn) / sum(test$churn)    # recall at the top 10%
mean(test$churn)                       # the base rate, for comparison

# The business case in two numbers: the calls are about 3 times better than
# calling at random, and the list covers about a third of all churners.

# ============================================================================
# YOUR TURN (the in-class exercise): same recipe, new question.
# Predict who clicks the ad: w9/case/w9-predicting-ad-click-w-logit.html
# ============================================================================
