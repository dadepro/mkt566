############################################################
############################################################
## Churn Modeling Exercise (No Data Creation)
## Models: Logistic Regression + Random Forest
## Metrics: Accuracy, Precision, Recall, ROC-AUC
## Plus: Threshold-by-goal and Top-K capacity checks
############################################################
############################################################

## ---- 0) Setup ------------------------------------------------------------
set.seed(123)

library(randomForest)
library(dplyr)
library(rstudioapi)
library(pROC)

#setwd as script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# read the data
df <- read.csv("data/churn_data.csv")


## ---- 0) Setup (minimal packages) ----------------------------------------
set.seed(123)

# Only these two are used; install if missing
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest", quiet = TRUE)
if (!requireNamespace("pROC", quietly = TRUE))         install.packages("pROC", quiet = TRUE)
library(randomForest)
library(pROC)

hr <- function(x) cat("\n", paste0(rep("=", nchar(x)+4), collapse=""),
                      "\n= ", x, " =\n", paste0(rep("=", nchar(x)+4), collapse=""), "\n", sep="")

## ---- 1) Bring your data --------------------------------------------------
# You already have a 'df' built elsewhere. If it's in a file, uncomment one of these:
# df <- read.csv("your_prepared_churn_data.csv")
# df <- readRDS("your_prepared_churn_data.rds")

# Expectations:
# - df$churn is 0/1 (numeric or integer). If it's "Yes/No", convert to 0/1:
#   df$churn <- ifelse(df$churn == "Yes", 1L, 0L)
# - Predictor columns: numeric/factor features you want to use.

stopifnot(exists("df"))
stopifnot("churn" %in% names(df))

# Optional: quick check
hr("Data check")
cat("Rows:", nrow(df), "  Columns:", ncol(df), "\n")
if (!all(df$churn %in% c(0,1))) stop("churn must be coded 0/1.")
cat("Churn rate:", round(mean(df$churn), 4), "\n")

## ---- 2) Train/Test split (simple, stratified by churn) -------------------
idx_pos <- which(df$churn == 1)
idx_neg <- which(df$churn == 0)

train_pos <- sample(idx_pos, floor(0.7 * length(idx_pos)))
train_neg <- sample(idx_neg, floor(0.7 * length(idx_neg)))
train_id  <- sort(c(train_pos, train_neg))
test_id   <- setdiff(seq_len(nrow(df)), train_id)

train <- df[train_id, ]
test  <- df[test_id,  ]

## ---- 3) Model formulas ---------------------------------------------------
# Use *all* columns except churn as predictors (adjust if needed).
predictor_cols <- setdiff(names(df), "churn")
form <- as.formula(paste("churn ~", paste(predictor_cols, collapse = " + ")))

## ---- 4) Fit models -------------------------------------------------------
# A) Logistic regression (interpretable baseline)
logit_fit <- glm(form, data = train, family = binomial())

# B) Random forest (captures nonlinearities & interactions)
rf_fit <- randomForest(
  factor(churn) ~ ., data = train[, c("churn", predictor_cols)],
  ntree = 300, mtry = max(2, floor(sqrt(length(predictor_cols)))), importance = TRUE
)

## ---- 5) Predict probabilities on test -----------------------------------
test$p_logit <- predict(logit_fit, newdata = test, type = "response")
# randomForest 'predict(..., type="prob")' returns a 2-col matrix for class 0/1
test$p_rf    <- predict(rf_fit, newdata = test[, predictor_cols], type = "prob")[, "1"]

## ---- 6) Helpers: labels, confusion, metrics, AUC, Top-K ------------------
to_label <- function(p, thr = 0.5) as.integer(p >= thr)

counts <- function(y_true, y_hat) {
  TP <- sum(y_hat==1 & y_true==1); FP <- sum(y_hat==1 & y_true==0)
  FN <- sum(y_hat==0 & y_true==1); TN <- sum(y_hat==0 & y_true==0)
  list(TP=TP, FP=FP, FN=FN, TN=TN)
}
metrics <- function(cc) {
  acc <- (cc$TP + cc$TN) / (cc$TP + cc$FP + cc$FN + cc$TN)
  prec <- ifelse((cc$TP + cc$FP)==0, NA, cc$TP/(cc$TP+cc$FP))
  rec  <- ifelse((cc$TP + cc$FN)==0, NA, cc$TP/(cc$TP+cc$FN))
  f1   <- ifelse(is.na(prec) | is.na(rec) | (prec+rec)==0, NA, 2*prec*rec/(prec+rec))
  c(Accuracy=acc, Precision=prec, Recall=rec, F1=f1)
}

auc_roc <- function(y_true, p_hat) {
  roc_obj <- pROC::roc(response = y_true, predictor = p_hat, quiet = TRUE)
  as.numeric(pROC::auc(roc_obj))
}

eval_topk <- function(y_true, p_hat, K) {
  ord <- order(-p_hat)
  top <- ord[seq_len(min(K, length(ord)))]
  y_top <- y_true[top]
  prec_k <- mean(y_top == 1)
  rec_k  <- sum(y_top == 1) / sum(y_true == 1)
  c(Precision_at_K = prec_k, Recall_at_K = rec_k)
}

pick_thr_for_precision <- function(y_true, p_hat, target_precision = 0.80, step = 0.001) {
  th <- seq(1, 0, by = -step); best <- NULL
  for (t in th) {
    y_hat <- to_label(p_hat, t)
    mm <- metrics(counts(y_true, y_hat))
    if (!is.na(mm["Precision"]) && mm["Precision"] >= target_precision) {
      if (is.null(best) || mm["Recall"] > best["Recall"]) best <- c(threshold=t, mm)
    }
  }
  best
}

## ---- 7) Baseline results at threshold = 0.5 ------------------------------
thr <- 0.5
yhat_logit <- to_label(test$p_logit, thr)
yhat_rf    <- to_label(test$p_rf,    thr)

m_logit <- metrics(counts(test$churn, yhat_logit))
m_rf    <- metrics(counts(test$churn, yhat_rf))
a_logit <- auc_roc(test$churn, test$p_logit)
a_rf    <- auc_roc(test$churn, test$p_rf)

hr(sprintf("Baseline @ threshold = %.2f", thr))
print(round(rbind(Logit = m_logit, RandomForest = m_rf), 3))
cat(sprintf("ROC-AUC — Logit: %.3f   |   RF: %.3f\n", a_logit, a_rf))

## ---- 8) Threshold-by-goal (e.g., Precision ≥ 80%) ------------------------
goal_prec <- 0.80
opt_logit <- pick_thr_for_precision(test$churn, test$p_logit, goal_prec)
opt_rf    <- pick_thr_for_precision(test$churn, test$p_rf,    goal_prec)

hr(sprintf("Choose threshold to reach Precision ≥ %.0f%%", goal_prec*100))
cat("Logit  -> ", if (is.null(opt_logit)) "No threshold meets target.\n" else paste(capture.output(print(round(opt_logit, 3))), collapse="\n"), "\n", sep="")
cat("RF     -> ", if (is.null(opt_rf)) "No threshold meets target.\n" else paste(capture.output(print(round(opt_rf, 3))), collapse="\n"), "\n", sep="")

## ---- 9) Capacity view: Top-K targeting (e.g., contact top 10%) -----------
K <- ceiling(0.10 * nrow(test))  # change 10% to your real capacity
tk_logit <- eval_topk(test$churn, test$p_logit, K)
tk_rf    <- eval_topk(test$churn, test$p_rf,    K)

hr(sprintf("Top-%d (%.0f%%) capacity: Precision@K / Recall@K", K, 100*K/nrow(test)))
print(round(rbind(Logit = tk_logit, RandomForest = tk_rf), 3))

## ---- 10) Pre-flight checklist (decision aid) -----------------------------
hr("Pre-flight checklist — Can we 'stick with it'?")
cat("- AUC high AND Accuracy high? Good. Now confirm the OPERATING POINT:\n",
    "   • At your chosen threshold (or Top-K), do Precision & Recall meet business targets?\n",
    "   • If offers are expensive: raise threshold to boost Precision (accept lower Recall).\n",
    "   • If missing churners is costly: lower threshold to boost Recall (accept lower Precision).\n",
    "   • If capacity-limited: skip a fixed threshold; action Top-K by score.\n",
    "   • (Optional) Check calibration and segment reliability; validate on a time-based holdout.\n", sep = "")

############################################################
## END — Student deliverable:
## (1) Which model would you deploy?
## (2) Which threshold or K would you use?
## (3) One-paragraph business rationale (precision/recall trade-off).
############################################################
