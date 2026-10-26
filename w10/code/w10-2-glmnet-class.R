# ============================================================================
# MKT 566, Week 10: Predicting sentiment with a regularized logit (glmnet)
# ============================================================================
# This script reproduces every number and chart from the glmnet slides,
# in the order they appear:
#   PART 1  the data and the 80/20 split
#   PART 2  the TF-IDF matrix (vocabulary from the TRAINING reviews only)
#   PART 3  what goes wrong without a penalty: memorizing
#   PART 4  cv.glmnet: the lasso with lambda chosen by cross-validation
#   PART 5  the CV curve
#   PART 6  the scoreboard: dictionary vs. no penalty vs. lasso
#   PART 7  the words the model trusts (the coefficient chart)
#
# HOW TO RUN THIS SCRIPT (same workflow as before):
#   - Run it from the top, one step at a time: click on a line and press
#     Cmd+Enter (Mac) or Ctrl+Enter (Windows).
#   - Tables print in the terminal (the R console). Charts open in a VS Code
#     tab and are also saved as PDFs in the "figures" folder.
#
# You are NOT expected to memorize this code. Read the comment above each
# block, run it, and look at the result. If you want to understand a
# specific line, select it and ask your AI assistant to explain it.
# ============================================================================

#### install libraries ####
pkgs <- c("ggplot2", "data.table", "scales", "text2vec", "glmnet", "pROC",
          "sentimentr")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")
#####

# load libraries
library(ggplot2)      # charts
library(data.table)   # fast tables
library(scales)       # nicer axis labels
library(text2vec)     # tokenization, DTM, TF-IDF + the movie-review data
library(glmnet)       # the regularized logit: new this week
library(pROC)         # ROC curves and AUC (week 9)
library(sentimentr)   # the dictionary baseline (Tuesday)

# ---- housekeeping: safe to run without understanding -----------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
dir.create("figures", showWarnings = FALSE)
usc_red <- "#990000"; usc_gold <- "#E69F00"; blue <- "#0072B2"

# a helper we will use a few times: AUC of predictions p against true labels
auc_of <- function(truth, p) {
  as.numeric(auc(roc(truth, p, levels = c("neg", "pos"),
                     direction = "<", quiet = TRUE)))
}
# ---------------------------------------------------------------------------

# ============================================================================
# PART 1: The data and the split
# ============================================================================
# The same 5,000 labeled IMDB reviews as Tuesday. We split 80/20 and the
# test set goes in the vault: nothing below touches it until evaluation.

data("movie_review", package = "text2vec")
dt <- as.data.table(movie_review)
dt[, id := .I]
dt[, label := factor(ifelse(sentiment == 1, "pos", "neg"))]

set.seed(123)
idx   <- sample.int(nrow(dt), floor(0.8 * nrow(dt)))
train <- dt[idx]
test  <- dt[-idx]
nrow(train); nrow(test)    # 4,000 train, 1,000 test

# ============================================================================
# PART 2: The TF-IDF matrix
# ============================================================================
# Tuesday's pipeline, with the week 9 discipline on top: the vocabulary and
# the TF-IDF weights are learned from the TRAINING reviews only, then
# applied to the test reviews. The test set must not shape the features.

# tokenize the training reviews
it_train <- itoken(tolower(train$review), tokenizer = word_tokenizer,
                   ids = train$id, progressbar = FALSE)

# vocabulary: every distinct word in the training reviews...
vocab_full <- create_vocabulary(it_train)
nrow(vocab_full)                       # ~38,000 distinct words

# ...pruned: drop words seen fewer than 10 times. You cannot learn what
# "ferrari" means for sentiment from 3 examples.
vocab <- prune_vocabulary(vocab_full, term_count_min = 10)
nrow(vocab)                            # ~6,700 words survive

# counts (the bag-of-words matrix), train and test in the SAME columns
vectorizer <- vocab_vectorizer(vocab)
dtm_train  <- create_dtm(it_train, vectorizer)
it_test    <- itoken(tolower(test$review), tokenizer = word_tokenizer,
                     ids = test$id, progressbar = FALSE)
dtm_test   <- create_dtm(it_test, vectorizer)

# TF-IDF: fit the weights on train, apply to both
tfidf   <- TfIdf$new(norm = "l2")
X_train <- tfidf$fit_transform(dtm_train)
X_test  <- tfidf$transform(dtm_test)
dim(X_train)   # 4,000 x ~6,700: still wider than it is tall

# ============================================================================
# PART 3: No penalty = memorizing
# ============================================================================
# A logit with (almost) no penalty on 6,700 columns labels every TRAINING
# review perfectly, and does worse on the reviews it never saw. The gap
# between the two numbers IS overfitting (week 9's trap at full scale).

m0 <- glmnet(X_train, train$label, family = "binomial",
             lambda = 1e-5, alpha = 1)   # lambda ~ 0: no real penalty

p0_train <- as.numeric(predict(m0, X_train, type = "response"))
p0_test  <- as.numeric(predict(m0, X_test,  type = "response"))

# training set: perfect. test set: not.
mean((p0_train >= 0.5) == (train$label == "pos"))   # accuracy on train: 100%
auc_of(train$label, p0_train)                       # AUC on train: 1.00
mean((p0_test >= 0.5) == (test$label == "pos"))     # accuracy on test: ~83%
auc_of(test$label, p0_test)                         # AUC on test: ~0.91
sum(coef(m0)[-1] != 0)                              # ~2,000 words used

# ============================================================================
# PART 4: The lasso, with lambda chosen by cross-validation
# ============================================================================
# One line does it all: cv.glmnet tries ~100 values of the penalty lambda,
# runs 5-fold cross-validation ON THE TRAINING SET for each, and records
# the average AUC. alpha = 1 means lasso: many coefficients get pushed to
# exactly zero.

set.seed(123)   # the folds are random; fix them for reproducibility
cvfit <- cv.glmnet(X_train, train$label, family = "binomial",
                   type.measure = "auc", nfolds = 5, alpha = 1)

cvfit$lambda.min   # the lambda with the best average AUC
cvfit$lambda.1se   # the heaviest lambda within 1 SE of the best (simpler)

# how many words does each choice keep?
sum(coef(cvfit, s = "lambda.min")[-1] != 0)   # ~500
sum(coef(cvfit, s = "lambda.1se")[-1] != 0)   # ~280

# ============================================================================
# PART 5: The CV curve
# ============================================================================
# One point per candidate lambda: the average AUC across the 5 folds.
# Left edge: tiny penalty, memorizing. Right edge: heavy penalty, the model
# is forced to drop everything and collapses toward a coin flip.

cv_dt <- data.table(lambda = cvfit$lambda, auc = cvfit$cvm,
                    lo = cvfit$cvlo, up = cvfit$cvup)

p_cv <- ggplot(cv_dt, aes(x = lambda, y = auc)) +
  geom_ribbon(aes(ymin = lo, ymax = up), fill = "grey85") +
  geom_line(color = usc_red, linewidth = 1) +
  geom_vline(xintercept = cvfit$lambda.min, linetype = "dashed", color = blue) +
  geom_vline(xintercept = cvfit$lambda.1se, linetype = "dashed", color = usc_gold) +
  scale_x_log10() +
  labs(title = "Cross-Validated AUC, Across ~100 Values of the Penalty",
       x = "the penalty strength lambda (log scale)",
       y = "average AUC across the 5 folds") +
  theme_minimal(base_size = 14)
p_cv
ggsave("figures/04_cv_curve.pdf", p_cv, width = 8, height = 4.5)

# ============================================================================
# PART 6: The scoreboard
# ============================================================================
# Everyone gets graded on the SAME 1,000 test reviews:
#   1. Tuesday's dictionary (sentimentr), which needs no training data
#   2. the no-penalty logit from PART 3
#   3. the lasso at lambda.min

# the dictionary, scored on the test reviews
test_clean <- gsub("<[^>]+>", " ", test$review)
dict_sc    <- sentiment_by(get_sentences(test_clean))$ave_sentiment

# the lasso at lambda.min
p_lasso <- as.numeric(predict(cvfit, X_test, s = "lambda.min",
                              type = "response"))

scoreboard <- data.table(
  model    = c("coin flip", "dictionary (sentimentr)",
               "logit, no penalty", "lasso at lambda.min"),
  AUC      = c(0.5,
               auc_of(test$label, dict_sc),
               auc_of(test$label, p0_test),
               auc_of(test$label, p_lasso)),
  accuracy = c(0.5,
               mean((dict_sc > 0)      == (test$label == "pos")),
               mean((p0_test >= 0.5)   == (test$label == "pos")),
               mean((p_lasso >= 0.5)   == (test$label == "pos"))))
scoreboard[, `:=`(AUC = round(AUC, 3), accuracy = round(accuracy, 3))]
scoreboard
# training on labels beats the dictionary; the penalty beats no penalty,
# with a model a quarter of the size

# ============================================================================
# PART 7: The words the model trusts
# ============================================================================
# The lasso's surviving coefficients ARE the model: a review's score is, at
# heart, the sum of the coefficients of the words it contains. Read the
# biggest ones and check they make sense.

cf <- coef(cvfit, s = "lambda.min")
cf <- data.table(word = rownames(cf)[-1], beta = as.numeric(cf)[-1])[beta != 0]
nrow(cf)                    # the whole model: ~500 words

cf[order(-beta)][1:10]      # the 10 strongest POSITIVE words
cf[order(beta)][1:10]       # the 10 strongest NEGATIVE words

top_words <- rbind(cf[order(beta)][1:10][, side := "negative"],
                   cf[order(-beta)][1:10][, side := "positive"])
top_words <- top_words[order(beta)]
top_words[, word := factor(word, levels = word)]

p_coef <- ggplot(top_words, aes(x = beta, y = word, fill = side)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = c(negative = usc_red, positive = blue),
                    name = NULL) +
  labs(title = "The 10 Biggest Coefficients, Each Way",
       x = "the word's coefficient in the lasso logit", y = NULL) +
  theme_minimal(base_size = 14) + theme(legend.position = "top")
p_coef
ggsave("figures/05_lasso_words.pdf", p_coef, width = 8, height = 4.5)

# ----------------------------------------------------------------------------
# That's the whole recipe: label, split, vectorize on train, cv.glmnet,
# evaluate on test, read the words. Change step 1 and the same code runs a
# spam filter or a fake-review detector. Now: the game
# (w10-sentiment-analysis-game.R).
# ----------------------------------------------------------------------------
