# ============================================================
# Sentiment classification with sentimentr, TF-IDF, and PRETRAINED Word2Vec
# ============================================================

set.seed(123)

#set working dir to be same as script
setwd("~/Dropbox/teaching/mkt566-2025/mkt566/w10/code/")

# ---- Packages ----
# install.packages(c("data.table","sentimentr","text2vec","glmnet","Matrix","pROC","yardstick","stopwords","wordVectors"))
library(data.table)
library(sentimentr)   # lexicon-based sentiment scores
library(text2vec)     # tokenization, DTM/TF-IDF
library(glmnet)       # sparse logistic regression
library(Matrix)
library(pROC)         # ROC/AUC
library(yardstick)    # confusion matrix & metrics
library(stopwords)
library(ggplot2)
library(word2vec)


# ---- Data ----
data("movie_review", package = "text2vec")                 # load a small movie-review dataset that ships with {text2vec}
dt <- as.data.table(movie_review)                          # convert to data.table for fast, convenient manipulation
dt[, id := .I]                                             # create a unique integer id per row (.I is row index)

# ---- Labels from sentimentr (binarize by polarity) ----
# sentiment_by() computes a sentiment polarity score per document (here, each review),
# handling valence shifters (e.g., "not good") and aggregating over sentences.
sent_scores <- sentiment_by(dt$review, by = dt$id)

# Merge the average sentiment back into our main table; keep just id and the
# document-level average (ave_sentiment) renamed to polarity.
dt <- merge(
  dt,
  as.data.table(sent_scores)[, .(id, polarity = ave_sentiment)],
  by = "id"
)

# Turn the continuous polarity score into a binary classification label:
# "pos" if polarity > 0, otherwise "neg". Store as a factor for modeling.
dt[, target := factor(ifelse(polarity > 0, "pos", "neg"))]
y <- dt$target                                             # convenience vector for labels (not strictly needed below)

# ---- Split ----
# Random 80/20 split to create train/test partitions. This ensures that all
# preprocessing steps that learn parameters (e.g., TF-IDF IDF weights) are fit
# **only** on training data, then applied to the held-out test data.
idx <- sample.int(nrow(dt), floor(0.8 * nrow(dt)))
train <- dt[idx]                                           # training set (80%)
test  <- dt[-idx]                                          # test set (20%)
y_train <- train$target
y_test  <- test$target

# ---- Tokenization / DTM / TF-IDF (train-only fit) ----
# Define simple preprocessing and tokenization:
# - tolower() for case normalization (helps vocab match)
# - word_tokenizer from {text2vec} splits reviews into word tokens
# - stopwords list is prepared (not directly used below but handy if you want to filter vocab)
prep_fun <- tolower
tok_fun  <- word_tokenizer
stop_en  <- stopwords("en")

# Build an iterator over training reviews that applies preprocessing + tokenization on the fly.
# ids=train$id keeps doc ids aligned through the pipeline; progressbar=TRUE shows iteration progress.
it_train <- itoken(
  prep_fun(train$review),
  tokenizer   = tok_fun,
  ids         = train$id,
  progressbar = TRUE
)

# Create a vocabulary (unique terms) from the training iterator, including unigrams and bigrams.
# Then prune the vocabulary to remove:
# - very rare terms (term_count_min = 5),
# - very common terms (doc_proportion_max = 0.5) that appear in >50% of documents (often stopword-like).
vocab <- create_vocabulary(it_train, ngram = c(1, 2)) |> 
  prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.5)

# Convert the vocabulary into a vectorizer object that maps tokens -> column indices in a Document-Term Matrix (DTM).
vectorizer <- vocab_vectorizer(vocab)

# Create the sparse document-term matrix (DTM) for the training set (rows = docs, cols = terms).
dtm_train <- create_dtm(it_train, vectorizer)

# Build the same iterator for the test set and map it into a DTM **using the training vocabulary**.
# This guarantees that the test set uses the same feature space (no data leakage).
it_test  <- itoken(
  prep_fun(test$review),
  tokenizer   = tok_fun,
  ids         = test$id,
  progressbar = TRUE
)
dtm_test <- create_dtm(it_test, vectorizer)

# Initialize TF-IDF transformer (L2 norm enforces unit-length rows after transform).
# Fit IDF weights on the training DTM only, then transform both train and test DTMs.
tfidf <- TfIdf$new(norm = "l2")
X_train_tfidf <- tfidf$fit_transform(dtm_train)            # learn IDF on train; apply to train
X_test_tfidf  <- tfidf$transform(dtm_test)                 # apply learned IDF to test

# ---- Baseline: TF-IDF + Logistic Regression ----
# Cross-validated (5-fold) logistic regression on sparse TF-IDF features.
# type.measure = "auc" picks the lambda (regularization strength) that maximizes AUC on CV folds.
cv_tfidf <- cv.glmnet(
  X_train_tfidf, y_train,
  family = "binomial",
  type.measure = "auc",
  nfolds = 5
)

# Refit a final model at the best lambda from CV on the full training data.
mdl_tfidf <- glmnet(
  X_train_tfidf, y_train,
  family = "binomial",
  lambda = cv_tfidf$lambda.min
)

# --- IMPORTANT: You need prediction probabilities and hard labels before metrics ---
# The variables `p_tfidf` (probabilities) and `pred_tfidf` (class labels)
p_tfidf   <- as.numeric(predict(mdl_tfidf, X_test_tfidf, type = "response"))   # P(y = "pos")
pred_tfidf <- factor(ifelse(p_tfidf >= 0.5, "pos", "neg"), levels = levels(y_test))

# compute confusion matrix
# yardstick::conf_mat expects a data.frame with columns: truth and .pred_class (factor labels).
cm_tfidf <- yardstick::conf_mat(
  data.frame(truth = y_test, .pred_class = pred_tfidf),
  truth, .pred_class
)

# compute accuracy, precision, recall, auc
# - accuracy: overall fraction of correct predictions
# - precision: among predicted positives, fraction that are actually positive (PPV)
# - recall (sensitivity): among actual positives, fraction predicted positive (TPR)
# - AUC: threshold-free ranking quality (area under ROC curve) using predicted probabilities
accuracy_tfidf  <- sum(pred_tfidf == y_test) / length(y_test)
precision_tfidf <- cm_tfidf$table[2, 2] / sum(cm_tfidf$table[, 2])  # TP / (TP + FP)
recall_tfidf    <- cm_tfidf$table[2, 2] / sum(cm_tfidf$table[2, ])  # TP / (TP + FN)
auc_tfidf       <- as.numeric(pROC::auc(y_test, p_tfidf))

# Nicely formatted summary of key metrics for the TF-IDF + Logit baseline.
cat(sprintf(
  "\nTF-IDF + Logit\nAccuracy: %.3f\nPrecision: %.3f\nRecall: %.3f\nAUC: %.3f\n",
  accuracy_tfidf, precision_tfidf, recall_tfidf, auc_tfidf
))

#plot roc curve using ggplot2
roc_tfidf <- roc(response = y_test, predictor = p_tfidf, quiet = TRUE)
tpr <- rev(roc_tfidf$sensitivities)
fpr <- rev(1 - roc_tfidf$specificities)
roc_data <- data.frame(fpr = fpr, tpr = tpr)
ggplot(roc_data, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve - TF-IDF + Logit",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()


# Inspect top TF-IDF features ----
coefs <- as.matrix(coef(mdl_tfidf))
coefs <- data.table(term = rownames(coefs), weight = as.numeric(coefs[,1]))[term != "(Intercept)"]
coefs[, odds_ratio := exp(weight)]
coefs[order(-odds_ratio)][, .(term, odds_ratio)][1:10]   # biggest positive ORs
coefs[order(odds_ratio)][, .(term, odds_ratio)][1:10]    # biggest negative ORs (OR < 1)


############################################################
## Sentiment classification with SIMPLE-AVERAGE Word2Vec  ##
############################################################

# ---------------- PRETRAINED WORD2VEC (GOOGLENEWS EXAMPLE) ----------------
# Point to your local pretrained binary model. If zipped, unzip first.
PRETRAINED_PATH <- "GoogleNews-vectors-negative300.bin"  # <- change if needed

# Load pretrained embeddings. GoogleNews is binary word2vec; set binary=TRUE.
# normalize=FALSE keeps original norms; we’ll L2-normalize document vectors later.
w2v_model <- word2vec::read.word2vec(PRETRAINED_PATH)

# Convert to a dense matrix:
#   rownames(w2v) = tokens, columns = embedding dimensions (e.g., 300)
w2v <- as.matrix(w2v_model)
emb_dim <- ncol(w2v)

# ---------------- ALIGN DTM TERMS WITH EMBEDDING VOCAB ----------------
# We will compute a SIMPLE average of word vectors per document using COUNT weights.
# Pretrained W2V usually does NOT contain bigrams with spaces; drop them now.
terms <- colnames(dtm_train)
keep_unigram <- !grepl("\\s", terms)
terms_uni <- terms[keep_unigram]

# Match DTM unigram columns to embedding rows once.
idx <- match(terms_uni, rownames(w2v))   # integer row index in w2v for each term; NA if OOV
has_vec <- !is.na(idx)

# Embedding submatrix aligned to the kept vocabulary subset (|V'| x dim).
E <- w2v[idx[has_vec], , drop = FALSE]

# Subset DTM columns to the same kept terms (counts, not tf-idf).
Xc_tr <- dtm_train[, which(keep_unigram)[has_vec], drop = FALSE]  # n_docs_train x |V'|
Xc_te <- dtm_test [, which(keep_unigram)[has_vec], drop = FALSE]  # n_docs_test  x |V'|

# ------------------- SIMPLE (UNWEIGHTED) AVERAGE DOC VECTORS -------------------
# For each document d:
#   doc_vec(d) = (counts(d) %*% E) / sum(counts(d))
# i.e., every in-vocabulary word contributes equally proportional to its count.

# Denominator = total in-vocab tokens per doc (avoid division by zero).
tot_tr <- Matrix::rowSums(Xc_tr)
tot_te <- Matrix::rowSums(Xc_te)

# Numerators for ALL docs at once: sparse–dense matrix multiply.
# Xc_* are sparse counts; E is dense embeddings.
Xtrain_avg <- as.matrix(Xc_tr %*% E)     # n_docs_train x emb_dim
Xtest_avg  <- as.matrix(Xc_te %*% E)     # n_docs_test  x emb_dim

# Divide each doc vector by its token count to get the average.
Xtrain_avg <- sweep(Xtrain_avg, 1, pmax(tot_tr, 1e-12), "/")
Xtest_avg  <- sweep(Xtest_avg,  1, pmax(tot_te, 1e-12), "/")

# Optional: L2-normalize each document vector (often helps linear models).
l2 <- function(M) sweep(M, 1, sqrt(rowSums(M^2)) + 1e-12, "/")
Xtrain_avg <- l2(Xtrain_avg)
Xtest_avg  <- l2(Xtest_avg)

# Some training docs may still be all zeros (e.g., no tokens overlapped the embedding vocab).
keep_rows <- rowSums(abs(Xtrain_avg)) > 0

# -------------------------- CLASSIFICATION (LOGIT) ---------------------------
# Cross-validated logistic regression on document vectors (simple, fast, strong baseline).
cv_avg <- cv.glmnet(
  Xtrain_avg[keep_rows, , drop = FALSE], y_train[keep_rows],
  family = "binomial",
  type.measure = "auc",
  nfolds = 5
)

# Refit at the best lambda from CV on the filtered train set.
mdl_avg <- glmnet(
  Xtrain_avg[keep_rows, , drop = FALSE], y_train[keep_rows],
  family = "binomial",
  lambda = cv_avg$lambda.min
)

# ------------------------------- EVALUATION --------------------------------
# Probabilities for the positive class ("pos"). Then threshold at 0.5 for hard labels.
p_avg <- as.numeric(predict(mdl_avg, Xtest_avg, type = "response"))
pred_avg <- factor(ifelse(p_avg >= 0.5, "pos", "neg"), levels = levels(y_test))

# Confusion matrix and metrics the same way as your TF-IDF baseline.
cm_avg <- yardstick::conf_mat(
  data.frame(truth = y_test, .pred_class = pred_avg),
  truth, .pred_class
)

accuracy_avg  <- mean(pred_avg == y_test)
precision_avg <- cm_avg$table[2, 2] / sum(cm_avg$table[, 2])  # TP / (TP + FP)
recall_avg    <- cm_avg$table[2, 2] / sum(cm_avg$table[2, ])  # TP / (TP + FN)
auc_avg       <- as.numeric(pROC::auc(y_test, p_avg))

cat(sprintf(
  "\nSimple-Average Word2Vec + Logit\nAccuracy: %.3f\nPrecision: %.3f\nRecall: %.3f\nAUC: %.3f\n",
  accuracy_avg, precision_avg, recall_avg, auc_avg
))

#plot roc curve using ggplot2
roc_avg <- roc(response = y_test, predictor = p_avg, quiet = TRUE)
tpr <- rev(roc_avg$sensitivities)
fpr <- rev(1 - roc_avg$specificities)
ggplot(data.frame(fpr = fpr, tpr = tpr), aes(x = fpr, y = tpr)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve - Simple-Average Word2Vec + Logit",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()

# # Inspect top w2v features ----
coefs_w2v <- as.matrix(coef(mdl_avg))
coefs_w2v <- data.table(dimension = rownames(coefs_w2v), weight = as.numeric(coefs_w2v[,1]))[dimension != "(Intercept)"]
coefs_w2v[, odds_ratio := exp(weight)]
coefs_w2v[order(-odds_ratio)][, .(dimension, odds_ratio)][1:10]   # biggest positive ORs
coefs_w2v[order(odds_ratio)][, .(dimension, odds_ratio)][1:10]    # biggest negative ORs (OR < 1)


