set.seed(123)

# ---- Packages ----
# install.packages(c("data.table","sentimentr","text2vec","glmnet","Matrix","pROC","yardstick","stopwords"))
library(data.table)
library(sentimentr)   # lexicon-based sentiment scores
library(text2vec)     # tokenization, DTM/TF-IDF
library(glmnet)       # sparse logistic regression
library(Matrix)
library(pROC)         # ROC/AUC
library(yardstick)    # confusion matrix & metrics
library(stopwords)


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
dt = dt[, .(id, review, target)]

# ===================================================================
# TF-IDF + Logistic (glmnet) with flexible knobs for experimentation
# ===================================================================

run_tfidf_experiment <- function(
    dt,                                  # data.table with columns: id, review, target (factor c("neg","pos"))
    ngram_min = 1,                      # minimum n in n-grams (1 = unigrams)
    ngram_max = 1,                      # maximum n in n-grams (2 = include bigrams); set =1L for unigrams-only
    remove_rare = FALSE,                  # prune rare terms?
    term_count_min = 0,                  # min corpus count for a term to be kept (if remove_rare = TRUE)
    remove_popular = FALSE,               # prune overly common terms?
    doc_proportion_max = 1,            # max fraction of documents a term may appear in (if remove_popular = TRUE)
    l2_norm = FALSE,                      # L2-normalize TF-IDF rows? (recommended for linear models)
    use_cv = FALSE,                       # use cross-validation to choose lambda?
    alpha = 0.5,                           # glmnet mixing: 1=Lasso (some coefficient are set exactly to zero), 0=Ridge (shrink coefficients towards zero), (0..1)=Elastic Net
    nfolds = 5,                          # number of folds if use_cv = TRUE
    lambda_no_cv = 0,                    # lambda value when use_cv = FALSE,  (Small λ → weak penalty → low bias, higher variance (risk of overfitting).
                                         #Large λ → strong penalty → higher bias, lower variance (simpler model).)
    keep_words_only = FALSE               # drop tokens containing any digits (keep only alphabetic words)
) {
  # --------- Basic input checks (fail fast with helpful messages) ----------
  stopifnot(all(c("id","review","target") %in% names(dt)))
  stopifnot(is.factor(dt$target) && all(levels(dt$target) %in% c("neg","pos")))
  
  # --------- Train/Test split (80/20) -------------------------------------
  # set.seed inside the function makes runs reproducible in class demos
  set.seed(123)
  idx <- sample.int(nrow(dt), floor(0.8 * nrow(dt)))
  train <- dt[idx]
  test  <- dt[-idx]
  y_train <- train$target
  y_test  <- test$target
  
  # --------- Tokenization & Vocabulary ------------------------------------
  # We lowercase text to unify variants like "Great" vs "great".
  prep_fun <- tolower
  
  # Tokenizer:
  # - If keep_words_only = TRUE, we remove any token that contains a digit.
  # - Otherwise, use text2vec's standard word_tokenizer.
  if (keep_words_only) {
    tok_fun <- function(x) {
      toks_list <- text2vec::word_tokenizer(x)           # list(list-of-tokens per doc)
      lapply(toks_list, function(doc) doc[!grepl("[0-9]", doc)])  # drop tokens with digits
    }
  } else {
    tok_fun <- text2vec::word_tokenizer
  }
  
  # Build an iterator over TRAIN reviews (no leakage from test).
  it_train <- text2vec::itoken(
    prep_fun(train$review),
    tokenizer   = tok_fun,
    ids         = train$id,
    progressbar = TRUE
  )
  
  # Create a vocabulary over chosen n-gram range.
  # ngram_min=1L, ngram_max=1L => unigrams only
  # ngram_min=1L, ngram_max=2L => unigrams + bigrams (captures "not good", etc.)
  vocab <- text2vec::create_vocabulary(
    it_train,
    ngram = c(as.integer(ngram_min), as.integer(ngram_max))
  )
  
  # Safety net: ensure no numeric-containing terms remain in the vocabulary
  # (covers n-grams that might still include digits).
  if (keep_words_only) {
    vocab$vocab <- vocab$vocab[!grepl("[0-9]", vocab$vocab$term), ]
  }
  
  # Pruning configuration:
  # - If remove_rare = FALSE, we effectively keep all terms by setting term_count_min = 1
  # - If remove_popular = FALSE, allow terms to appear in up to 100% of docs
  if (!remove_rare)        term_count_min <- 1
  if (!remove_popular)     doc_proportion_max <- 1.0
  
  vocab <- text2vec::prune_vocabulary(
    vocab,
    term_count_min     = term_count_min,
    doc_proportion_max = doc_proportion_max
  )
  
  # Vectorizer maps tokens -> columns (feature indices)
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  
  # Sparse Document-Term Matrix for TRAIN
  dtm_train <- text2vec::create_dtm(it_train, vectorizer)
  
  # Build the TEST iterator with the same preprocessing & tokenizer
  it_test <- text2vec::itoken(
    prep_fun(test$review),
    tokenizer   = tok_fun,
    ids         = test$id,
    progressbar = TRUE
  )
  # Project TEST docs into the same feature space (same columns as TRAIN)
  dtm_test <- text2vec::create_dtm(it_test, vectorizer)
  
  # --------- TF-IDF transformation ----------------------------------------
  # text2vec::TfIdf can L2-normalize rows (norm="l2") which often helps linear models
  norm_opt <- if (l2_norm) "l2" else "none"
  tfidf <- text2vec::TfIdf$new(norm = norm_opt)
  
  # Fit IDF on TRAIN only, then transform both TRAIN and TEST
  X_train <- tfidf$fit_transform(dtm_train)
  X_test  <- tfidf$transform(dtm_test)
  
  # Ensure inputs remain sparse dgCMatrix (glmnet handles these efficiently)
  # Avoid as.matrix() here (would densify!)
  stopifnot(inherits(X_train, "dgCMatrix"), inherits(X_test, "dgCMatrix"))
  
  # --------- Classifier: Logistic Regression via glmnet --------------------
  # Regularization helps with very high-dimensional TF-IDF.
  # alpha=1 (Lasso) tends to produce sparse solutions; alpha=0 (Ridge) can be more stable.
  if (use_cv) {
    # Cross-validate to choose lambda by AUC
    cvfit <- glmnet::cv.glmnet(
      X_train, y_train,
      family = "binomial",
      type.measure = "auc",
      nfolds = nfolds,
      alpha = alpha
    )
    # Refit at the best lambda from CV
    mdl <- glmnet::glmnet(
      X_train, y_train,
      family = "binomial",
      lambda = cvfit$lambda.min,
      alpha = alpha
    )
    used_lambda <- cvfit$lambda.min
    cv_obj <- cvfit
  } else {
    # Fixed lambda path (useful to demonstrate the role of regularization)
    mdl <- glmnet::glmnet(
      X_train, y_train,
      family = "binomial",
      lambda = lambda_no_cv,
      alpha = alpha
    )
    used_lambda <- lambda_no_cv
    cv_obj <- NULL
  }
  
  # --------- Evaluation on TEST -------------------------------------------
  # Predict probabilities for the positive class ("pos")
  p <- as.numeric(predict(mdl, X_test, type = "response"))
  
  # Convert to hard labels using a 0.5 threshold (simple, tunable)
  pred <- factor(ifelse(p >= 0.5, "pos", "neg"), levels = levels(y_test))
  
  # Confusion matrix (yardstick expects a data.frame with columns truth, .pred_class)
  cm <- yardstick::conf_mat(
    data.frame(truth = y_test, .pred_class = pred),
    truth, .pred_class
  )
  
  # ROC/AUC with explicit class order (neg < pos) to avoid direction issues
  roc_obj <- pROC::roc(
    response  = y_test,
    predictor = p,
    levels    = c("neg","pos"),
    direction = "<",
    quiet     = TRUE
  )
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  # Basic metrics
  acc  <- mean(pred == y_test)
  prec <- cm$table[2,2] / sum(cm$table[,2])   # TP / (TP + FP)
  rec  <- cm$table[2,2] / sum(cm$table[2,])   # TP / (TP + FN)
  
  # --------- Return a compact bundle of results & artifacts ----------------
  list(
    metrics = list(
      AUC = auc_val,
      Accuracy = acc,
      Precision = prec,
      Recall = rec,
      lambda = used_lambda,
      alpha = alpha,
      l2_norm = l2_norm,
      ngram = c(ngram_min, ngram_max),
      term_count_min = term_count_min,
      doc_proportion_max = doc_proportion_max,
      keep_words_only = keep_words_only,
      use_cv = use_cv,
      nfolds = if (use_cv) nfolds else NA
    ),
    model = mdl,             # fitted glmnet model
    cv = cv_obj,             # cv.glmnet object (or NULL if use_cv=FALSE)
    vectorizer = vectorizer, # to transform new text the same way
    tfidf = tfidf,           # fitted TF-IDF transformer
    dtm_train = dtm_train,   # raw DTMs (sparse)
    dtm_test = dtm_test,
    X_train = X_train,       # TF-IDF matrices (sparse)
    X_test = X_test,
    y_train = y_train,       # labels
    y_test = y_test,
    vocab = vocab,           # vocabulary object (with stats)
    roc = roc_obj,           # ROC curve object (for plotting)
    confusion_matrix = cm    # yardstick cm object
  )
}

# =========================
# Example
# =========================

res_bad <- run_tfidf_experiment(
  dt
)
print(res_bad$metrics)

res_bad <- run_tfidf_experiment(
  dt,
  ngram_min = 1, ngram_max = 1,   # unigrams only
  remove_rare = FALSE,              # keep rare terms
  remove_popular = FALSE,           # keep very common terms
  l2_norm = FALSE,                  # no L2 normalization
  use_cv = FALSE,                   # no CV tuning
  lambda_no_cv = 0,              # 0 lambda = no regularization
  alpha = 1,                        # Lasso path, but with tiny lambda it won’t shrink
  keep_words_only = FALSE           # keep numbers too (adds noise)
)
print(res_bad$metrics)

# 1) Unigrams ONLY, keep only words (no digits), standard pruning, L2 on, with CV (Lasso)
#    → Baseline with good generalization and interpretability.
res1 <- run_tfidf_experiment(
  dt,
  ngram_min = 1L, ngram_max = 1L,           # unigrams only
  remove_rare = TRUE, term_count_min = 5,   # drop very rare terms
  remove_popular = TRUE, doc_proportion_max = 0.5,  # drop very common terms
  l2_norm = TRUE,                           # L2-normalize TF-IDF rows
  use_cv = TRUE, alpha = 1, nfolds = 5,     # Lasso with 5-fold CV
  keep_words_only = TRUE                    # drop tokens with digits
)
print(res1$metrics)

# 2) Unigrams + bigrams, looser pruning, Elastic Net (alpha=0.5), still words-only
#    → Often improves sentiment by capturing phrases like "not good".
res2 <- run_tfidf_experiment(
  dt,
  ngram_min = 1L, ngram_max = 2L,           # add bigrams
  remove_rare = TRUE, term_count_min = 5,
  remove_popular = TRUE, doc_proportion_max = 0.9,  # keep more common terms
  l2_norm = TRUE,
  use_cv = TRUE, alpha = 0.5, nfolds = 5,   # Elastic Net
  keep_words_only = TRUE
)
print(res2$metrics)

# 3) Unigrams ONLY, no pruning, NO CV (fixed lambda), still words-only
#    → Demonstrates the effect of regularization/normalization choices.
res3 <- run_tfidf_experiment(
  dt,
  ngram_min = 1L, ngram_max = 1L,
  remove_rare = FALSE,                      # keep all terms (can increase noise)
  remove_popular = FALSE,                   # keep very common terms
  l2_norm = FALSE,                          # turn off row L2 norm to compare impact
  use_cv = FALSE, lambda_no_cv = 0.001,     # fixed lambda (teaching only)
  alpha = 1,
  keep_words_only = TRUE
)
print(res3$metrics)


# =========================
# Grid search for best AUC
# =========================

# --- Define a manageable grid (expand later if you want) ---
# Notes:
# - We use CV (use_cv=TRUE) so lambda is chosen automatically; alpha is varied.
# - We include both unigram and unigram+bigram settings.
# - Minimal pruning knobs to keep the grid small (you can add more).
param_grid <- CJ(
  ngram_min        = 1L,
  ngram_max        = c(1L, 2L),          # 1=unigrams, 2=uni+bi
  remove_rare      = c(FALSE, TRUE),
  term_count_min   = c(1L, 5L),          # only used when remove_rare=TRUE
  remove_popular   = FALSE,              # keep common terms (simplify first pass)
  doc_proportion_max = 1.0,              # ignored when remove_popular=FALSE
  l2_norm          = c(FALSE, TRUE),
  use_cv           = TRUE,               # let CV pick lambda
  alpha            = c(0, 0.5, 1),       # Ridge / Elastic Net / Lasso
  nfolds           = 5L,
  lambda_no_cv     = 0,                  # ignored when use_cv=TRUE
  keep_words_only  = c(FALSE, TRUE),
  unique = TRUE
)

# Optional: if remove_rare==FALSE, force term_count_min to 1
param_grid[remove_rare == FALSE, term_count_min := 1L]

# --- Run all combos ---
run_one <- function(row) {
  args <- as.list(row)
  # Prepend the required 'dt' argument
  args <- c(list(dt = dt), args)
  out <- do.call(run_tfidf_experiment, args)
  # Return a tidy one-row data.table with params + metrics
  as.data.table(c(out$metrics))
}

# Apply over each row of the grid
results_list <- lapply(split(param_grid, seq_len(nrow(param_grid))), function(pg) run_one(pg))
results_dt <- rbindlist(results_list, use.names = TRUE, fill = TRUE)

# Merge metrics back with their parameter rows for easy inspection
full_dt <- cbind(param_grid, results_dt)

# --- Pick the best by AUC ---
setorder(full_dt, -AUC)
best <- full_dt[1]

cat("\nBest configuration by AUC:\n")
print(best)

# --- (Optional) Refit once with best params to keep artifacts (model, vectorizer, ROC, etc.) ---
best_args <- as.list(best[, .(
  ngram_min, ngram_max, remove_rare, term_count_min, remove_popular, doc_proportion_max,
  l2_norm, use_cv, alpha, nfolds, lambda_no_cv, keep_words_only
)])
best_fit <- do.call(run_tfidf_experiment, c(list(dt = dt), best_args))

cat("\nBest AUC:", round(best_fit$metrics$AUC, 4),
    "| Accuracy:", round(best_fit$metrics$Accuracy, 4),
    "| Precision:", round(best_fit$metrics$Precision, 4),
    "| Recall:", round(best_fit$metrics$Recall, 4), "\n")

# (Optional) quick ROC plot
# plot(best_fit$roc, main = sprintf("ROC (AUC = %.3f)", best_fit$metrics$AUC))


