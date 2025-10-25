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
    dt,                                 # data.table with columns: id, review, target (factor c("neg","pos"))
    ngram_min = 1,                      # minimum n in n-grams (1 = unigrams)
    ngram_max = 1,                      # maximum n in n-grams (2 = include bigrams); set =1L for unigrams-only
    remove_rare = FALSE,                # prune rare terms?
    term_count_min = 0,                 # min corpus count for a term to be kept (if remove_rare = TRUE)
    remove_popular = FALSE,             # prune overly common terms?
    doc_proportion_max = 1,             # max fraction of documents a term may appear in (if remove_popular = TRUE)
    l2_norm = FALSE,                    # L2-normalize TF-IDF rows? (recommended for linear models)
    use_cv = FALSE,                     # use cross-validation to choose lambda?
    alpha = 0.5,                        # glmnet mixing: 1=Lasso (some coefficient are set exactly to zero), 0=Ridge (shrink coefficients towards zero), (0..1)=Elastic Net
    nfolds = 5,                         # number of folds if use_cv = TRUE
    lambda_no_cv = 0,                   # lambda value when use_cv = FALSE,  (Small λ → weak penalty → low bias, higher variance (risk of overfitting).
                                        #Large λ → strong penalty → higher bias, lower variance (simpler model).)
    keep_words_only = FALSE             # drop tokens containing any digits (keep only alphabetic words)
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

# run default model
res_bad <- run_tfidf_experiment(
  dt
)
print(res_bad$metrics)

# change alpha
res_bad <- run_tfidf_experiment(
  dt,
  ngram_min = 1, ngram_max = 1,   # unigrams only
  remove_rare = FALSE,              # keep rare terms
  remove_popular = FALSE,           # keep very common terms
  l2_norm = FALSE,                  # no L2 normalization
  use_cv = FALSE,                   # no CV tuning
  lambda_no_cv = 0,                 # 0 lambda = no regularization
  alpha = 1,                        # Lasso path, but with 0 lambda it won’t shrink
  keep_words_only = FALSE           # keep numbers too (adds noise)
)
print(res_bad$metrics)
