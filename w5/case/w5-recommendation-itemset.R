## ---------------------------------------------
## Assoc. Rule Mining (from scratch) – R script
## ---------------------------------------------
rm(list = ls())  # clear workspace

library(data.table)
#setwd as the directory where this file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

set.seed(42)

## 1) Simulate catalog
cats <- c("Electronics","Home","Beauty","Sports")
items <- data.table(
  item_id = 1:12,
  name = c("Laptop","Mouse","Headphones","Charger",     # Electronics
           "Vacuum","Mop","Detergent",                  # Home
           "Shampoo","Face Cream","Toothpaste",         # Beauty
           "Tennis Racket","Tennis Balls"),             # Sports
  category = c(rep("Electronics",4), rep("Home",3),
               rep("Beauty",3), rep("Sports",2))
)

## 2) Simulate transactions with some co-purchase structure
n_tx <- 120
tx <- rbindlist(lapply(1:n_tx, function(tid){
  # choose a primary category for the basket
  cat <- base::sample(cats, 1)
  # basket size (mostly 2-4 items)
  k <- pmin(6, pmax(1, rpois(1, lambda = 3)))
  # bias toward items in the chosen category
  in_cat <- items[category == cat, item_id]
  out_cat <- items[category != cat, item_id]
  pick <- c(
    base::sample(in_cat, size = min(length(in_cat), base::sample(1:3,1)), replace = FALSE),
    base::sample(out_cat, size = max(0, k - 2), replace = FALSE)
  )
  # add a couple of realistic affinities
  if (all(c(11,12) %in% items$item_id) && 11 %in% pick && !(12 %in% pick) && runif(1)<0.7) pick <- c(pick, 12) # Racket→Balls
  if (1 %in% pick && !(4 %in% pick) && runif(1)<0.5) pick <- c(pick, 4)  # Laptop→Charger
  data.table(tid = tid, item_id = unique(pick))
}))

## 3) Transaction–item binary matrix
ui <- dcast(tx[, .(val = 1L), by = .(tid, item_id)], tid ~ item_id,
            value.var = "val", fill = 0)
M <- as.matrix(ui[ , -1])  # drop tid
colnames(M) <- items$name[match(colnames(M), items$item_id)]
n_transactions <- nrow(M)

## 4) Counts & metrics using cross-products
co_counts <- t(M) %*% M                 # item-item co-occurrence counts
diag_counts <- diag(co_counts)          # item supports as counts

# Build rules A->B for all ordered pairs A != B
make_rules <- function(min_support = 0.02, min_conf = 0.10, min_lift = 1.0){
  n <- ncol(M); item_names <- colnames(M)
  rules <- list()
  idx <- 1
  for(i in seq_len(n)){
    for(j in seq_len(n)){
      if (i == j) next
      cAB <- co_counts[i, j]
      if (cAB == 0) next
      supAB <- cAB / n_transactions
      supA  <- diag_counts[i] / n_transactions
      supB  <- diag_counts[j] / n_transactions
      conf  <- if (diag_counts[i] > 0) cAB / diag_counts[i] else NA_real_
      lift  <- if (supB > 0) conf / supB else NA_real_
      if (!is.na(conf) && !is.na(lift) &&
          supAB >= min_support && conf >= min_conf && lift >= min_lift){
        rules[[idx]] <- list(
          antecedent = item_names[i],
          consequent = item_names[j],
          support = round(supAB, 4),
          confidence = round(conf, 4),
          lift = round(lift, 4)
        )
        idx <- idx + 1
      }
    }
  }
  if (length(rules) == 0) return(data.table())
  rbindlist(rules)[order(-lift, -confidence, -support)]
}

rules <- make_rules(min_support = 0.03, min_conf = 0.2, min_lift = 1.05)
print(head(rules, 10))

## 5) Use rules to recommend for a basket
recommend <- function(basket_names, rules_dt, top_n = 5){
  basket <- unique(basket_names)
  
  # 1) Keep only rules whose antecedent is in the basket,
  #    and don’t recommend things already in the basket
  rel <- rules_dt[antecedent %in% basket & !consequent %in% basket]
  
  # 2) If nothing matches, return empty result
  if (nrow(rel) == 0) return(data.table(consequent=character(), score=numeric()))
  
  # 3) Score each candidate consequent
  #    score = confidence * lift  (strong + non-trivial associations)
  rel[, score := confidence*lift]
  
  # 4) Aggregate evidence: if multiple basket items point to the same consequent,
  #    sum their scores, then rank and take top N
  rel[, .(score = sum(score)), by = consequent][order(-score)][1:min(.N, top_n)]
}

# Example: recommendations for a basket with "Laptop" and "Tennis Racket"
example_recs <- recommend(c("Laptop","Tennis Racket"), rules, top_n = 5)
print(example_recs)

## 6) Nice little summary for class
cat("\nTransactions:", n_transactions,
    "\nUnique items:", ncol(M),
    "\nRules mined:", nrow(rules), "\n")


