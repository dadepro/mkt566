# -------------------------------
# Assoc. Rules on your dataset
# -------------------------------
library(data.table)
#setwd as the directory where this file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read the market basket data:
dt <- fread("../data/w5-market-basket.csv.gz")

# 1) Basic cleaning – keep actual purchases only
dt[, BillNo := as.character(BillNo)]
dt[, Itemname := toupper(trimws(Itemname))]
dt <- dt[Quantity > 0 & !is.na(Itemname) & Itemname != ""]


# 2) Build baskets: unique (BillNo, Itemname)
baskets <- unique(dt[, .(tid = BillNo, Itemname)])

# (Optional) keep only items with minimal support to keep the matrix reasonable
min_item_support <- 0.005  # 0.5% of transactions
n_tx <- uniqueN(baskets$tid)
item_counts <- baskets[, .N, by = Itemname]
keep_items <- item_counts[N >= ceiling(min_item_support * n_tx), Itemname]
baskets <- baskets[Itemname %in% keep_items]

# 3) Transaction–item binary matrix
ui <- dcast(baskets[, .(val = 1L), by = .(tid, Itemname)],
            tid ~ Itemname, value.var = "val", fill = 0)
M <- as.matrix(ui[, -1, with = FALSE])          # drop tid
item_names <- colnames(M)
n_trx <- nrow(M)

# 4) Co-occurrence counts and rule metrics
co_counts  <- t(M) %*% M                         # item x item counts
diag_counts <- diag(co_counts)                   # per-item counts

nz <- which(co_counts > 0, arr.ind = TRUE)
nz <- nz[nz[,1] != nz[,2], , drop = FALSE]       # exclude diagonal (A != B)

A_idx <- nz[,1]; B_idx <- nz[,2]
cAB   <- as.numeric(co_counts[nz])
cA    <- as.numeric(diag_counts[A_idx])
cB    <- as.numeric(diag_counts[B_idx])

supportAB <- cAB / n_trx
confidence <- cAB / pmax(cA, 1)
supportB   <- cB  / n_trx
lift       <- confidence / pmax(supportB, .Machine$double.eps)

rules <- data.table(
  antecedent = item_names[A_idx],
  consequent = item_names[B_idx],
  support = supportAB,
  confidence = confidence,
  lift = lift
)[support >= 0.003 & confidence >= 0.15 & lift >= 1.05][  # tweak thresholds as needed
  order(-lift, -confidence, -support)
]

cat("Transactions:", n_trx,
    "\nItems (after filter):", ncol(M),
    "\nRules mined:", nrow(rules), "\n\n")
print(head(rules, 10))

# 5) Recommendation function (students can implement/extend this)
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
  rel[, score := confidence]
  
  # 4) Aggregate evidence: if multiple basket items point to the same consequent,
  #    sum their scores, then rank and take top N
  rel[, .(score = sum(score)), by = consequent][order(-score)][1:min(.N, top_n)]
}

# Example: try two items from your catalog
# (Use exact names as they appear in Itemname after toupper/trimws)
example_basket <- c("WHITE HANGING HEART T-LIGHT HOLDER", "WHITE METAL LANTERN")
print(recommend(example_basket, rules, top_n = 8))
