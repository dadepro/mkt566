# ---- Generate Synthetic Customer Dataset for Clustering ----
set.seed(123)

n <- 200

# create three "true" customer segments
seg1 <- data.frame(
  Age          = round(rnorm(n/3, mean = 25, sd = 4)),
  Income       = round(rnorm(n/3, mean = 35000, sd = 5000)),
  Online_Spend = round(rnorm(n/3, mean = 1200, sd = 300)),
  InStore_Spend= round(rnorm(n/3, mean = 300, sd = 100))
)

seg2 <- data.frame(
  Age          = round(rnorm(n/3, mean = 45, sd = 6)),
  Income       = round(rnorm(n/3, mean = 60000, sd = 8000)),
  Online_Spend = round(rnorm(n/3, mean = 400, sd = 150)),
  InStore_Spend= round(rnorm(n/3, mean = 2400, sd = 500))
)

seg3 <- data.frame(
  Age          = round(rnorm(n/3, mean = 35, sd = 5)),
  Income       = round(rnorm(n/3, mean = 80000, sd = 12000)),
  Online_Spend = round(rnorm(n/3, mean = 2000, sd = 600)),
  InStore_Spend= round(rnorm(n/3, mean = 2000, sd = 600))
)

# combine
df <- rbind(seg1, seg2, seg3)
df$CustomerID <- 1:nrow(df)

# reorder columns
df <- df[, c("CustomerID","Age","Income","Online_Spend","InStore_Spend")]

# save to CSV for students
write.csv(df, "customer_clustering_data.csv", row.names = FALSE)

head(df)
summary(df)

# what students will see
# 
# one cluster ≈ “young, low income, online-first”
# 
# one cluster ≈ “middle-aged, mid income, omni-channel”
# 
# one cluster ≈ “older, higher spend in-store”



