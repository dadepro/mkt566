# ---- Setup ----

#setwd as script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pkg <- c("ggplot2", "dplyr")
to_install <- pkg[!pkg %in% installed.packages()[,1]]
if (length(to_install) > 0) install.packages(to_install, quiet = TRUE)
invisible(lapply(pkg, library, character.only = TRUE))

# ---- 1) Confusion matrix -> metrics (churn example) ----
TP <- 50; FP <- 20; FN <- 10; TN <- 120
accuracy  <- (TP + TN) / (TP + FP + FN + TN)
precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)

metrics <- tibble::tibble(
  Metric = c("Accuracy","Precision","Recall"),
  Value  = c(accuracy, precision, recall)
)
print(metrics)

# ---- 2) Precision–Recall trade-off plot (conceptual points) ----
tradeoff <- tibble::tibble(
  recall    = c(0.20, 0.40, 0.60, 0.80, 0.90),
  precision = c(0.95, 0.90, 0.85, 0.70, 0.60)
)

p <- ggplot(tradeoff, aes(x = recall, y = precision)) +
  geom_line() +
  geom_point(size = 3) +
  # annotations
  annotate(
    "text",
    x = 0.15, y = 0.83, hjust = 0,
    label = "High precision, low recall\n= few wasted offers,\nmiss many churners"
  ) +
  annotate(
    "segment",
    x = 0.20, xend = 0.24, y = 0.95, yend = 0.9,
    arrow = arrow(length = unit(0.15, "cm"))
  ) +
  annotate(
    "text",
    x = 0.58, y = 0.43, hjust = 0,
    label = "High recall, low precision\n= catch most churners,\nwaste many offers"
  ) +
  annotate(
    "segment",
    x = 0.9, xend = 0.75, y = 0.6, yend = 0.5,
    arrow = arrow(length = unit(0.15, "cm"))
  ) +
  labs(
    title = "Precision-Recall Trade-off in Churn Prediction",
    x = "Recall (catch churners)",
    y = "Precision (offers not wasted)"
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0.4, 1.05)) +
  theme_minimal(base_size = 13)

print(p)
ggsave("figures/precision_recall_tradeoff.pdf", p, width = 8, height = 6)
 