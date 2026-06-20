# ============================================================
# model_training.R
# Feature selection → Train/Test split → Train 3 models
# → Compare → Evaluate → Save best model
# Run AFTER data_cleaning.R and eda.R
# ============================================================

# ── 0. Packages ──────────────────────────────────────────────
required <- c("tidyverse", "caret", "randomForest", "xgboost",
              "pROC", "pdp", "tibble")
new_pkgs  <- required[!required %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)

library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(pROC)
library(pdp)
library(tibble)

# ── 1. Load cleaned data ─────────────────────────────────────
if (!file.exists("pcos_clean.rds")) stop("Run data_cleaning.R first!")
df <- readRDS("pcos_clean.rds")
cat("Data loaded:", nrow(df), "rows x", ncol(df), "cols\n")

# ── 2. Feature Selection ─────────────────────────────────────
# Load correlation matrix from eda.R (or recompute)
if (file.exists("cor_matrix.rds")) {
  cor_mat <- readRDS("cor_matrix.rds")
} else {
  num_df  <- df |> select(where(is.numeric))
  num_df  <- num_df[, apply(num_df, 2, var, na.rm = TRUE) != 0]
  cor_mat <- cor(num_df, use = "complete.obs")
}

# Remove highly correlated features (> 0.9)
highly_correlated <- findCorrelation(cor_mat, cutoff = 0.9)
numeric_data      <- df |> select(where(is.numeric))
numeric_data      <- numeric_data[, apply(numeric_data,
                                          2, var, na.rm = TRUE) != 0]

if (length(highly_correlated) > 0) {
  cat("Removing", length(highly_correlated), "highly correlated features.\n")
  numeric_data <- numeric_data[, -highly_correlated]
}

# Build final modelling dataset
factor_data           <- df |> select(where(is.factor)) |> select(-PCOS_Y_N)
final_data            <- bind_cols(numeric_data, factor_data)
final_data$PCOS_Y_N  <- df$PCOS_Y_N

cat("Final dataset:", nrow(final_data), "rows x", ncol(final_data), "cols\n")

# ── 3. Train / Test Split (70 / 30) ──────────────────────────
set.seed(123)
train_idx  <- createDataPartition(final_data$PCOS_Y_N, p = 0.7, list = FALSE)
train_data <- final_data[train_idx, ]
test_data  <- final_data[-train_idx, ]

cat("Train:", nrow(train_data), " | Test:", nrow(test_data), "\n")

# Shared cross-validation control
ctrl <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# ── 4a. Logistic Regression ───────────────────────────────────
cat("\nTraining Logistic Regression...\n")
logistic_model <- train(
  PCOS_Y_N ~ .,
  data      = train_data,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl,
  metric    = "ROC"
)
cat("Logistic CV ROC:", round(max(logistic_model$results$ROC), 4), "\n")

# ── 4b. Random Forest ────────────────────────────────────────
cat("Training Random Forest...\n")
rf_model <- train(
  PCOS_Y_N ~ .,
  data       = train_data,
  method     = "rf",
  trControl  = ctrl,
  ntree      = 200,
  importance = TRUE,
  metric     = "ROC"
)
cat("RF CV ROC:", round(max(rf_model$results$ROC), 4), "\n")

# ── 4c. XGBoost ──────────────────────────────────────────────
cat("Training XGBoost...\n")
xgb_model <- train(
  PCOS_Y_N ~ .,
  data      = train_data,
  method    = "xgbTree",
  trControl = ctrl,
  verbose   = FALSE,
  metric    = "ROC"
)
cat("XGB CV ROC:", round(max(xgb_model$results$ROC), 4), "\n")

# ── 5. Compare Models ─────────────────────────────────────────
models  <- list(Logistic = logistic_model, RandomForest = rf_model,
                XGBoost = xgb_model)
results <- resamples(models)
cat("\nModel Comparison:\n")
print(summary(results))
dotplot(results, main = "Model Comparison — ROC / Sensitivity / Specificity")

# ── 6. Evaluate Best Model (Random Forest) on Test Set ───────
cat("\nEvaluating Random Forest on test data...\n")
predictions      <- predict(rf_model, newdata = test_data)
predictions_prob <- predict(rf_model, newdata = test_data, type = "prob")
conf_matrix      <- confusionMatrix(predictions, test_data$PCOS_Y_N,
                                    positive = "PCOS")
print(conf_matrix)

# ── 7. ROC Curve & AUC ───────────────────────────────────────
roc_curve <- roc(
  response  = as.numeric(test_data$PCOS_Y_N == "PCOS"),
  predictor = predictions_prob$PCOS
)
auc_value <- auc(roc_curve)
cat("Test AUC:", round(as.numeric(auc_value), 4), "\n")

plot(roc_curve,
     main = paste0("ROC Curve — Random Forest 
     (AUC = ", round(auc_value, 3), ")"),
     col  = "steelblue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "grey60")

# ── 8. Feature Importance ────────────────────────────────────
importance_obj <- varImp(rf_model)
plot(importance_obj, top = 20, main = "Top 20 Feature Importances")

top_features <- importance_obj$importance |>
  as.data.frame() |>
  rownames_to_column("Feature") |>
  arrange(desc(Overall)) |>
  head(10)

cat("\nTop 10 Features:\n")
print(top_features)

ggplot(top_features, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Features for PCOS Prediction",
       x = "Feature", y = "Importance Score") +
  theme_minimal()

# ── 9. Partial Dependence Plots (top 3 numeric features) ─────
top_3 <- top_features$Feature[
  top_features$Feature %in% names(train_data)[sapply(train_data, is.numeric)]
][1:3]

for (feat in top_3) {
  if (!is.na(feat)) {
    pd <- partial(rf_model, pred.var = feat, train = train_data, prob = TRUE)
    plot(pd, main = paste("Partial Dependence —", feat),
         xlab = feat, ylab = "P(PCOS)")
  }
}

# ── 10. Performance Summary Table ────────────────────────────
perf <- data.frame(
  Model       = c("Logistic Regression", "Random Forest", "XGBoost"),
  CV_ROC      = round(c(max(logistic_model$results$ROC),
                        max(rf_model$results$ROC),
                        max(xgb_model$results$ROC)), 4),
  Test_AUC    = c(NA, round(as.numeric(auc_value), 4), NA),
  Accuracy    = c(NA, round(conf_matrix$overall["Accuracy"], 4), NA),
  Sensitivity = c(NA, round(conf_matrix$byClass["Sensitivity"], 4), NA),
  Specificity = c(NA, round(conf_matrix$byClass["Specificity"], 4), NA)
)
cat("\nPerformance Summary:\n")
print(perf)

# ── 11. Save everything needed by prediction.R ───────────────
saveRDS(rf_model,    "pcos_prediction_model.rds")
saveRDS(train_data,  "train_data.rds")
saveRDS(test_data,   "test_data.rds")
saveRDS(top_features, "top_features.rds")

# Export test predictions CSV
test_preds_df <- data.frame(
  Actual              = test_data$PCOS_Y_N,
  Predicted           = predictions,
  Probability_PCOS    = round(predictions_prob$PCOS, 4),
  Probability_No_PCOS = round(predictions_prob$No_PCOS, 4),
  Correct             = predictions == test_data$PCOS_Y_N
)
write.csv(test_preds_df, "test_predictions.csv", row.names = FALSE)

cat("\nSaved: pcos_prediction_model.rds\n")
cat("Saved: train_data.rds\n")
cat("Saved: test_data.rds\n")
cat("Saved: test_predictions.csv\n")

cat("\n========================================\n")
cat("  Model Training Complete\n")
cat("  Best Model : Random Forest\n")
cat("  Accuracy   :", round(conf_matrix$overall["Accuracy"] * 100, 2), "%\n")
cat("  AUC        :", round(as.numeric(auc_value), 3), "\n")
cat("========================================\n")