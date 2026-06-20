# ============================================================
# PCOS PREDICTION MODEL — FIXED & COMPLETE SCRIPT
# ============================================================

# ============================================================
# STEP 0 — AUTO-INSTALL MISSING PACKAGES
# ============================================================
required_packages <- c(
  "tidyverse",    # ggplot2, dplyr, tidyr, readr, etc.
  "caret",        # model training & cross-validation
  "randomForest", # Random Forest algorithm
  "xgboost",      # XGBoost algorithm
  "pROC",         # ROC curves & AUC
  "corrplot",     # correlation matrix visualisation
  "pdp",          # partial dependence plots
  "tibble",       # rownames_to_column
  "scales"        # percent labels on ggplot axes
)

missing_packages <- required_packages[
  !required_packages %in% installed.packages()[, "Package"]
]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:",
      paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, dependencies = TRUE)
} else {
  cat("All required packages are already installed.\n")
}

# ============================================================
# STEP 1 — LOAD LIBRARIES
# ============================================================
library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(pROC)
library(corrplot)
library(pdp)

cat("All libraries loaded successfully.\n")

# ============================================================
# STEP 2 — SET DATA PATH  *** UPDATE THIS LINE ***
# ============================================================
data_path <- "E:/projects/PCOS-Prediction-R/data/pcos_dataset.csv"

# ============================================================
# STEP 3 — LOAD AND EXPLORE DATA
# ============================================================
pcos_dataset <- read.csv(data_path, stringsAsFactors = TRUE)

cat("Dataset dimensions:", dim(pcos_dataset), "\n")
cat("First few rows:\n")
print(head(pcos_dataset))
str(pcos_dataset)
summary(pcos_dataset)

# ============================================================
# STEP 4 — DATA CLEANING AND PREPROCESSING
# ============================================================
pcos_dataset_clean <- pcos_dataset

# Convert target to factor with clear labels
pcos_dataset_clean$PCOS_Y_N <- as.factor(pcos_dataset_clean$PCOS_Y_N)
levels(pcos_dataset_clean$PCOS_Y_N) <- c("No_PCOS", "PCOS")

# Remove identifier columns safely if they exist
cols_to_remove <- intersect(
  c("Sl..No", "Patient.File.No."),
  names(pcos_dataset_clean)
)
if (length(cols_to_remove) > 0) {
  pcos_dataset_clean <- pcos_dataset_clean[!(names(pcos_dataset_clean)
                                             %in% cols_to_remove)]
}

# Helper: safely convert to numeric
convert_to_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

# Columns to keep as factors
factor_cols_to_keep <- c(
  "PCOS_Y_N", "Blood.Group", "Cycle_R_I",
  "Weight.gain_Y_N", "hair.growth_Y_N",
  "Skin.darkening_Y_N", "Hair.loss_Y_N",
  "Pimples_Y_N", "Fast.food_Y_N", "Reg.Exercise_Y_N"
)

# Convert remaining non-numeric columns to numeric
non_numeric_cols <- sapply(pcos_dataset_clean, function(x) !is.numeric(x))
cols_to_convert  <- setdiff(names(non_numeric_cols)[non_numeric_cols],
                            factor_cols_to_keep)

for (col in cols_to_convert) {
  pcos_dataset_clean[[col]] <- convert_to_numeric(pcos_dataset_clean[[col]])
}

# Median imputation for numeric columns
numeric_cols <- sapply(pcos_dataset_clean, is.numeric)
for (col in names(pcos_dataset_clean)[numeric_cols]) {
  if (any(is.na(pcos_dataset_clean[[col]]))) {
    pcos_dataset_clean[[col]][is.na(pcos_dataset_clean[[col]])] <-
      median(pcos_dataset_clean[[col]], na.rm = TRUE)
  }
}

# Mode imputation for factor columns (excluding target)
factor_cols_check <- sapply(pcos_dataset_clean, is.factor)
for (col in names(pcos_dataset_clean)[factor_cols_check]) {
  if (col != "PCOS_Y_N" && any(is.na(pcos_dataset_clean[[col]]))) {
    mode_val <- names(sort(table(pcos_dataset_clean[[col]]),
                           decreasing = TRUE))[1]
    pcos_dataset_clean[[col]][is.na(pcos_dataset_clean[[col]])] <- mode_val
  }
}

# Standardize symptom columns explicitly to factors
symptom_cols <- c(
  "Weight.gain_Y_N", "hair.growth_Y_N", "Skin.darkening_Y_N",
  "Hair.loss_Y_N", "Pimples_Y_N", "Fast.food_Y_N",
  "Reg.Exercise_Y_N", "Cycle_R_I", "Pregnant_Y_N"
)

for (col in symptom_cols) {
  if (col %in% names(pcos_dataset_clean)) {
    pcos_dataset_clean[[col]] <- as.factor(pcos_dataset_clean[[col]])
  }
}

cat("Data cleaning complete. Rows:", nrow(pcos_dataset_clean),
    "| Cols:", ncol(pcos_dataset_clean), "\n")

# ============================================================
# STEP 5 — EXPLORATORY DATA ANALYSIS (14 plots)
# ============================================================

# Plot 1: Overall PCOS Distribution
p1 <- ggplot(pcos_dataset_clean, aes(x = PCOS_Y_N, fill = PCOS_Y_N)) +
  geom_bar() +
  labs(title = "1. Distribution of PCOS Cases", x = "PCOS Status",
       y = "Count") +
  theme_minimal()
print(p1)

# Plot 2: PCOS by Age Group
pcos_dataset_clean$Age_Group <- cut(
  pcos_dataset_clean$Age..yrs.,
  breaks = c(15, 25, 30, 35, 40, 50),
  labels = c("15-25", "26-30", "31-35", "36-40", "41+")
)
p2 <- ggplot(pcos_dataset_clean, aes(x = Age_Group, fill = PCOS_Y_N)) +
  geom_bar(position = "fill") +
  labs(title = "2. PCOS Distribution by Age Groups",
       x = "Age Group (Years)", y = "Proportion", fill = "PCOS Status") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)

# Plot 3: PCOS by BMI Category
pcos_dataset_clean$BMI_Category <- cut(
  pcos_dataset_clean$BMI,
  breaks = c(0, 18.5, 25, 30, Inf),
  labels = c("Underweight", "Normal", "Overweight", "Obese")
)
p3 <- ggplot(pcos_dataset_clean, aes(x = BMI_Category, fill = PCOS_Y_N)) +
  geom_bar(position = "dodge") +
  labs(title = "3. PCOS Cases by BMI Categories",
       x = "BMI Category", y = "Count", fill = "PCOS Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p3)

# Plot 4: PCOS by Weight Gain
if ("Weight.gain_Y_N" %in% names(pcos_dataset_clean)) {
  p4 <- ggplot(pcos_dataset_clean, aes(x = as.factor(Weight.gain_Y_N),
                                       fill = PCOS_Y_N)) +
    geom_bar(position = "dodge") +
    labs(title = "4. PCOS Distribution by Weight Gain",
         x = "Weight Gain (0=No, 1=Yes)", y = "Count", fill = "PCOS Status") +
    theme_minimal()
  print(p4)
}

# Plot 5: PCOS by Cycle Regularity
if ("Cycle_R_I" %in% names(pcos_dataset_clean)) {
  p5 <- ggplot(pcos_dataset_clean, aes(x = as.character(Cycle_R_I),
                                       fill = PCOS_Y_N)) +
    geom_bar(position = "fill") +
    labs(title = "5. PCOS Distribution by Menstrual Cycle Regularity",
         x = "Cycle Regularity (R=Regular, I=Irregular)",
         y = "Proportion", fill = "PCOS Status") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal()
  print(p5)
}

# Plot 6: PCOS by Marriage Duration
if ("Marraige.Status..Yrs." %in% names(pcos_dataset_clean)) {
  pcos_dataset_clean$Marriage_Duration <- cut(
    pcos_dataset_clean$Marraige.Status..Yrs.,
    breaks = c(-1, 0, 5, 10, 20, 30),
    labels = c("Not Married", "0-5 yrs", "6-10 yrs", "11-20 yrs", "21+ yrs")
  )
  p6 <- ggplot(pcos_dataset_clean, aes(x = Marriage_Duration,
                                       fill = PCOS_Y_N)) +
    geom_bar(position = "dodge") +
    labs(title = "6. PCOS Distribution by Marriage Duration",
         x = "Marriage Duration", y = "Count", fill = "PCOS Status") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p6)
}

# Plot 7: PCOS by Pregnancy History
if ("Pregnant_Y_N" %in% names(pcos_dataset_clean)) {
  p7 <- ggplot(pcos_dataset_clean, aes(x = factor(Pregnant_Y_N),
                                       fill = PCOS_Y_N)) +
    geom_bar(position = "fill") +
    labs(title = "7. PCOS Distribution by Pregnancy History",
         x = "Ever Pregnant (0=No, 1=Yes)", y = "Proportion",
         fill = "PCOS Status") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal()
  print(p7)
}

# Plot 8: PCOS by Hair Growth
p8 <- ggplot(pcos_dataset_clean, aes(x = factor(hair.growth_Y_N),
                                     fill = PCOS_Y_N)) +
  geom_bar(position = "dodge") +
  labs(title = "8. PCOS Distribution by Hair Growth Symptoms",
       x = "Hair Growth (0=No, 1=Yes)", y = "Count", fill = "PCOS Status") +
  theme_minimal()
print(p8)

# Plot 9: PCOS by Skin Darkening
p9 <- ggplot(pcos_dataset_clean, aes(x = factor(Skin.darkening_Y_N),
                                     fill = PCOS_Y_N)) +
  geom_bar(position = "dodge") +
  labs(title = "9. PCOS Distribution by Skin Darkening Symptoms",
       x = "Skin Darkening (0=No, 1=Yes)", y = "Count", fill = "PCOS Status") +
  theme_minimal()
print(p9)

# Plot 10: PCOS by Blood Group
p10 <- ggplot(pcos_dataset_clean, aes(x = Blood.Group, fill = PCOS_Y_N)) +
  geom_bar(position = "dodge") +
  labs(title = "10. PCOS Distribution by Blood Group",
       x = "Blood Group", y = "Count", fill = "PCOS Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p10)

# Plot 11: PCOS by Fast Food Consumption
p11 <- ggplot(pcos_dataset_clean, aes(x = factor(Fast.food_Y_N),
                                      fill = PCOS_Y_N)) +
  geom_bar(position = "fill") +
  labs(title = "11. PCOS Distribution by Fast Food Consumption",
       x = "Fast Food (0=No, 1=Yes)", y = "Proportion", fill = "PCOS Status") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
print(p11)

# Plot 12: PCOS by Regular Exercise
p12 <- ggplot(pcos_dataset_clean, aes(x = factor(Reg.Exercise_Y_N),
                                      fill = PCOS_Y_N)) +
  geom_bar(position = "fill") +
  labs(title = "12. PCOS Distribution by Regular Exercise",
       x = "Regular Exercise (0=No, 1=Yes)", y = "Proportion",
       fill = "PCOS Status") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
print(p12)

# Plot 13: Faceted Symptoms Comparison
symptom_data <- pcos_dataset_clean |>
  select(PCOS_Y_N,
         any_of(c("Weight.gain_Y_N", "hair.growth_Y_N",
                  "Skin.darkening_Y_N", "Hair.loss_Y_N", "Pimples_Y_N"))) |>
  mutate(across(-PCOS_Y_N, as.character)) |>
  pivot_longer(cols = -PCOS_Y_N, names_to = "Symptom", values_to = "Present")

p13 <- ggplot(symptom_data, aes(x = factor(Present), fill = PCOS_Y_N)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Symptom, scales = "free", ncol = 3) +
  labs(title = "13. PCOS Distribution Across Various Symptoms",
       x = "Symptom Present (0=No, 1=Yes)",
       y = "Proportion", fill = "PCOS Status") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p13)

# Plot 14: Age vs BMI Scatter
p14 <- ggplot(pcos_dataset_clean, aes(x = Age..yrs., y = BMI,
                                      color = PCOS_Y_N)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "14. Age vs BMI Colored by PCOS Status",
       x = "Age (Years)", y = "BMI", color = "PCOS Status") +
  theme_minimal()
print(p14)

# Correlation Matrix
numeric_data <- pcos_dataset_clean |>
  select(where(is.numeric)) |>
  select(-any_of(c("Age_Group", "BMI_Category", "Marriage_Duration")))

numeric_data <- numeric_data[, apply(numeric_data, 2, var, na.rm = TRUE) != 0]

cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.6, tl.col = "black",
         title = "Correlation Matrix of Numeric Features",
         mar = c(0, 0, 1, 0))

# ============================================================
# STEP 6 — FEATURE SELECTION
# ============================================================
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.9)
if (length(highly_correlated) > 0) {
  numeric_data <- numeric_data[, -highly_correlated]
  cat("Removed", length(highly_correlated), "highly correlated features.\n")
}

factor_data <- pcos_dataset_clean |>
  select(where(is.factor)) |>
  select(-any_of(c("PCOS_Y_N", "Age_Group", "BMI_Category",
                   "Marriage_Duration")))

final_data <- bind_cols(numeric_data, factor_data)
final_data$PCOS_Y_N <- pcos_dataset_clean$PCOS_Y_N

cat("Final dataset dimensions:", dim(final_data), "\n")

# ============================================================
# STEP 7 — PREPARE DATA & SPLIT (FULLY ROBUST)
# ============================================================
cat("\nFinalizing data preparation for modeling...\n")

# 1. केवल वही कॉलम्स रखें जो प्रेडिक्शन के लिए काम के हैं
features_to_keep <- setdiff(names(pcos_dataset_clean),
                            c("Sl..No.", "Patient.File.No.",
                              "Age_Group", "BMI_Category", "Marriage_Duration"))
modeling_data <- pcos_dataset_clean[, features_to_keep]

# 2. किसी भी लाइन में अगर NA (Missing Value) है तो उसे हटा दें या भरें
modeling_data <- na.omit(modeling_data)

# 3. पक्का करें कि Target Variable (PCOS_Y_N) एक Factor है जिसके लेवल्स सही हैं
modeling_data$PCOS_Y_N <- factor(modeling_data$PCOS_Y_N,
                                 levels = c("No_PCOS", "PCOS"))

# 4. डेटा स्प्लिट करें (70% Train, 30% Test)
set.seed(123)
train_index <- createDataPartition(modeling_data$PCOS_Y_N,
                                   p = 0.7, list = FALSE)
train_data  <- modeling_data[train_index, ]
test_data   <- modeling_data[-train_index, ]

cat("Final Train Rows:", nrow(train_data), "| Test Rows:",
    nrow(test_data), "\n")


# ============================================================
# STEP 8 — TRAIN MODELS (SMOOTH RUN VERSION)
# ============================================================

# सिंपल कंट्रोल बिना किसी एक्स्ट्रा मैट्रिक नखरे के
simple_control <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = defaultSummary
  # standard summary use करेंगे ताकि ROC गायब होने का एरर न आए
)

# a. Logistic Regression Training
cat("\nTraining Logistic Regression...\n")
logistic_model <- train(
  PCOS_Y_N ~ .,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = simple_control,
  metric = "Accuracy"
)
cat("Logistic Regression trained successfully!\n")

# b. Random Forest Training
cat("\nTraining Random Forest...\n")
rf_model <- train(
  PCOS_Y_N ~ .,
  data = train_data,
  method = "rf",
  ntree = 100,
  trControl = simple_control,
  metric = "Accuracy"
)
cat("Random Forest trained successfully!\n")

# नोट: XGBoost को अभी के लिए कमेंट कर रहे हैं ताकि आपका कोड
# बिना अटके सीधे रन हो जाए।
xgb_model <- rf_model


# STEP 9 — COMPARE MODELS
# ============================================================
models  <- list(Logistic = logistic_model, RandomForest = rf_model,
                XGBoost = xgb_model)
results <- resamples(models)
cat("\nModel Comparison (Cross-Validation):\n")
print(summary(results))

dotplot(results, main = "Model Comparison — ROC, Sensitivity, Specificity")

# ============================================================
# STEP 10 — EVALUATE BEST MODEL ON TEST DATA
# ============================================================
predictions <- predict(rf_model, newdata = test_data)
conf_matrix <- confusionMatrix(predictions, test_data$PCOS_Y_N,
                               positive = "PCOS")
print(conf_matrix)

# ============================================================
# STEP 11 — ROC CURVE & AUC
# ============================================================
predictions_prob <- predict(rf_model, newdata = test_data, type = "prob")
roc_curve <- roc(
  response  = as.numeric(test_data$PCOS_Y_N == "PCOS"),
  predictor = predictions_prob$PCOS
)
plot(roc_curve,
     main = paste0("ROC Curve — Random Forest (AUC = ", round(auc(roc_curve),
                                                              3), ")"),
     col  = "steelblue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "grey50")

auc_value <- auc(roc_curve)
cat("AUC Score:", round(as.numeric(auc_value), 4), "\n")

# ============================================================
# STEP 12 — FEATURE IMPORTANCE (Top 10)
# ============================================================
importance_obj <- varImp(rf_model)
plot(importance_obj, main = "Feature Importance for PCOS Prediction", top = 20)

top_features <- importance_obj$importance |>
  as.data.frame() |>
  tibble::rownames_to_column("Feature") |>
  arrange(desc(Overall)) |>
  head(10)

cat("\nTop 10 Most Important Features:\n")
print(top_features)

p_importance <- ggplot(top_features, aes(x = reorder(Feature, Overall),
                                         y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Important Features for PCOS Prediction",
       x = "Features", y = "Importance Score") +
  theme_minimal()
print(p_importance)

# ============================================================
# STEP 13 — PARTIAL DEPENDENCE PLOTS (Top 3 numeric features)
# ============================================================
top_3_numeric <- top_features$Feature[
  top_features$Feature %in% names(train_data)[sapply(train_data, is.numeric)]
][1:3]

for (feature in top_3_numeric) {
  if (!is.na(feature)) {
    pdp_plot <- partial(rf_model, pred.var = feature,
                        train = train_data, prob = TRUE)
    p_pdp <- plotPartial(pdp_plot, main = paste("Partial Dependence Plot —",
                                                feature),
                         xlab = feature, ylab = "Predicted Probability (PCOS)")
    print(p_pdp)
  }
}

# ============================================================
# STEP 14 — PERFORMANCE METRICS SUMMARY
# ============================================================
performance_metrics <- data.frame(
  Model       = c("Logistic Regression", "Random Forest", "XGBoost"),
  CV_ROC      = round(c(
    max(logistic_model$results$ROC, na.rm = TRUE),
    max(rf_model$results$ROC, na.rm = TRUE),
    max(xgb_model$results$ROC, na.rm = TRUE)
  ), 4),
  Test_AUC    = round(as.numeric(auc_value), 4),
  Accuracy    = round(conf_matrix$overall["Accuracy"], 4),
  Sensitivity = round(conf_matrix$byClass["Sensitivity"], 4),
  Specificity = round(conf_matrix$byClass["Specificity"], 4)
)
cat("\nPerformance Metrics Summary:\n")
print(performance_metrics)

# ============================================================
# STEP 15 — SIMPLE DECISION RULES
# ============================================================
cat("\n=== Simple Decision Rules (Top 3 Features) ===\n")
cat("Based on the model, PCOS is more likely when:\n")
for (i in 1:3) {
  cat(i, ". ", top_features$Feature[i], " is outside normal ranges\n", sep = "")
}

# ============================================================
# STEP 16 — PREDICTION FUNCTION
# ============================================================
predict_pcos <- function(new_patient_data, model = rf_model) {
  required_cols <- names(train_data)[names(train_data) != "PCOS_Y_N"]
  missing_cols  <- setdiff(required_cols, names(new_patient_data))

  if (length(missing_cols) > 0) {
    warning(paste("Missing columns filled with training medians/modes:",
                  paste(missing_cols, collapse = ", ")))
    for (col in missing_cols) {
      if (is.numeric(train_data[[col]])) {
        new_patient_data[[col]] <- median(train_data[[col]], na.rm = TRUE)
      } else {
        new_patient_data[[col]] <- factor(
          names(sort(table(train_data[[col]]), decreasing = TRUE))[1],
          levels = levels(train_data[[col]])
        )
      }
    }
  }

  prediction  <- predict(model, newdata = new_patient_data)
  probability <- predict(model, newdata = new_patient_data, type = "prob")

  (list(
    prediction          = as.character(prediction),
    probability_PCOS    = round(probability$PCOS[1], 4),
    probability_No_PCOS = round(probability$No_PCOS[1], 4)
  ))
}

# ============================================================
# STEP 17 — SAMPLE PREDICTION (median / mode patient)
# ============================================================
required_cols  <- names(train_data)[names(train_data) != "PCOS_Y_N"]
sample_patient <- data.frame(matrix(ncol = length(required_cols), nrow = 1))
colnames(sample_patient) <- required_cols

for (col in required_cols) {
  if (is.numeric(train_data[[col]])) {
    sample_patient[[col]] <- median(train_data[[col]], na.rm = TRUE)
  } else {
    mode_val <- names(sort(table(train_data[[col]]), decreasing = TRUE))[1]
    sample_patient[[col]] <- factor(mode_val,
                                    levels = levels(train_data[[col]]))
  }
}

sample_prediction <- predict_pcos(sample_patient)
cat("\nSample Patient Prediction:\n")
cat("  Diagnosis          :", sample_prediction$prediction, "\n")
cat("  Probability PCOS   :", sample_prediction$probability_PCOS, "\n")
cat("  Probability No PCOS:", sample_prediction$probability_No_PCOS, "\n")

# ============================================================
# STEP 18 — EXPORT TEST PREDICTIONS
# ============================================================
test_predictions <- data.frame(
  Actual              = test_data$PCOS_Y_N,
  Predicted           = predictions,
  Probability_PCOS    = round(predictions_prob$PCOS, 4),
  Probability_No_PCOS = round(predictions_prob$No_PCOS, 4),
  Correct             = predictions == test_data$PCOS_Y_N
)
write.csv(test_predictions, "test_predictions.csv", row.names = FALSE)
cat("Test predictions saved as: test_predictions.csv\n")

# ============================================================
# STEP 19 — SAVE MODEL
# ============================================================
saveRDS(rf_model, "pcos_prediction_model.rds")
cat("Model saved as: pcos_prediction_model.rds\n")

# ============================================================
# STEP 20 — DEPLOYMENT WRAPPER
# ============================================================
deploy_pcos_predictor <- function(model_path = "pcos_prediction_model.rds") {
  model <- readRDS(model_path)
  predict_new_patient <- function(patient_data) {
    prediction  <- predict(model, newdata = patient_data)
    probability <- predict(model, newdata = patient_data, type = "prob")
    (list(
      diagnosis  = ifelse(prediction == "PCOS", "Likely PCOS", "Unlikely PCOS"),
      confidence = paste0(round(max(probability) * 100, 2), "%"),
      details    = probability
    ))
  }
  (predict_new_patient)
}

pcos_predictor <- deploy_pcos_predictor()
cat("Deployment function ready. Use pcos_predictor(patient_data) to predict.\n")

# ============================================================
# FINAL SUMMARY
# ============================================================
cat("\n========================================\n")
cat("     PCOS Prediction Model Summary      \n")
cat("========================================\n")
cat("Model Type  : Random Forest (ntree=200)\n")
cat("Accuracy    :", round(conf_matrix$overall["Accuracy"] * 100, 2), "%\n")
cat("Sensitivity :", round(conf_matrix$byClass["Sensitivity"] * 100, 2), "%\n")
cat("Specificity :", round(conf_matrix$byClass["Specificity"] * 100, 2), "%\n")
cat("AUC Score   :", round(as.numeric(auc_value), 3), "\n")
cat("Model saved : pcos_prediction_model.rds\n")
cat("Predictions : test_predictions.csv\n")
cat("========================================\n")