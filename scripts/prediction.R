# ============================================================
# prediction.R
# Load saved model → predict on new patients → deploy function
# Run AFTER model_training.R
# ============================================================

# ── 0. Packages ──────────────────────────────────────────────
required <- c("caret", "randomForest", "tidyverse")
new_pkgs  <- required[!required %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)

library(caret)
library(randomForest)
library(tidyverse)

# ── 1. Load saved model & training data ──────────────────────
if (!file.exists("pcos_prediction_model.rds"))
  stop("Run model_training.R first!")
if (!file.exists("train_data.rds")) stop("Run model_training.R first!")

model      <- readRDS("pcos_prediction_model.rds")
train_data <- readRDS("train_data.rds")

cat("Model and training data loaded.\n")
cat("Model type:", model$method, "\n")

# ── 2. Core Prediction Function ───────────────────────────────
predict_pcos <- function(patient_data, model = model, train_ref = train_data) {

  required_cols <- names(train_ref)[names(train_ref) != "PCOS_Y_N"]
  missing_cols  <- setdiff(required_cols, names(patient_data))

  # Auto-fill any missing columns with training medians / modes
  if (length(missing_cols) > 0) {
    message("Filling missing columns with training defaults: ",
            paste(missing_cols, collapse = ", "))
    for (col in missing_cols) {
      if (is.numeric(train_ref[[col]])) {
        patient_data[[col]] <- median(train_ref[[col]], na.rm = TRUE)
      } else {
        mode_val <- names(sort(table(train_ref[[col]]), decreasing = TRUE))[1]
        patient_data[[col]] <- factor(mode_val,
                                      levels = levels(train_ref[[col]]))
      }
    }
  }

  # Align factor levels with training data
  for (col in names(patient_data)) {
    if (col %in% names(train_ref) && is.factor(train_ref[[col]])) {
      patient_data[[col]] <- factor(patient_data[[col]],
                                    levels = levels(train_ref[[col]]))
    }
  }

  prediction  <- predict(model, newdata = patient_data)
  probability <- predict(model, newdata = patient_data, type = "prob")

  result <- list(
    diagnosis           = ifelse(prediction == "PCOS", "⚠ Likely PCOS",
                                 "✅ Unlikely PCOS"),
    prediction          = as.character(prediction),
    probability_PCOS    = round(probability$PCOS[1], 4),
    probability_No_PCOS = round(probability$No_PCOS[1], 4),
    confidence          = paste0(round(max(probability) * 100, 1), "%")
  )
  result
}

# ── 3. Sample Prediction (median/mode patient) ───────────────
cat("\n--- Sample Patient (median/mode values) ---\n")

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

result <- predict_pcos(sample_patient)
cat("Diagnosis           :", result$diagnosis, "\n")
cat("Prediction          :", result$prediction, "\n")
cat("Probability PCOS    :", result$probability_PCOS, "\n")
cat("Probability No PCOS :", result$probability_No_PCOS, "\n")
cat("Confidence          :", result$confidence, "\n")

# ── 4. Custom Patient Example ─────────────────────────────────
# Edit the values below to predict for a real patient.
# Only add columns you know — missing ones are auto-filled.
cat("\n--- Custom Patient Example ---\n")

custom_patient <- data.frame(
  Age..yrs.           = 28,
  BMI                 = 27.5,
  Cycle.R.I.          = factor("I", levels = levels(train_data$Cycle.R.I.)),
  Weight.gain.Y.N    = factor("1", levels = levels(train_data$Weight.gain.Y.N)),
  hair.growth.Y.N    = factor("1", levels = levels(train_data$hair.growth.Y.N)),
  Skin.darkening..Y.N = factor("0", levels =
                                 levels(train_data$Skin.darkening..Y.N)),
  Hair.loss.Y.N       = factor("1", levels = levels(train_data$Hair.loss.Y.N)),
  Pimples.Y.N         = factor("1", levels = levels(train_data$Pimples.Y.N)),
  Fast.food..Y.N      = factor("1", levels = levels(train_data$Fast.food..Y.N)),
  Reg.Exercise.Y.N   = factor("0", levels = levels(train_data$Reg.Exercise.Y.N))
)

custom_result <- predict_pcos(custom_patient)
cat("Diagnosis           :", custom_result$diagnosis, "\n")
cat("Probability PCOS    :", custom_result$probability_PCOS, "\n")
cat("Confidence          :", custom_result$confidence, "\n")

# ── 5. Batch Prediction from CSV ──────────────────────────────
# Uncomment and set path to run predictions on a CSV file of patients
batch_data    <- read.csv("new_patients.csv", stringsAsFactors = TRUE)
batch_preds   <- predict(model, newdata = batch_data)
batch_probs   <- predict(model, newdata = batch_data, type = "prob")

batch_results <- data.frame(
  Predicted           = batch_preds,
  Probability_PCOS    = round(batch_probs$PCOS, 4),
  Probability_No_PCOS = round(batch_probs$No_PCOS, 4)
)
write.csv(batch_results, "batch_predictions.csv", row.names = FALSE)
cat("Batch predictions saved: batch_predictions.csv\n")

# ── 6. Deploy Function (reuse in other scripts) ───────────────
deploy_pcos_predictor <- function(model_path     = "pcos_prediction_model.rds",
                                  train_data_path = "train_data.rds") {
  saved_model <- readRDS(model_path)
  saved_train <- readRDS(train_data_path)

  function(patient_data) predict_pcos(patient_data, saved_model, saved_train)
}

pcos_predictor <- deploy_pcos_predictor()
cat("\nDeployment function ready.\n")
cat("Usage: pcos_predictor(your_patient_dataframe)\n")

# ── 7. Load & Display Previously Saved Test Predictions ───────
if (file.exists("test_predictions.csv")) {
  test_preds <- read.csv("test_predictions.csv")
  cat("\nTest Predictions Summary:\n")
  cat("Total predictions :", nrow(test_preds), "\n")
  cat("Correct predictions:", sum(test_preds$Correct), "\n")
  cat("Accuracy           :", round(mean(test_preds$Correct) * 100, 2), "%\n")
  cat("\nFirst 10 rows:\n")
  print(head(test_preds, 10))
}