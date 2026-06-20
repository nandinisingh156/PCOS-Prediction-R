# 🩺 PCOS Prediction Model — R Project

A machine learning project that predicts **Polycystic Ovary Syndrome (PCOS)** using clinical and lifestyle data. Three models are trained and compared: **Logistic Regression**, **Random Forest**, and **XGBoost**.

---

## 📁 Project Structure

```
PCOS-Prediction-R/
│
├── data/
│   └── pcos_dataset.csv          # Raw dataset (input)
│
├── output/
│   ├── predictions.csv           # Test set predictions
│   ├── pcos_prediction_model.rds # Saved Random Forest model
│   ├── pcos_clean.rds            # Cleaned dataset
│   ├── train_data.rds            # Training split
│   ├── test_data.rds             # Test split
│   └── cor_matrix.rds            # Correlation matrix
│
├── screenshots/
│   └── dashboard.png             # Project dashboard preview
│
├── scripts/
│   ├── data_cleaning.R           # Step 1 — Load & clean data
│   ├── eda.R                     # Step 2 — Exploratory analysis (14 plots)
│   ├── model_training.R          # Step 3 — Train, compare & evaluate models
│   └── prediction.R              # Step 4 — Predict new patients
│
├── PCOS_Prediction.R             # Master script (runs all steps)
├── README.md                     # This file
└── requirements.txt              # R package dependencies
```

---

## 🚀 Quick Start

### 1. Install R and VS Code
- Download R: https://cran.r-project.org/
- Download VS Code: https://code.visualstudio.com/
- Install R extension in VS Code: `REditorSupport.r`

### 2. Install required packages
Open R console and run:
```r
install.packages(c(
  "tidyverse", "caret", "randomForest",
  "xgboost", "pROC", "corrplot", "pdp"
), dependencies = TRUE)
```

### 3. Set your data path
Open `scripts/data_cleaning.R` and update line 10:
```r
DATA_PATH <- "E:/projects/PCOS-Prediction-R/data/pcos_dataset.csv"
```

### 4. Run scripts in order
```r
source("scripts/data_cleaning.R")   # Clean data
source("scripts/eda.R")             # Explore data
source("scripts/model_training.R")  # Train models
source("scripts/prediction.R")      # Make predictions
```
Or run the master script:
```r
source("PCOS_Prediction.R")
```

### 5. Run from terminal (after adding R to PATH)
```powershell
& "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" "PCOS_Prediction.R"
```

---

## 📊 Dataset

| Property | Details |
|---|---|
| Source | Kaggle — PCOS Dataset |
| Rows | ~541 patients |
| Target | `PCOS_Y_N` (Yes / No) |
| Features | 41 clinical + lifestyle variables |

### Key Features Used
- **Demographics** — Age, BMI, Blood Group
- **Symptoms** — Weight gain, Hair growth, Skin darkening, Hair loss, Pimples
- **Lifestyle** — Fast food consumption, Regular exercise
- **Clinical** — Cycle regularity, Pregnancy history, Marriage duration
- **Hormonal** — FSH, LH, TSH, AMH levels

---

## 🤖 Models

| Model | Algorithm | Cross-Validation |
|---|---|---|
| Logistic Regression | `glm` (binomial) | 10-fold CV |
| Random Forest | `randomForest` | 5-fold CV |
| XGBoost | `xgbTree` | 5-fold CV |

Best model selection is based on **AUC (ROC)** score.

---

## 📈 Results (Random Forest — Best Model)

| Metric | Score |
|---|---|
| Accuracy | ~88% |
| Sensitivity | ~85% |
| Specificity | ~90% |
| AUC | ~0.94 |

> Results may vary slightly depending on your dataset version and random seed.

---

## 🔮 Making Predictions

```r
# Load the saved model
model      <- readRDS("pcos_prediction_model.rds")
train_data <- readRDS("train_data.rds")

# Create a patient record
new_patient <- data.frame(
  Age..yrs.        = 28,
  BMI              = 27.5,
  Cycle.R.I.       = factor("I", levels = levels(train_data$Cycle.R.I.)),
  Weight.gain.Y.N  = factor("1", levels = levels(train_data$Weight.gain.Y.N)),
  hair.growth.Y.N  = factor("1", levels = levels(train_data$hair.growth.Y.N))
  # ... add more columns as needed
)

# Predict
prediction  <- predict(model, newdata = new_patient)
probability <- predict(model, newdata = new_patient, type = "prob")

cat("Diagnosis   :", as.character(prediction), "\n")
cat("Probability :", round(probability$PCOS, 4), "\n")
```

---

## 📦 Output Files

| File | Description |
|---|---|
| `pcos_prediction_model.rds` | Trained Random Forest model |
| `output/predictions.csv` | Test set actual vs predicted results |
| `pcos_clean.rds` | Cleaned and preprocessed dataset |
| `train_data.rds` | 70% training split |
| `test_data.rds` | 30% test split |

---

## ⚠️ Disclaimer

This model is built for **educational and research purposes only**.  
It is **not a medical diagnostic tool**.  
Always consult a qualified healthcare professional for medical advice.

---

## 👩‍💻 Tech Stack

![R](https://img.shields.io/badge/R-4.5.3-blue)
![VS Code](https://img.shields.io/badge/VS%20Code-Editor-blue)
![Random Forest](https://img.shields.io/badge/Model-Random%20Forest-green)
![XGBoost](https://img.shields.io/badge/Model-XGBoost-orange)