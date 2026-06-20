# ============================================================
# data_cleaning.R
# Loads raw CSV, cleans it, and saves pcos_clean.rds
# ============================================================

# ── 0. Packages ──────────────────────────────────────────────
required <- c("tidyverse")
new_pkgs  <- required[!required %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
library(tidyverse)

# ── 1. Load raw data ─────────────────────────────────────────
data_path <- "E:/projects/PCOS-Prediction-R/data/pcos_dataset.csv"

pcos_dataset <- read.csv(data_path, stringsAsFactors = TRUE)
# !! UPDATE the path above to match your CSV location !!

pcos_raw <- read.csv(DATA_PATH, stringsAsFactors = TRUE)
cat("Raw data loaded:", nrow(pcos_raw), "rows x", ncol(pcos_raw), "cols\n")

# ── 2. Target variable ───────────────────────────────────────
pcos_raw$PCOS_Y_N <- as.factor(pcos_raw$PCOS_Y_N)
levels(pcos_raw$PCOS_Y_N) <- c("No_PCOS", "PCOS")

# ── 3. Remove identifier columns ────────────────────────────
id_cols <- intersect(c("Sl..No", "Patient.File.No."), names(pcos_raw))
if (length(id_cols) > 0) pcos_raw <- pcos_raw |> select(-all_of(id_cols))

# ── 4. Check missing values ──────────────────────────────────
cat("\nMissing values before cleaning:\n")
mv <- colSums(is.na(pcos_raw))
print(mv[mv > 0])

# ── 5. Columns to stay as factors ───────────────────────────
factor_cols_to_keep <- c(
  "PCOS_Y_N", "Blood.Group", "Cycle.R.I.",
  "Weight.gain.Y.N", "hair.growth.Y.N",
  "Skin.darkening..Y.N", "Hair.loss.Y.N",
  "Pimples.Y.N", "Fast.food..Y.N", "Reg.Exercise.Y.N"
)

# ── 6. Convert remaining columns to numeric ──────────────────
convert_to_numeric <- function(x) suppressWarnings(as.numeric(as.character(x)))

non_numeric <- sapply(pcos_raw, function(x) !is.numeric(x))
cols_to_num <- setdiff(names(non_numeric)[non_numeric], factor_cols_to_keep)
for (col in cols_to_num) pcos_raw[[col]] <- convert_to_numeric(pcos_raw[[col]])

# ── 7. Impute missing values ─────────────────────────────────
# Numeric → median
num_cols <- sapply(pcos_raw, is.numeric)
for (col in names(pcos_raw)[num_cols]) {
  if (any(is.na(pcos_raw[[col]]))) {
    pcos_raw[[col]][is.na(pcos_raw[[col]])] <-
      median(pcos_raw[[col]], na.rm = TRUE)
  }
}

# Factor → mode (excluding target)
fac_cols <- sapply(pcos_raw, is.factor)
for (col in names(pcos_raw)[fac_cols]) {
  if (col != "PCOS_Y_N" && any(is.na(pcos_raw[[col]]))) {
    mode_val <- names(sort(table(pcos_raw[[col]]), decreasing = TRUE))[1]
    pcos_raw[[col]][is.na(pcos_raw[[col]])] <- mode_val
  }
}

# ── 8. Convert symptom columns to factors ───────────────────
symptom_cols <- c(
  "Weight.gain.Y.N", "hair.growth.Y.N", "Skin.darkening..Y.N",
  "Hair.loss.Y.N", "Pimples.Y.N", "Fast.food..Y.N",
  "Reg.Exercise.Y.N", "Cycle.R.I.", "Pregnant.Y.N"
)
for (col in symptom_cols) {
  if (col %in% names(pcos_raw)) pcos_raw[[col]] <- as.factor(pcos_raw[[col]])
}

# ── 9. Add derived columns ───────────────────────────────────
if ("Age..yrs." %in% names(pcos_raw)) {
  pcos_raw$Age_Group <- cut(
    pcos_raw$Age..yrs.,
    breaks = c(15, 25, 30, 35, 40, 50),
    labels = c("15-25", "26-30", "31-35", "36-40", "41+")
  )
}

if ("BMI" %in% names(pcos_raw)) {
  pcos_raw$BMI_Category <- cut(
    pcos_raw$BMI,
    breaks = c(0, 18.5, 25, 30, Inf),
    labels = c("Underweight", "Normal", "Overweight", "Obese")
  )
}

# ── 10. Final check ──────────────────────────────────────────
cat("\nMissing values after cleaning:\n")
mv2 <- colSums(is.na(pcos_raw))
print(mv2[mv2 > 0])
cat("Clean data dimensions:", nrow(pcos_raw), "rows x",
    ncol(pcos_raw), "cols\n")

# ── 11. Save cleaned data ────────────────────────────────────
saveRDS(pcos_raw, "pcos_clean.rds")
cat("Cleaned data saved as: pcos_clean.rds\n")