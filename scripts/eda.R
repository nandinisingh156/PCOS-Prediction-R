# ============================================================
# eda.R
# Exploratory Data Analysis — 14 plots + correlation matrix
# Run AFTER data_cleaning.R
# ============================================================

# ── 0. Packages ──────────────────────────────────────────────
required <- c("tidyverse", "corrplot", "scales")
new_pkgs  <- required[!required %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
library(tidyverse)
library(corrplot)
library(scales)

# ── 1. Load cleaned data ─────────────────────────────────────
if (!file.exists("pcos_clean.rds")) stop("Run data_cleaning.R first!")
df <- readRDS("pcos_clean.rds")
cat("Data loaded:", nrow(df), "rows x", ncol(df), "cols\n")

# ── 2. Plot 1: Overall PCOS Distribution ─────────────────────
p1 <- ggplot(df, aes(x = PCOS_Y_N, fill = PCOS_Y_N)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "1. Distribution of PCOS Cases",
       x = "PCOS Status", y = "Count") +
  theme_minimal() + theme(legend.position = "none")
print(p1)

# ── 3. Plot 2: PCOS by Age Group ─────────────────────────────
p2 <- ggplot(df, aes(x = Age_Group, fill = PCOS_Y_N)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = "2. PCOS by Age Group",
       x = "Age Group", y = "Proportion", fill = "PCOS Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)

# ── 4. Plot 3: PCOS by BMI Category ──────────────────────────
p3 <- ggplot(df, aes(x = BMI_Category, fill = PCOS_Y_N)) +
  geom_bar(position = "dodge") +
  labs(title = "3. PCOS by BMI Category",
       x = "BMI Category", y = "Count", fill = "PCOS Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p3)

cat("Cycle.R.I. unique values:", unique(pcos_dataset_clean$Cycle.R.I.), "\n")

# ── 5. Plot 4: PCOS by Weight Gain ───────────────────────────
if ("Weight.gain.Y.N" %in% names(df)) {
  p4 <- ggplot(df, aes(x = as.factor(Weight.gain.Y.N), fill = PCOS_Y_N)) +
    geom_bar(position = "dodge") +
    labs(title = "4. PCOS by Weight Gain",
         x = "Weight Gain (0=No, 1=Yes)", y = "Count", fill = "PCOS Status") +
    theme_minimal()
  print(p4)
}

# ── 6. Plot 5: PCOS by Cycle Regularity ──────────────────────
p5 <- ggplot(df, aes(x = factor(Cycle.R.I.), fill = PCOS_Y_N)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = "5. PCOS by Menstrual Cycle Regularity",
       x = "Cycle (R=Regular, I=Irregular)",
       y = "Proportion", fill = "PCOS Status") +
  theme_minimal()
print(p5)

# ── 7. Plot 6: PCOS by Marriage Duration ─────────────────────
if ("Marraige.Status..Yrs." %in% names(df)) {
  df$Marriage_Duration <- cut(
    df$Marraige.Status..Yrs.,
    breaks = c(-1, 0, 5, 10, 20, 30),
    labels = c("Not Married", "0-5 yrs", "6-10 yrs", "11-20 yrs", "21+ yrs")
  )
  p6 <- ggplot(df, aes(x = Marriage_Duration, fill = PCOS_Y_N)) +
    geom_bar(position = "dodge") +
    labs(title = "6. PCOS by Marriage Duration",
         x = "Duration", y = "Count", fill = "PCOS Status") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p6)
}

# ── 8. Plot 7: PCOS by Pregnancy History ─────────────────────
if ("Pregnant.Y.N" %in% names(df)) {
  p7 <- ggplot(df, aes(x = factor(Pregnant.Y.N), fill = PCOS_Y_N)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = percent) +
    labs(title = "7. PCOS by Pregnancy History",
         x = "Ever Pregnant (0=No, 1=Yes)",
         y = "Proportion", fill = "PCOS Status") +
    theme_minimal()
  print(p7)
}

# ── 9. Plot 8: PCOS by Hair Growth ───────────────────────────
p8 <- ggplot(df, aes(x = factor(hair.growth.Y.N), fill = PCOS_Y_N)) +
  geom_bar(position = "dodge") +
  labs(title = "8. PCOS by Hair Growth",
       x = "Hair Growth (0=No, 1=Yes)", y = "Count", fill = "PCOS Status") +
  theme_minimal()
print(p8)

# ── 10. Plot 9: PCOS by Skin Darkening ───────────────────────
p9 <- ggplot(df, aes(x = factor(Skin.darkening..Y.N), fill = PCOS_Y_N)) +
  geom_bar(position = "dodge") +
  labs(title = "9. PCOS by Skin Darkening",
       x = "Skin Darkening (0=No, 1=Yes)", y = "Count", fill = "PCOS Status") +
  theme_minimal()
print(p9)

# ── 11. Plot 10: PCOS by Blood Group ─────────────────────────
p10 <- ggplot(df, aes(x = Blood.Group, fill = PCOS_Y_N)) +
  geom_bar(position = "dodge") +
  labs(title = "10. PCOS by Blood Group",
       x = "Blood Group", y = "Count", fill = "PCOS Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p10)

# ── 12. Plot 11: PCOS by Fast Food ───────────────────────────
p11 <- ggplot(df, aes(x = factor(Fast.food..Y.N), fill = PCOS_Y_N)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = "11. PCOS by Fast Food Consumption",
       x = "Fast Food (0=No, 1=Yes)", y = "Proportion", fill = "PCOS Status") +
  theme_minimal()
print(p11)

# ── 13. Plot 12: PCOS by Exercise ────────────────────────────
p12 <- ggplot(df, aes(x = factor(Reg.Exercise.Y.N), fill = PCOS_Y_N)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = "12. PCOS by Regular Exercise",
       x = "Exercise (0=No, 1=Yes)", y = "Proportion", fill = "PCOS Status") +
  theme_minimal()
print(p12)

# ── 14. Plot 13: Faceted Symptoms ────────────────────────────
symptom_data <- df |>
  select(PCOS_Y_N, any_of(c(
    "Weight.gain.Y.N", "hair.growth.Y.N",
    "Skin.darkening..Y.N", "Hair.loss.Y.N", "Pimples.Y.N"
  ))) |>
  mutate(across(-PCOS_Y_N, as.character)) |>
  pivot_longer(-PCOS_Y_N, names_to = "Symptom", values_to = "Present")

p13 <- ggplot(symptom_data, aes(x = factor(Present), fill = PCOS_Y_N)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Symptom, ncol = 3) +
  scale_y_continuous(labels = percent) +
  labs(title = "13. PCOS Across Multiple Symptoms",
       x = "Present (0=No, 1=Yes)", y = "Proportion", fill = "PCOS Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p13)

# ── 15. Plot 14: Age vs BMI Scatter ──────────────────────────
p14 <- ggplot(df, aes(x = Age..yrs., y = BMI, color = PCOS_Y_N)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "14. Age vs BMI by PCOS Status",
       x = "Age (Years)", y = "BMI", color = "PCOS Status") +
  theme_minimal()
print(p14)

# ── 16. Correlation Matrix ────────────────────────────────────
num_df <- df |>
  select(where(is.numeric)) |>
  select(-any_of(c("Age_Group", "BMI_Category", "Marriage_Duration")))

num_df <- num_df[, apply(num_df, 2, var, na.rm = TRUE) != 0]
cor_mat <- cor(num_df, use = "complete.obs")

corrplot(cor_mat,
         method = "color", type = "upper",
         tl.cex = 0.6, tl.col = "black",
         title  = "Correlation Matrix of Numeric Features",
         mar    = c(0, 0, 1, 0))

cat("\nEDA complete — all 14 plots printed.\n")

# Save correlation matrix for use in model_training.R
saveRDS(cor_mat, "cor_matrix.rds")
cat("Correlation matrix saved as: cor_matrix.rds\n")