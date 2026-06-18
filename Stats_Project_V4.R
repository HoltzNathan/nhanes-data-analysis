# Holtz Final Project

library(haven)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(car)



data_path <- "D:/Stat_Project_2/"

ALQ    <- read_xpt(paste0(data_path, "ALQ_L.xpt"))
DPQ    <- read_xpt(paste0(data_path, "DPQ_L.xpt"))
HSCRP  <- read_xpt(paste0(data_path, "HSCRP_L.xpt"))
SMQ    <- read_xpt(paste0(data_path, "SMQ_L.xpt"))
TRIGLY <- read_xpt(paste0(data_path, "TRIGLY_L.xpt"))
BPXO   <- read_xpt(paste0(data_path, "BPXO_L.xpt"))



hscrp_sub <- data.frame(
  SEQN = HSCRP[[1]],
  HSCRP_var = HSCRP[[3]]
)

alq_sub <- data.frame(
  SEQN = ALQ[[1]],
  ALQ_var = ifelse(ALQ[[2]] == 2, 0, ALQ[[4]])
)

smq_sub <- data.frame(
  SEQN = SMQ[[1]],
  SMQ_var = ifelse(SMQ[[2]] == 2, 0, SMQ[[5]])
)

dpq_sub <- data.frame(
  SEQN = DPQ[[1]],
  DPQ_var = DPQ[[3]]
)

trigly_sub <- data.frame(
  SEQN = TRIGLY[[1]],
  TRIGLY_var = TRIGLY[[5]]
)

bpxo_sub <- data.frame(
  SEQN = BPXO[[1]],
  BPXO_var = BPXO[[8]]
)



main_df_v2 <- hscrp_sub %>%
  left_join(alq_sub, by = "SEQN") %>%
  left_join(smq_sub, by = "SEQN") %>%
  left_join(dpq_sub, by = "SEQN") %>%
  left_join(trigly_sub, by = "SEQN") %>%
  left_join(bpxo_sub, by = "SEQN")

View(main_df_v2)
head(main_df_v2)
str(main_df_v2)
summary(main_df_v2)

saveRDS(main_df_v2, "D:/Stat_Project_2/main_df_v2_raw.rds")



ggplot(main_df_v2, aes(x = HSCRP_var)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(
    title = "C-Reactive Protein (mg/mL)",
    x = "C-Reactive Protein (mg/mL)",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(main_df_v2, aes(x = ALQ_var)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(
    title = "Alcoholic Drinks Consumed per Day in the Last 12 Months",
    x = "Alcoholic Drinks Consumed per Day in the Last 12 Months",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(main_df_v2, aes(x = SMQ_var)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(
    title = "Average Cigarettes Smoked per Day in the Past 30 Days",
    x = "Average Cigarettes Smoked per Day in the Past 30 Days",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(main_df_v2, aes(x = DPQ_var)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(
    title = "Depression Score",
    x = "Depression Score",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(main_df_v2, aes(x = TRIGLY_var)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(
    title = "Triglycerides (mg/dL)",
    x = "Triglycerides (mg/dL)",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(main_df_v2, aes(x = BPXO_var)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(
    title = "Systolic Blood Pressure (mmHg)",
    x = "Systolic Blood Pressure (mmHg)",
    y = "Frequency"
  ) +
  theme_minimal()


main_df_v2_no_outliers <- main_df_v2 %>%
  mutate(
    ALQ_var   = ifelse(ALQ_var > 100, NA, ALQ_var),
    SMQ_var   = ifelse(SMQ_var > 100, NA, SMQ_var),
    HSCRP_var = ifelse(HSCRP_var > 30, NA, HSCRP_var)
  )

View(main_df_v2_no_outliers)
summary(main_df_v2_no_outliers)
colSums(is.na(main_df_v2_no_outliers))
sum(complete.cases(main_df_v2_no_outliers))



ggplot(main_df_v2_no_outliers, aes(x = ALQ_var)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(
    title = "Alcoholic Drinks Consumed per Day in the Last 12 Months",
    subtitle = "Outliers Removed",
    x = "Alcoholic Drinks Consumed per Day in the Last 12 Months",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(x = SMQ_var)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(
    title = "Average Cigarettes Smoked per Day in the Past 30 Days",
    subtitle = "Outliers Removed",
    x = "Average Cigarettes Smoked per Day in the Past 30 Days",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(x = HSCRP_var)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  labs(
    title = "C-Reactive Protein (mg/mL)",
    subtitle = "Outliers Removed",
    x = "C-Reactive Protein (mg/mL)",
    y = "Frequency"
  ) +
  theme_minimal()



ggplot(main_df_v2_no_outliers, aes(x = HSCRP_var)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot: C-Reactive Protein (mg/mL)",
       x = "C-Reactive Protein (mg/mL)",
       y = "Density") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(sample = HSCRP_var)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot: C-Reactive Protein (mg/mL)") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(x = ALQ_var)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot: Alcoholic Drinks Consumed per Day",
       x = "Alcoholic Drinks Consumed per Day",
       y = "Density") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(sample = ALQ_var)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot: Alcoholic Drinks Consumed per Day") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(x = SMQ_var)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot: Cigarettes Smoked per Day",
       x = "Cigarettes Smoked per Day",
       y = "Density") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(sample = SMQ_var)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot: Cigarettes Smoked per Day") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(x = DPQ_var)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot: Depression Score",
       x = "Depression Score",
       y = "Density") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(sample = DPQ_var)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot: Depression Score") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(x = TRIGLY_var)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot: Triglycerides (mg/dL)",
       x = "Triglycerides (mg/dL)",
       y = "Density") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(sample = TRIGLY_var)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot: Triglycerides (mg/dL)") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(x = BPXO_var)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot: Systolic Blood Pressure (mmHg)",
       x = "Systolic Blood Pressure (mmHg)",
       y = "Density") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(sample = BPXO_var)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot: Systolic Blood Pressure (mmHg)") +
  theme_minimal()

# 8. Boxplots

ggplot(main_df_v2_no_outliers, aes(y = HSCRP_var)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "C-Reactive Protein (mg/mL)",
       y = "C-Reactive Protein (mg/mL)",
       x = "") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(y = ALQ_var)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Alcoholic Drinks Consumed per Day",
       y = "Alcoholic Drinks Consumed per Day",
       x = "") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(y = SMQ_var)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Cigarettes Smoked per Day",
       y = "Cigarettes Smoked per Day",
       x = "") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(y = DPQ_var)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Depression Score",
       y = "Depression Score",
       x = "") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(y = TRIGLY_var)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Triglycerides (mg/dL)",
       y = "Triglycerides (mg/dL)",
       x = "") +
  theme_minimal()

ggplot(main_df_v2_no_outliers, aes(y = BPXO_var)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Systolic Blood Pressure (mmHg)",
       y = "Systolic Blood Pressure (mmHg)",
       x = "") +
  theme_minimal()



analysis_df <- main_df_v2_no_outliers %>%
  mutate(
    log_HSCRP = log(HSCRP_var + 0.1),
    log_ALQ   = log1p(ALQ_var),
    log_SMQ   = log1p(SMQ_var)
  )

View(analysis_df)
summary(analysis_df)
colSums(is.na(analysis_df))
sum(complete.cases(analysis_df))



ggplot(analysis_df, aes(x = log_HSCRP)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(
    title = "C-Reactive Protein (mg/mL)",
    subtitle = "Log Transformation Applied",
    x = "log(HSCRP + 0.1)",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(analysis_df, aes(x = log_ALQ)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(
    title = "Alcoholic Drinks Consumed per Day",
    subtitle = "log(1 + x) Transformation Applied",
    x = "log(1 + ALQ)",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(analysis_df, aes(x = log_SMQ)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(
    title = "Cigarettes Smoked per Day",
    subtitle = "log(1 + x) Transformation Applied",
    x = "log(1 + SMQ)",
    y = "Frequency"
  ) +
  theme_minimal()



model_alq <- lm(log_HSCRP ~ log_ALQ, data = analysis_df)
summary(model_alq)

model_smq <- lm(log_HSCRP ~ log_SMQ, data = analysis_df)
summary(model_smq)

model_dpq <- lm(log_HSCRP ~ DPQ_var, data = analysis_df)
summary(model_dpq)

model_trig <- lm(log_HSCRP ~ TRIGLY_var, data = analysis_df)
summary(model_trig)

model_bpxo <- lm(log_HSCRP ~ BPXO_var, data = analysis_df)
summary(model_bpxo)



ggplot(analysis_df, aes(x = log_ALQ, y = log_HSCRP)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(
    title = "Simple Linear Regression: Alcohol vs C-Reactive Protein",
    subtitle = "Transformed Variables",
    x = "log(1 + Alcoholic Drinks per Day)",
    y = "log(C-Reactive Protein + 0.1)"
  ) +
  theme_minimal()

ggplot(analysis_df, aes(x = log_SMQ, y = log_HSCRP)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(
    title = "Simple Linear Regression: Smoking vs C-Reactive Protein",
    subtitle = "Transformed Variables",
    x = "log(1 + Cigarettes per Day)",
    y = "log(C-Reactive Protein + 0.1)"
  ) +
  theme_minimal()

ggplot(analysis_df, aes(x = DPQ_var, y = log_HSCRP)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(
    title = "Simple Linear Regression: Depression vs C-Reactive Protein",
    subtitle = "Outcome Transformed",
    x = "Depression Score",
    y = "log(C-Reactive Protein + 0.1)"
  ) +
  theme_minimal()

ggplot(analysis_df, aes(x = TRIGLY_var, y = log_HSCRP)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(
    title = "Simple Linear Regression: Triglycerides vs C-Reactive Protein",
    subtitle = "Outcome Transformed",
    x = "Triglycerides (mg/dL)",
    y = "log(C-Reactive Protein + 0.1)"
  ) +
  theme_minimal()

ggplot(analysis_df, aes(x = BPXO_var, y = log_HSCRP)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(
    title = "Simple Linear Regression: Systolic Blood Pressure vs C-Reactive Protein",
    subtitle = "Outcome Transformed",
    x = "Systolic Blood Pressure (mmHg)",
    y = "log(C-Reactive Protein + 0.1)"
  ) +
  theme_minimal()



get_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

get_stats <- function(x) {
  x_nonmiss <- x[!is.na(x)]
  
  data.frame(
    N = length(x_nonmiss),
    Mean = mean(x_nonmiss),
    Median = median(x_nonmiss),
    Mode = get_mode(x_nonmiss),
    Min = min(x_nonmiss),
    Max = max(x_nonmiss),
    Range = max(x_nonmiss) - min(x_nonmiss),
    Variance = var(x_nonmiss),
    SD = sd(x_nonmiss)
  )
}

alq_before  <- get_stats(analysis_df$ALQ_var)
alq_after   <- get_stats(analysis_df$log_ALQ)

smq_before  <- get_stats(analysis_df$SMQ_var)
smq_after   <- get_stats(analysis_df$log_SMQ)

dpq_before  <- get_stats(analysis_df$DPQ_var)
trig_before <- get_stats(analysis_df$TRIGLY_var)
bpxo_before <- get_stats(analysis_df$BPXO_var)

descriptive_results <- bind_rows(
  cbind(Variable = "ALQ_var",    Transformation = "Before",      alq_before),
  cbind(Variable = "ALQ_var",    Transformation = "After log1p", alq_after),
  cbind(Variable = "SMQ_var",    Transformation = "Before",      smq_before),
  cbind(Variable = "SMQ_var",    Transformation = "After log1p", smq_after),
  cbind(Variable = "DPQ_var",    Transformation = "None",        dpq_before),
  cbind(Variable = "TRIGLY_var", Transformation = "None",        trig_before),
  cbind(Variable = "BPXO_var",   Transformation = "None",        bpxo_before)
)

View(descriptive_results)
descriptive_results



models <- list(
  "Model 1" = lm(log_HSCRP ~ log_SMQ, data = analysis_df),
  "Model 2" = lm(log_HSCRP ~ log_SMQ + log_ALQ, data = analysis_df),
  "Model 3" = lm(log_HSCRP ~ log_SMQ + log_ALQ + DPQ_var, data = analysis_df),
  "Model 4" = lm(log_HSCRP ~ log_SMQ + log_ALQ + DPQ_var + TRIGLY_var, data = analysis_df),
  "Model 5" = lm(log_HSCRP ~ log_SMQ + log_ALQ + DPQ_var + TRIGLY_var + BPXO_var, data = analysis_df)
)

predictors <- c("log_SMQ", "log_ALQ", "DPQ_var", "TRIGLY_var", "BPXO_var")

predictor_labels <- c(
  "Smoking",
  "Alcohol",
  "Depression",
  "Triglycerides",
  "Blood_Pressure"
)

get_f_p <- function(model) {
  f <- summary(model)$fstatistic
  pf(f[1], f[2], f[3], lower.tail = FALSE)
}

regression_results <- data.frame(
  Model = names(models),
  Smoking_Coefficient = NA,
  Smoking_p_value = NA,
  Alcohol_Coefficient = NA,
  Alcohol_p_value = NA,
  Depression_Coefficient = NA,
  Depression_p_value = NA,
  Triglycerides_Coefficient = NA,
  Triglycerides_p_value = NA,
  Blood_Pressure_Coefficient = NA,
  Blood_Pressure_p_value = NA,
  R2 = NA,
  F_Significance = NA
)

for (i in seq_along(models)) {
  model_summary <- summary(models[[i]])
  coefs <- model_summary$coefficients
  
  for (j in seq_along(predictors)) {
    pred <- predictors[j]
    
    if (pred %in% rownames(coefs)) {
      regression_results[i, paste0(predictor_labels[j], "_Coefficient")] <- coefs[pred, "Estimate"]
      regression_results[i, paste0(predictor_labels[j], "_p_value")] <- coefs[pred, "Pr(>|t|)"]
    }
  }
  
  regression_results$R2[i] <- model_summary$r.squared
  regression_results$F_Significance[i] <- get_f_p(models[[i]])
}

regression_results_rounded <- regression_results
regression_results_rounded[, -1] <- round(regression_results_rounded[, -1], 4)

View(regression_results_rounded)
regression_results_rounded



wb <- createWorkbook()
addWorksheet(wb, "Regression Summary")

writeData(wb, "Regression Summary", x = regression_results_rounded, startRow = 3, startCol = 1)

writeData(wb, "Regression Summary", "Multiple Linear Regression Summary", startRow = 1, startCol = 1)
mergeCells(wb, "Regression Summary", cols = 1:13, rows = 1)

header_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom"
)

title_style <- createStyle(
  textDecoration = "bold",
  fontSize = 14,
  halign = "center"
)

addStyle(wb, "Regression Summary", title_style, rows = 1, cols = 1)
addStyle(wb, "Regression Summary", header_style, rows = 3, cols = 1:13, gridExpand = TRUE)

setColWidths(wb, "Regression Summary", cols = 1:13, widths = "auto")

saveWorkbook(
  wb,
  file = "D:/Stat_Project_2/regression_summary_table.xlsx",
  overwrite = TRUE
)



model5 <- lm(
  log_HSCRP ~ log_SMQ + log_ALQ + DPQ_var + TRIGLY_var + BPXO_var,
  data = analysis_df
)

summary(model5)



residuals_final <- residuals(model5)
fitted_final <- fitted(model5)

summary(residuals_final)

hist(
  residuals_final,
  breaks = 30,
  main = "Histogram of Residuals",
  xlab = "Residuals"
)

plot(
  fitted_final,
  residuals_final,
  main = "Residuals vs Fitted",
  xlab = "Fitted Values",
  ylab = "Residuals"
)
abline(h = 0, lty = 2)

qqnorm(residuals_final)
qqline(residuals_final, lty = 2)

plot(model5, which = 3)
plot(model5, which = 5)

par(mfrow = c(2, 2))
plot(model5)
par(mfrow = c(1, 1))

cooks_d <- cooks.distance(model5)

plot(
  cooks_d,
  type = "h",
  main = "Cook's Distance",
  ylab = "Cook's distance"
)

abline(h = 4 / nobs(model5), lty = 2)

which(cooks_d > 4 / nobs(model5))



predictor_data <- analysis_df[, c("log_SMQ", "log_ALQ", "DPQ_var", "TRIGLY_var", "BPXO_var")]

cor(predictor_data, use = "complete.obs")
round(cor(predictor_data, use = "complete.obs"), 3)

pairs(predictor_data)

vif(model5)



interaction_vars <- c("log_SMQ", "log_ALQ", "DPQ_var", "TRIGLY_var", "BPXO_var")

interaction_results <- data.frame()

for (i in 1:(length(interaction_vars) - 1)) {
  for (j in (i + 1):length(interaction_vars)) {
    
    v1 <- interaction_vars[i]
    v2 <- interaction_vars[j]
    
    formula_text <- paste(
      "log_HSCRP ~",
      v1, "+", v2, "+", paste0(v1, ":", v2)
    )
    
    mod <- lm(as.formula(formula_text), data = analysis_df)
    s <- summary(mod)
    
    interaction_name <- paste0(v1, ":", v2)
    
    pval <- coef(s)[interaction_name, "Pr(>|t|)"]
    beta <- coef(s)[interaction_name, "Estimate"]
    
    interaction_results <- rbind(
      interaction_results,
      data.frame(
        Var1 = v1,
        Var2 = v2,
        Interaction_Beta = beta,
        P_Value = pval
      )
    )
  }
}

interaction_results <- interaction_results[order(interaction_results$P_Value), ]

View(interaction_results)
print(interaction_results)



ls()