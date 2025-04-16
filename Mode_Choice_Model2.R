# Load necessary libraries
library(readxl)        # For reading Excel files
library(dplyr)         # For data manipulation
library(nnet)          # For multinomial logistic regression
library(broom)         # For tidy model output
library(kableExtra)    # For nice table formatting
library(MASS)          # For stepAIC function
library(ggplot2)       # For visualizations

# Load and prepare data
data <- read_excel("C:/Onedrive_Sync/OneDrive - Dalhousie University/PhD Dal/Coursework/Data Analysis/Project/Analysis Files/WorkMode.xlsx")

# Convert categorical variables to factors
categorical_vars <- c("Gender", "AGEGROUP", "EDULEVEL", "EMP", "INCOMELEVEL", "LICENSE", "TPASS", 
                      "VEHNUMLVL", "BICYCLELVL", "HOMEOWNER", "LOCYRLVL", "MODE", "HHMEMLVL", "HHHASCHLD", 
                      "HOMELOC", "BUS5KM")

data <- data %>%
  mutate(across(all_of(categorical_vars), as.factor)) %>%
  na.omit()

# Check initial class distribution
cat("Original class distribution:\n")
print(table(data$MODE))

# Plot original class distribution
ggplot(data, aes(x = MODE)) + 
  geom_bar() +
  labs(title = "Original Class Distribution", x = "Mode Choice", y = "Count")

# 1. Simple Oversampling (Random Replication) ----------------------------

# Set target size for each class (desired number of samples, e.g., 333)
target_size <- 333

# Initialize balanced dataset
balanced_data <- data.frame()

# Apply oversampling for each class
set.seed(123)
for (class in levels(data$MODE)) {
  current_class <- data %>% filter(MODE == class)
  current_count <- nrow(current_class)
  
  if (current_count < target_size) {
    # Calculate how many samples are needed to reach target_size
    needed_samples <- target_size - current_count
    oversampled_class <- current_class[sample(1:current_count, size = needed_samples, replace = TRUE), ]
    
    # Combine with original class
    balanced_class <- rbind(current_class, oversampled_class)
  } else {
    # For majority class, just take a sample of size target_size
    balanced_class <- current_class %>%
      sample_n(size = target_size, replace = FALSE)
  }
  
  balanced_data <- rbind(balanced_data, balanced_class)
}

# Verify balanced class distribution
cat("\nClass distribution after oversampling:\n")
print(table(balanced_data$MODE))

# Plot balanced class distribution
ggplot(balanced_data, aes(x = MODE)) + 
  geom_bar() +
  labs(title = "Balanced Class Distribution After Oversampling", x = "Mode Choice", y = "Count")

# 2. Model Development ----------------------------------------------------
balanced_data$MODE <- relevel(balanced_data$MODE, ref = "3")

# Fit initial null model
null_model <- multinom(MODE ~ 1, data = balanced_data, trace = FALSE)

# Fit full model with carefully selected predictors
full_model <- multinom(MODE ~ Gender + AGEGROUP + EDULEVEL + EMP + 
                         INCOMELEVEL + LICENSE + VEHNUMLVL + HOMELOC + 
                         TRIPS + NEARCBDKM,
                       data = balanced_data,
                       maxit = 1000,
                       trace = FALSE)

# Verify convergence
if(full_model$convergence != 0) {
  full_model <- multinom(formula(full_model), data = balanced_data, 
                         maxit = 5000, trace = FALSE)
}

# 3. Stepwise Selection ---------------------------------------------------
safe_scope <- list(
  lower = formula(null_model),
  upper = formula(full_model)
)

step_model <- tryCatch({
  stepAIC(null_model, 
          scope = safe_scope,
          direction = "forward",
          trace = FALSE)
}, error = function(e) {
  return(full_model)
})

# 4. Model Evaluation -----------------------------------------------------
results_table <- broom::tidy(step_model, conf.int = TRUE) %>%
  mutate(
    t.stat = estimate / std.error,
    p.value = 2 * (1 - pnorm(abs(t.stat))),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    odds.ratio = exp(estimate)
  ) %>% 
  select(
    y.level, term, 
    Coefficient = estimate, 
    `Std. Error` = std.error,
    `Odds Ratio` = odds.ratio,
    `t-stat` = t.stat,
    `p-value` = p.value,
    significance
  )

# Print results
results_table %>%
  kable(digits = 3, align = c("l", "l", "r", "r", "r", "r", "r", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE) %>%
  pack_rows(index = table(results_table$y.level)) %>%
  add_header_above(c(" " = 2, "Stepwise Multinomial Logit Results" = 6)) %>%
  footnote(
    general = "Reference category for MODE is level 3",
    symbol = c("*** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1")
  )

# Model statistics
cat("\nFinal Model Fit Statistics:\n")
cat("AIC:", AIC(step_model), "\n")
cat("BIC:", BIC(step_model), "\n")
cat("Log-Likelihood:", logLik(step_model), "\n")
mcfadden_r2 <- 1 - (logLik(step_model)/logLik(null_model))
cat("McFadden's R-squared:", mcfadden_r2, "\n")

# Confusion matrix
predicted_classes <- predict(step_model)
confusion_matrix <- table(Actual = balanced_data$MODE, Predicted = predicted_classes)
cat("\nConfusion Matrix:\n")
print(confusion_matrix)
cat("\nOverall Accuracy:", mean(predicted_classes == balanced_data$MODE), "\n")

# Save results
saveRDS(list(
  data = balanced_data,
  models = list(null = null_model, full = full_model, final = step_model),
  results = results_table,
  confusion = confusion_matrix
), "mode_choice_results.rds")
