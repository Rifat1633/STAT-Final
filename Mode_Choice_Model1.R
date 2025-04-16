# Load necessary libraries
library(readxl)
library(dplyr)
library(nnet)
library(broom)       # For tidy model output
library(kableExtra)  # For nice table formatting
library(MASS)        # For stepAIC function

# Explicitly specify dplyr's select function
select <- dplyr::select

# Load and prepare data
data <- read_excel("C:/Onedrive_Sync/OneDrive - Dalhousie University/PhD Dal/Coursework/Data Analysis/Project/Analysis Files/WorkMode.xlsx")

# Convert categorical variables to factors
categorical_vars <- c("Gender", "AGEGROUP", "EDULEVEL", "EMP", "INCOMELEVEL", "LICENSE", "TPASS", 
                      "VEHNUMLVL", "BICYCLELVL", "HOMEOWNER", "LOCYRLVL", "MODE", "HHMEMLVL", "HHHASCHLD", 
                      "HOMELOC", "BUS5KM")

data <- data %>%
  mutate(across(all_of(categorical_vars), as.factor)) %>%
  na.omit()

# Set reference level for MODE (response variable)
data$MODE <- relevel(data$MODE, ref = "3")  # Setting level 3 as reference

# Fit initial null model (intercept only)
null_model <- multinom(MODE ~ 1, data = data)

# Fit full model with all predictors
full_model <- multinom(MODE ~ Gender + AGEGROUP + 
                         EDULEVEL + EMP + INCOMELEVEL + LICENSE + TPASS + VEHNUMLVL + BICYCLELVL + 
                         HOMEOWNER + LOCYRLVL + HHMEMLVL + HHHASCHLD + TRIPS + HOMELOC + BUS5KM + 
                         NEARMALLKM + NEARSCHKM + NEARGROCKM + NEARCBDKM + NEARRESTAUKM + LANDUSE,
                       data = data,
                       maxit = 1000)

# Perform stepwise AIC selection (forward only)
step_model <- stepAIC(null_model, 
                      scope = list(lower = null_model, upper = full_model),
                      direction = "forward",
                      trace = TRUE)  # Set trace=TRUE to see selection process

# Create a tidy table of results from the final stepwise model
results_table <- tidy(step_model, conf.int = TRUE) %>%
  mutate(
    t.stat = estimate / std.error,
    p.value = 2 * (1 - pnorm(abs(t.stat))),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
  ) %>%
  select(
    y.level, term, 
    Coefficient = estimate, 
    `Std. Error` = std.error,
    `t-stat` = t.stat,
    `p-value` = p.value,
    significance
  )

# Print the formatted table
results_table %>%
  kable(digits = 3, align = c("l", "l", "r", "r", "r", "r", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE) %>%
  pack_rows(index = table(results_table$y.level)) %>%
  add_header_above(c(" " = 2, "Stepwise Multinomial Logit Results" = 5)) %>%
  footnote(
    general = "Reference category for MODE is level 4",
    symbol = c("*** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.1")
  )

# Relative risk ratios (exponentiated coefficients) for final model
rrr <- exp(coef(step_model))
print("Relative Risk Ratios for Final Model:")
print(rrr)

# Model fit statistics for final model
cat("\nFinal Model Fit Statistics:\n")
cat("AIC:", AIC(step_model), "\n")
cat("BIC:", BIC(step_model), "\n")
cat("Log-Likelihood:", logLik(step_model), "\n")

# McFadden's pseudo R-squared for final model
mcfadden_r2 <- 1 - (logLik(step_model)/logLik(null_model))
cat("McFadden's R-squared:", mcfadden_r2, "\n")

# Confusion matrix for final model
predicted_classes <- predict(step_model)
confusion_matrix <- table(Actual = data$MODE, Predicted = predicted_classes)
print("Confusion Matrix for Final Model:")
print(confusion_matrix)

# Calculate accuracy for final model
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
cat("Classification Accuracy:", round(accuracy, 3), "\n")