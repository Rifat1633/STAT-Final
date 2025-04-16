# Load necessary libraries
library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation
library(MASS)    # For robust regression (rlm) and other helpers

# Load the data
data <- read_excel("C:/Onedrive_Sync/OneDrive - Dalhousie University/PhD Dal/Coursework/Data Analysis/Project/Analysis Files/Workdata.xlsx")

# Check the structure of the data
str(data)

# Convert categorical variables to factors
categorical_vars <- c("Gender", "AGEGROUP", "EDULEVEL", "EMP", "INCOMELEVEL", "LICENSE", "TPASS", 
                      "VEHNUMLVL", "BICYCLELVL", "HOMEOWNER", "LOCYRLVL", "HHMEMLVL", "HHHASCHLD", 
                      "HOMELOC", "BUS5KM")

data <- data %>%
  mutate(across(all_of(categorical_vars), as.factor))

# Check for missing values
colSums(is.na(data))

# Handle missing values (listwise deletion)
data <- na.omit(data)  # Remove rows with any missing values

# Prepare the model formula
model_formula <- WDURHR ~ CHOREHR + RECHR + OTHERHR + HOMEHR + PBHR + Gender + AGEGROUP + 
  EDULEVEL + EMP + INCOMELEVEL + LICENSE + TPASS + VEHNUMLVL + BICYCLELVL + 
  HOMEOWNER + LOCYRLVL + HHMEMLVL + HHHASCHLD + TRIPS + HOMELOC + BUS5KM + 
  NEARMALLKM + NEARSCHKM + NEARGROCKM + NEARCBDKM + NEARRESTAUKM + LANDUSE

# Initial model using ordinary least squares (OLS)
ols_model <- lm(model_formula, data = data)

# Get the initial coefficients and residuals
coefficients_ols <- coef(ols_model)
residuals_ols <- residuals(ols_model)

# Function for Huber weighting (robust weight function)
huber_weight <- function(residuals, k = 1.5) {
  # Huber weight function
  abs_resid <- abs(residuals)
  weights <- ifelse(abs_resid <= k, 1, k / abs_resid)
  return(weights)
}

# Maximum number of iterations for IRLS
max_iter <- 100
tolerance <- 1e-6

# Initialize residuals and weights
residuals_current <- residuals_ols
weights_current <- huber_weight(residuals_current)

# Start IRLS algorithm
for (i in 1:max_iter) {
  # Weighted least squares regression with current weights
  weighted_model <- lm(model_formula, data = data, weights = weights_current)
  
  # Update coefficients and residuals
  coefficients_current <- coef(weighted_model)
  residuals_current <- residuals(weighted_model)
  
  # Calculate new weights using the Huber function
  weights_current <- huber_weight(residuals_current)
  
  # Check for convergence: if the coefficients change very little, stop the iteration
  if (sum((coefficients_current - coefficients_ols)^2) < tolerance) {
    cat("Convergence reached at iteration", i, "\n")
    break
  }
  
  # Update the coefficients for the next iteration
  coefficients_ols <- coefficients_current
}

# Final model summary after IRLS
cat("\n=== Final Robust Regression (IRLS) Model Summary ===\n")
summary(weighted_model)

# Model fit statistics
cat("\n=== MODEL FIT STATISTICS ===\n")
# AIC and BIC for the IRLS model
cat("AIC:", round(AIC(weighted_model), 2), "\n")
cat("BIC:", round(BIC(weighted_model), 2), "\n")

# RMSE (Root Mean Squared Error) for the IRLS model
rmse_irls <- sqrt(mean(residuals(weighted_model)^2))
cat("RMSE:", round(rmse_irls, 4), "\n")

# MAE (Mean Absolute Error) for the IRLS model
mae_irls <- mean(abs(residuals(weighted_model)))
cat("MAE:", round(mae_irls, 4), "\n")

# Check for multicollinearity using Variance Inflation Factor (VIF)
cat("\n=== MULTICOLLINEARITY CHECK ===\n")
vif_results <- car::vif(weighted_model)
print(vif_results)

# Diagnostic plots for the final IRLS model
par(mfrow = c(1, 1))
plot(weighted_model)
