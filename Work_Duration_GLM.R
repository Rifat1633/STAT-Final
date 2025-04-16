# Load necessary libraries
library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation
library(car)     # For VIF calculation
library(ggplot2) # For visualization
library(MASS)    # For Stepwise Selection

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

# Ensure WDURHR has only positive values before using log-link GLM
if (any(data$WDURHR <= 0)) {
  data <- data %>% filter(WDURHR > 0)  # Remove zero or negative values
}

# Create a histogram of WDURHR
ggplot(data, aes(x = WDURHR)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue", color = "black", alpha = 0.6) +
  labs(title = "Distribution of WDURHR", x = "Work Duration (Hours)", y = "Frequency") +
  theme_minimal(base_size = 15) + 
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12)) 

# Fit the initial GLM with log link function
glm_model <- glm(WDURHR ~ CHOREHR + RECHR + OTHERHR + HOMEHR + PBHR + Gender + AGEGROUP + 
                   EDULEVEL + EMP + INCOMELEVEL + LICENSE + TPASS + VEHNUMLVL + BICYCLELVL + 
                   HOMEOWNER + LOCYRLVL + HHMEMLVL + HHHASCHLD + TRIPS + HOMELOC + BUS5KM + 
                   NEARMALLKM + NEARSCHKM + NEARGROCKM + NEARCBDKM + NEARRESTAUKM + LANDUSE, 
                 family = gaussian(link = "log"), data = data)

# Perform stepwise regression on GLM using AIC
stepwise_glm <- step(glm_model, direction = "both", trace = 1)

# Summarize the final stepwise-selected GLM model
summary(stepwise_glm)

# Check for multicollinearity using VIF (for GLM, we use standard `lm()` model)
vif(glm_model)  # Note: VIF should be interpreted with caution in GLM

# Residual diagnostics
par(mfrow = c(2, 2))
plot(stepwise_glm)

# Histogram of residuals after GLM with log link
hist(resid(stepwise_glm), breaks = 30, main = "Histogram of Residuals (GLM with Log Link)", 
     col = "skyblue", border = "black")

# Calculate and report model fit statistics
cat("\n=== MODEL FIT STATISTICS ===\n")

# 1. McFadden's Pseudo R-squared
null_model <- glm(WDURHR ~ 1, family = gaussian(link = "log"), data = data)
mcfadden_r2 <- 1 - (logLik(stepwise_glm)/logLik(null_model))
cat("McFadden's R-squared:", round(mcfadden_r2, 4), "\n")

# 2. Adjusted McFadden's R-squared
n <- nobs(stepwise_glm)
k <- length(coef(stepwise_glm)) - 1
adj_mcfadden <- 1 - ((logLik(stepwise_glm) - k)/logLik(null_model))
cat("Adjusted McFadden's R-squared:", round(adj_mcfadden, 4), "\n")

# 3. AIC and BIC
cat("AIC:", round(AIC(stepwise_glm), 2), "\n")
cat("BIC:", round(BIC(stepwise_glm), 2), "\n")

# 4. Residual Deviance and Null Deviance
cat("Residual Deviance:", deviance(stepwise_glm), "\n")
cat("Null Deviance:", deviance(null_model), "\n")
cat("Deviance Explained:", 
    round((deviance(null_model) - deviance(stepwise_glm))/deviance(null_model), 4), "\n")

# 5. Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(residuals(stepwise_glm, type = "response")^2))
cat("RMSE:", round(rmse, 4), "\n")

# 6. Mean Absolute Error (MAE)
mae <- mean(abs(residuals(stepwise_glm, type = "response")))
cat("MAE:", round(mae, 4), "\n")

# Check for multicollinearity using VIF
cat("\n=== MULTICOLLINEARITY CHECK ===\n")
vif_results <- car::vif(stepwise_glm)
print(vif_results)

# Residual diagnostics
cat("\n=== RESIDUAL DIAGNOSTICS ===\n")
par(mfrow = c(2, 2))
plot(stepwise_glm)

# Histogram of residuals
hist(resid(stepwise_glm), breaks = 30, 
     main = "Residual Distribution", 
     xlab = "Residuals", 
     col = "skyblue", 
     border = "black")
