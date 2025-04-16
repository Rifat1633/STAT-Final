# Load necessary libraries
library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation
library(car)     # For VIF calculation

# Load the data
data <- read_excel("C:/Onedrive_Sync/OneDrive - Dalhousie University/PhD Dal/Coursework/Data Analysis/Project/Analysis Files/Workdata.xlsx")

# Check the structure of the data
str(data)

# Convert categorical variables to factors
categorical_vars <- c("Gender", "AGEGROUP", "EDULEVEL", "EMP", "INCOMELEVEL", "LICENSE", "TPASS", 
                      "VEHNUMLVL", "BICYCLELVL",  "HOMEOWNER", "LOCYRLVL", "HHMEMLVL", "HHHASCHLD", 
                      "HOMELOC", "BUS5KM")

data <- data %>%
  mutate(across(all_of(categorical_vars), as.factor))

# Check for missing values
colSums(is.na(data))

# Handle missing values (listwise deletion)
data <- na.omit(data)  # Remove rows with any missing values

# Fit the initial OLS regression model
# Dependent variable: WDURHR
# Independent variables: All other variables
full_model <- lm(WDURHR ~ CHOREHR + RECHR + OTHERHR + HOMEHR + PBHR + Gender + AGEGROUP + 
                   EDULEVEL + EMP + INCOMELEVEL + LICENSE + TPASS + VEHNUMLVL + BICYCLELVL + 
                   HOMEOWNER + LOCYRLVL + HHMEMLVL + HHHASCHLD + TRIPS + HOMELOC + BUS5KM + 
                   NEARMALLKM + NEARSCHKM + NEARGROCKM + NEARCBDKM + NEARRESTAUKM + LANDUSE, 
                 data = data)

# Perform stepwise regression using AIC as the criterion
stepwise_model <- step(full_model, direction = "both", trace = 1)

# Summarize the final stepwise model
summary(stepwise_model)

# Check for multicollinearity using Variance Inflation Factor (VIF)
vif(stepwise_model)

# Diagnostic plots for the final stepwise model
par(mfrow = c(2, 2))
plot(stepwise_model)