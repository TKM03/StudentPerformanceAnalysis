# Load required packages
library(tidyverse)
library(summarytools)
library(corrplot)

# Load the data
data <- read.csv("Student_Performance_on_an_Entrance_Examination.csv", stringsAsFactors = FALSE)

# Verify the data loaded correctly
head(data)
nrow(data)  # Should be 666

#<<================================DATA CLEANING========================================================>>

# Convert categorical variables to factors
data <- data %>%
  mutate(across(c(Gender, Caste, coaching, Class_ten_education, twelve_education, 
                  medium, Father_occupation, Mother_occupation, time), as.factor))

# Convert performance levels to numeric for analysis (ordinal scale) with case-insensitive matching
data <- data %>%
  mutate(
    Class_X_Percentage_num = case_when(
      tolower(Class_X_Percentage) == "excellent" ~ 95,
      tolower(Class_X_Percentage) == "vg" ~ 85,
      tolower(Class_X_Percentage) == "good" ~ 75,
      tolower(Class_X_Percentage) == "average" ~ 65,
      TRUE ~ NA_real_
    ),
    Class_XII_Percentage_num = case_when(
      tolower(Class_XII_Percentage) == "excellent" ~ 95,
      tolower(Class_XII_Percentage) == "vg" ~ 85,
      tolower(Class_XII_Percentage) == "good" ~ 75,
      tolower(Class_XII_Percentage) == "average" ~ 65,
      TRUE ~ NA_real_
    ),
    Performance_num = case_when(
      tolower(Performance) == "excellent" ~ 95,
      tolower(Performance) == "vg" ~ 85,
      tolower(Performance) == "good" ~ 75,
      tolower(Performance) == "average" ~ 65,
      TRUE ~ NA_real_
    )
  )

# Check for missing values
colSums(is.na(data))

# Keep original categorical columns as factors for EDA
data <- data %>%
  mutate(across(c(Class_X_Percentage, Class_XII_Percentage, Performance), as.factor))

# Create a cleaned dataset (no NAs in key columns)
data_clean <- data %>% filter(!is.na(Performance_num))
nrow(data_clean)  # Should still be 666 unless there are unexpected NAs

#<<===================================EDA===============================================>>

# Basic summary
summary(data_clean)

# Detailed summary
print(dfSummary(data_clean), method = "render")

# Distribution of Performance
ggplot(data_clean, aes(x = Performance)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Entrance Exam Performance", x = "Performance", y = "Count") +
  theme_minimal()

# Performance by Gender
ggplot(data_clean, aes(x = Gender, fill = Performance)) +
  geom_bar(position = "dodge") +
  labs(title = "Performance by Gender", x = "Gender", y = "Count", fill = "Performance") +
  theme_minimal()

# Performance by Coaching
ggplot(data_clean, aes(x = coaching, fill = Performance)) +
  geom_bar(position = "dodge") +
  labs(title = "Performance by Coaching", x = "Coaching", y = "Count", fill = "Performance") +
  theme_minimal()

# Check for NAs in numeric columns
sum(is.na(data_clean$Class_XII_Percentage_num))
sum(is.na(data_clean$Performance_num))

# Check unique values
unique(data_clean$Class_XII_Percentage_num)
unique(data_clean$Performance_num)

# Ensure data is clean and numeric columns are populated
ggplot(data_clean, aes(x = Class_XII_Percentage_num, y = Performance_num)) +
  geom_point(color = "blue", alpha = 0.5, size = 2) +  # Increase point size for visibility
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Class XII Percentage vs. Performance", 
       x = "Class XII Percentage (Numeric)", y = "Performance (Numeric)") +
  theme_minimal()

# Select numeric columns (explicitly use dplyr::select to avoid conflicts)
numeric_data <- data_clean %>% dplyr::select(Class_X_Percentage_num, Class_XII_Percentage_num, Performance_num)

# Check the numeric data
head(numeric_data)
summary(numeric_data)

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Check the correlation matrix
print(cor_matrix)

# Visualize with a different method for clarity
corrplot(cor_matrix, method = "number", type = "upper", 
         title = "Correlation Between Numeric Variables", 
         mar = c(0,0,2,0), diag = FALSE)

# Alternative: Use "shade" method
corrplot(cor_matrix, method = "shade", type = "upper", 
         title = "Correlation Between Numeric Variables", 
         mar = c(0,0,2,0), diag = FALSE)
#<<==================================Statistical  Modeling==============================================>>

# Fit linear regression
model <- lm(Performance_num ~ Class_XII_Percentage_num + coaching + Gender, data = data_clean)

# Summary
summary(model)

# Plot predicted vs. actual
data_clean$predicted <- predict(model)
ggplot(data_clean, aes(x = Performance_num, y = predicted)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs. Predicted Performance", x = "Actual", y = "Predicted") +
  theme_minimal()


# Install and load package for ordinal regression
# install.packages("MASS")
library(MASS)

# Fit ordinal logistic regression
ordinal_model <- polr(Performance ~ Class_XII_Percentage + coaching + Gender, 
                      data = data_clean, Hess = TRUE)

# Summary
summary(ordinal_model)

# Predicted probabilities
data_clean$pred_prob <- predict(ordinal_model, type = "class")
table(data_clean$Performance, data_clean$pred_prob)  # Confusion matrix

#<<==========================================Socioeconomic Influence====================================>>

ggplot(data_clean, aes(x = Father_occupation, fill = Performance)) +
  geom_bar(position = "dodge") +
  labs(title = "Performance by Father’s Occupation", x = "Father’s Occupation", y = "Count", fill = "Performance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data_clean, aes(x = Mother_occupation, fill = Performance)) +
  geom_bar(position = "dodge") +
  labs(title = "Performance by Mother’s Occupation", x = "Mother’s Occupation", y = "Count", fill = "Performance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



