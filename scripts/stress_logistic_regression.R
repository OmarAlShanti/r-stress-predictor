library(glm2)
library(tidyverse)
library(ggplot2)
library(dplyr)
rm(list=ls()) 

# Run from the repo root (r-stress-predictor):  source("scripts/stress_logistic_regression.R")
data <- read.csv("data/StressLevelDataset.csv")
head(data)

# Removing duplicates and null values
data <- data[!duplicated(data), ]
data <- na.omit(data)

# Convert categorical columns to appropriate data types
data$mental_health_history <- as.factor(data$mental_health_history)

# Normalize or standardize column: anxiety_level
data$anxiety_level <- scale(data$anxiety_level)
head(data)
str(data)

#Max of all the columns which are numeric
numeric_cols <- sapply(data, is.numeric)
max_values <- sapply(data[numeric_cols], max, na.rm = TRUE)
max_values_df <- as.data.frame(max_values)
print(max_values_df)

#Columns who's max vals are less than 5
columns_max_less_than_5 <- names(max_values[max_values < 5])
print(columns_max_less_than_5)

#Min of all the columns which are numeric
numeric_cols <- sapply(data, is.numeric)
min_values <- sapply(data[numeric_cols], min, na.rm = TRUE)
min_values_df <- as.data.frame(min_values)
print(min_values_df)

# Assuming your data frame is named 'data'

# Identify numeric columns
numeric_cols <- sapply(data, is.numeric)

# Subset data to include only numeric columns
data_numeric <- data[numeric_cols]

# Calculate the correlation matrix
numeric_cols <- sapply(data, is.numeric)
data_numeric <- data[numeric_cols]
correlation_matrix <- cor(data_numeric, use = "complete.obs")
correlation_df <- as.data.frame(correlation_matrix)
print(correlation_df)

##__________
# Identify numeric columns
numeric_cols <- sapply(data, is.numeric)
# Subset data to include only numeric columns
data_numeric <- data[numeric_cols]
# Calculate the correlation matrix for the numeric columns
correlation_matrix <- cor(data_numeric)
# Extract the correlations of 'stress_level' with other variables
stress_level_correlations <- correlation_matrix["stress_level", ]
# Creating a DataFrame to display the correlations
correlation_df <- as.data.frame(stress_level_correlations)
# Printing the DataFrame
print(correlation_df)
##__________

#___________/
# Calculate the mean of the 'stress_level' column
mean_stress_level <- mean(data$stress_level)

# Convert 'stress_level' to a binary factor variable
# It will be 1 if 'stress_level' is greater than the mean, 0 otherwise
data$stress_level_factor <- factor(ifelse(data$stress_level > mean_stress_level, 1, 0), 
                                   levels = c(0, 1))

# Simplify the model by selecting a subset of predictors
# Replace 'predictor1', 'predictor2', ... with actual predictor variable names
# Logistic regression model with 'stress_level_factor' as dependent variable
logistic.model <- glm(stress_level_factor ~ anxiety_level + self_esteem + mental_health_history + 
                        depression + headache + blood_pressure + sleep_quality + 
                        breathing_problem + noise_level + living_conditions + 
                        safety + basic_needs + academic_performance + study_load + 
                        teacher_student_relationship + future_career_concerns + 
                        social_support + peer_pressure + extracurricular_activities + 
                        bullying, data=data, family="binomial")

# Display summary of the logistic regression model
summary(logistic.model)


# Converting coefficients to Odds Ratios
odds_ratios <- exp(coef(logistic.model))

# Calculating 95% Confidence Interval for the Odds Ratios
conf_int <- exp(confint.default(logistic.model))

# Combining Odds Ratios and their Confidence Intervals
combined <- cbind(OddsRatio = odds_ratios, conf_int)
combined


##Top 5 variables effecting Y-Variable(Stress_level) are
#Study Load
#Self Esteem
#Bullying
#Sleep Quality
#Future Career Concerns

###visualization for the top 5 variables
library(ggplot2)
library(dplyr)
library(reshape2) # For melting data frames

# Plotting each of the top 5 variables
ggplot(data, aes(x = stress_level_factor, y = study_load)) + 
  geom_boxplot() + 
  ggtitle("Study Load vs Stress Level")

ggplot(data, aes(x = stress_level_factor, y = self_esteem)) + 
  geom_boxplot() + 
  ggtitle("Self Esteem vs Stress Level")

ggplot(data, aes(x = stress_level_factor, y = bullying)) + 
  geom_boxplot() + 
  ggtitle("Bullying vs Stress Level")

ggplot(data, aes(x = stress_level_factor, y = sleep_quality)) + 
  geom_boxplot() + 
  ggtitle("Sleep Quality vs Stress Level")

ggplot(data, aes(x = stress_level_factor, y = future_career_concerns)) + 
  geom_boxplot() + 
  ggtitle("Future Career Concerns vs Stress Level")

# Preparing data for heatmap
selected_vars <- data[, c("stress_level", "study_load", "self_esteem", "bullying", 
                          "sleep_quality", "future_career_concerns")]
cor_matrix <- cor(selected_vars, use = "complete.obs")

# Melting the correlation matrix for heatmap
melted_cor_matrix <- melt(cor_matrix)

# Drawing a heatmap
ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  ggtitle("Heatmap of Correlation Matrix")


#Rileys code - Decision Tree
library(rpart)
library(rpart.plot)
data$stress_level <- as.factor(data$stress_level)
cor(data)
data <- read.csv("StressLevelDataset.csv")
data <- data[!duplicated(data), ]
data <- na.omit(data)
data2<- data[,c("living_conditions","safety","basic_needs","academic_performance","study_load","teacher_student_relationship"
                ,"future_career_concerns","social_support","peer_pressure","extracurricular_activities"
                ,"bullying","stress_level")]
set.seed(256)
ind <- sample(2, nrow(data2), replace = TRUE, prob = c(0.8,0.2))
train <- data2[ind==1,]
test <- data2[ind==2,]
tree <- rpart(stress_level ~., data = train, method = "class", control = rpart.control(minsplit=10))
rpart.plot(tree)
predict <- predict(tree, test, type='class')
confusionMatrix(predict, test$stress_level, positive = 'y')





