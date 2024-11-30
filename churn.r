# Load required libraries
library(ggplot2)
library(dplyr)
library(pROC) # For ROC curve

# Assuming `data` is your preprocessed dataset
data <- read.csv("C:/Users/DELL/Downloads/customer_churn.csv") %>% 
  select(-customerID) %>% 
  drop_na() %>% 
  mutate(Churn = as.factor(Churn))

# Normalize numeric columns for consistent analysis
data$tenure <- scale(data$tenure)
data$MonthlyCharges <- scale(data$MonthlyCharges)
data$TotalCharges <- as.numeric(gsub(" ", NA, data$TotalCharges))
data$TotalCharges[is.na(data$TotalCharges)] <- median(data$TotalCharges, na.rm = TRUE)
data$TotalCharges <- scale(data$TotalCharges)

### 1. Histogram for Monthly Charges Distribution
ggplot(data, aes(x = MonthlyCharges)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Distribution of Monthly Charges", x = "Monthly Charges", y = "Count") +
  theme_minimal()

### 2. Bar Plot for Churn Count
ggplot(data, aes(x = Churn)) +
  geom_bar(fill = c("steelblue", "tomato")) +
  labs(title = "Churn Count", x = "Churn", y = "Count") +
  theme_minimal()

### 3. Box Plot of Tenure by Churn
ggplot(data, aes(x = Churn, y = tenure, fill = Churn)) +
  geom_boxplot() +
  labs(title = "Tenure by Churn Status", x = "Churn", y = "Tenure") +
  theme_minimal() +
  scale_fill_manual(values = c("tomato", "steelblue"))

### 4. Heatmap for Correlation Matrix
numeric_data <- data %>% select(where(is.numeric))
cor_matrix <- cor(numeric_data)
cor_melted <- reshape2::melt(cor_matrix)

ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-1, 1)) +
  labs(title = "Correlation Heatmap", x = "Features", y = "Features") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 5. ROC Curve for Model Evaluation (e.g., Logistic Regression)
# Split data into train and test (using previously defined trainData and testData)
log_model <- glm(Churn ~ ., data = trainData, family = "binomial")
log_prob <- predict(log_model, testData, type = "response")
roc_curve <- roc(testData$Churn, as.numeric(log_prob))

plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve for Logistic Regression")
