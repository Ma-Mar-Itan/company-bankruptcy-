############Step 1: Load Necessary Libraries############
# Load the neuralnet package
install.packages("neuralnet")
library(neuralnet)
library(dplyr)    # For data manipulation
library(ggplot2)  # For custom visualizations (optional)
install.packages("pROC")
library(pROC)
##########Check and rename col####
str(y1_ready)           # Check structure of the dataset
summary(y1_ready)       # Get a summary of the dataset
anyNA(y1_ready)         # Check for missing values
colnames(y1_ready)[ncol(y1_ready)] <- "binary"  # Rename the last column to 'binary'


#########prepare the data###########
set.seed(123)  # Set a seed for reproducibility

# Create an 80-20 train-test split
train_index <- sample(1:nrow(y1_ready), 0.8 * nrow(y1_ready))
train_data <- y1_ready[train_index, ]
test_data <- y1_ready[-train_index, ]
# Normalize function
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply normalization to all columns
train_data <- train_data %>%
  mutate(across(everything(), normalize))

test_data <- test_data %>%
  mutate(across(everything(), normalize))

##########Step 5: Define the Model Formula#####
# Create formula for neuralnet
predictors <- setdiff(names(train_data), "binary")
formula <- as.formula(paste("binary ~", paste(predictors, collapse = " + ")))
# Train the neural network
nn_model <- neuralnet(
  formula, 
  data = train_data, 
  hidden = c(10, 5),         # Two hidden layers with 10 and 5 neurons
  act.fct = "logistic",      # Logistic activation function for classification
  linear.output = FALSE,     # For binary classification
  stepmax = 1e5              # Maximum steps for convergence
)

# Visualize the neural network
plot(nn_model)
plot(nn_model, rep = "best", show.weights = FALSE)


###############MAKE PREDICTIONS##########
# Make predictions
predictions <- compute(nn_model, test_data %>% select(-binary))$net.result
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
actual <- test_data$binary[1:length(predicted_classes)]  # Adjust actual to match predictions
length(predicted_classes)  # Length of predictions
length(actual)             # Length of actual values
head(test_data$binary)
# Ensure test_data and predictions align
# Confusion Matrix
actual <- test_data$binary
conf_matrix <- table(Predicted = predicted_classes, Actual = actual)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Additional metrics: Precision, Recall, F1-Score
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")

library(pROC)

roc_obj <- roc(actual, predictions)
plot(roc_obj, col = "blue", main = "ROC Curve")
auc(roc_obj)

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

train_data <- train_data %>%
  mutate(across(where(is.numeric) & !all_of("binary"), normalize))
test_data <- test_data %>%
  mutate(across(where(is.numeric) & !all_of("binary"), normalize))
