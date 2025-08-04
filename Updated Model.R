install.packages("ROSE")  # For oversampling, undersampling, and SMOTE
install.packages("caret") # For resampling methods
install.packages("DMwR")  # For SMOTE
library(ROSE)
library(caret)
library(DMwR)
table(y1_ready$binary)  # Replace 'binary' with the name of your target column
prop.table(table(y1_ready$binary))  # Check percentage distribution



# Oversample the minority class
oversampled_data <- ovun.sample(binary ~ ., data = train_data, method = "over", 
                                N = 2 * nrow(train_data[train_data$binary == 0, ]))$data

# Check the new class distribution
table(oversampled_data$binary)
 
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization
oversampled_data[ , -which(names(oversampled_data) == "target")] <- 
  lapply(oversampled_data[ , -which(names(oversampled_data) == "target")], normalize)


################new model##########
formula <- class ~ predictor1 + predictor2 + predictor3


##new model
library(nnet)

newnn_model <- nnet(
  formula,
  data = oversampled_data,
  size = 43,         # Number of neurons in the hidden layer
  linout = FALSE,   # Nonlinear output for binary classification
  maxit = 10000      # Maximum iterations
)

test_inputs <- test_data %>% select(-binary)
predictions <- predict(newnn_model, test_inputs, type = "raw")
predicted_classes <- ifelse(predictions > 0.8, 1, 0)
accuracy <- mean(predicted_classes == test_data$binary)
print(paste("Accuracy:", accuracy))
# Make predictions
predictions <- compute(newnn_model, test_data %>% select(-binary))$net.result
predicted_classes <- ifelse(predictions > 0.7, 1, 0)

# Create a confusion matrix
conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$binary)
print(conf_matrix)

# Calculate metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")
plot(newnn_model)
install.packages("NeuralNetTools")
library(NeuralNetTools)
plotnet(newnn_model)
