y1test <- y1maindata

#trying extreme boosting example(
data(y1test)
# Separate features and target
x_data <- as.matrix(y1test[,-65])  
y_data <- as.numeric(y1test[, 65]) 

#The xgb.DMatrix is an optimized structure for XGBoost to handle data efficiently.
dtrain <- xgb.DMatrix(data = x_data, label = y_data)

# Define the parameters for binary classification
params <- list(
  objective = "binary:logistic",  # Binary classification objective
  eta = 0.1,                     # Learning rate
  max_depth = 6,                 # Maximum depth of trees
  eval_metric = "logloss"        # Log loss evaluation metric
)




#This is to transform the outcome data to 0 and 1 only 
unique(target)  # Should return only 0 and 1
target <- ifelse(y1test[, 65] == "yes", 1, 0)
unique(y1test[, 65])

# Set the number of boosting iterations
nrounds <- 50

dtrain <- xgb.DMatrix(data = as.matrix(y1test[, -65]), label = target)
# Train the model
model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = nrounds,
  verbose = 1
)
