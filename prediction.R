# Prediction: trying out the autoML functionality from H2O

library(h2o)
library(lime)
library(tidyverse)

# read in validation set
features_and_labels <- read_rds("validation_set.rds")

h2o.init()

# Split data into Train/Validation/Test Sets
data_h2o <- as.h2o(features_and_labels)

split_h2o <- h2o.splitFrame(data_h2o, ratios = c(0.7, 0.15), seed = 1234 )

train_h2o <- h2o.assign(split_h2o[[1]], "train" ) # 70%
valid_h2o <- h2o.assign(split_h2o[[2]], "valid" ) # 15%
test_h2o  <- h2o.assign(split_h2o[[3]], "test" )  # 15%

train_h2o$isSTEM <- as.factor(train_h2o$isSTEM)
valid_h2o$isSTEM <- as.factor(valid_h2o$isSTEM)
test_h2o$isSTEM <- as.factor(test_h2o$isSTEM)

# Set names for h2o
# Target
y <- "isSTEM"
# Features
x <- features_and_labels %>% 
  select(SchoolId:Ln) %>% 
  colnames()

# Run the automated machine learning 
automl_models_h2o <- h2o.automl(
  x = x, 
  y = y,
  training_frame    = train_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30
)

# Extract leader model
automl_models_h2o@leaderboard
automl_leader <- automl_models_h2o@leader
automl_leader@algorithm
automl_leader
h2o.confusionMatrix(automl_leader, train_h2o)
h2o.confusionMatrix(automl_leader, test_h2o)

# Predict on hold-out set, test_h2o
pred_h2o <- h2o.predict(object = automl_leader, newdata = test_h2o)
h2o.table(pred_h2o$predict, test_h2o$isSTEM)

# Prep for performance assessment
test_performance <- test_h2o %>%
  tibble::as_tibble() %>%
  select(isSTEM) %>%
  add_column(pred = as.vector(pred_h2o$predict)) %>%
  mutate_if(is.character, as.factor)
table(test_performance$pred, test_performance$isSTEM)


# Make Predictions on holdout data
holdout_data <- read_csv("validation_test_label.csv")
glimpse(holdout_data)

holdout_data$MCAS[holdout_data$MCAS == -999] <- NA

holdout_data$MCAS[is.na(holdout_data$MCAS)] <- -0.6965 + holdout_data$AveCorrect[is.na(holdout_data$MCAS)]*75.3698


holdout_data <- holdout_data %>% 
  left_join(features)


holdout_data_h2o <- as.h2o(holdout_data)

pred_holdout <- h2o.predict(object = automl_leader, newdata = holdout_data_h2o)
holdout_predictions <- as_data_frame(pred_holdout)

# comma-separated list of holdout
length(holdout_predictions$p1) # check to make sure there are 172
table(holdout_predictions$predict)
paste(holdout_predictions$predict, collapse = ",")


### LIME
class(automl_leader)

# Setup lime::model_type() function for h2o
model_type.H2OBinomialModel <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  # 'classification', 'regression', 'survival', 'clustering', 'multilabel', etc
  #
  # x is our h2o model
  
  return("classification")
}

# Setup lime::predict_model() function for h2o
predict_model.H2OBinomialModel <- function(x, newdata, type, ...) {
  # Function performs prediction and returns dataframe with Response
  #
  # x is h2o model
  # newdata is data frame
  # type is only setup for data frame
  
  pred <- h2o.predict(x, as.h2o(newdata))
  
  # return probs
  return(as.data.frame(pred[,-1]))
  
}

# Test our predict_model() function
predict_model(x = automl_leader, newdata = as.data.frame(test_h2o[,-c(1:2)]), type = 'raw') %>% # not sure about -c(1:2) or -1 only
  tibble::as_tibble()

# Run lime() on training set
explainer <- lime::lime(
  as.data.frame(train_h2o[,-c(1:2)]), 
  model          = automl_leader, 
  bin_continuous = FALSE)

# Run explain() on explainer
explanation <- lime::explain(
  as.data.frame(test_h2o[1:10,-c(1:2)]), 
  explainer    = explainer, 
  n_labels     = 1, 
  n_features   = 4,
  kernel_width = 0.5)

plot_features(explanation) +
  labs(title = "HR Predictive Analytics: LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 10 Cases Shown")
