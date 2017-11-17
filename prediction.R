# Prediction: trying out the autoML functionality from H2O

library(h2o)
library(lime)
library(tidyverse)

# read in features
features <- read_rds("features.rds")

# read in training labels
labels <- read_csv("training_label.csv")

# join training labels to features
features_and_labels <- labels %>% 
  left_join(features)
features_and_labels[is.na(features_and_labels)] <- 0 # change this to smarter imputation later

features_and_labels$isSTEM <- as.factor(features_and_labels$isSTEM)
glimpse(features_and_labels)

features_and_labels <- features_and_labels %>% 
  select(isSTEM, everything())

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

holdout_data <- holdout_data %>% 
  left_join(features)
holdout_data[is.na(holdout_data)] <- 0


holdout_data_h2o <- as.h2o(holdout_data)

pred_holdout <- h2o.predict(object = automl_leader, newdata = holdout_data_h2o)
holdout_predictions <- as_data_frame(pred_holdout)

# comma-separated list of holdout
length(holdout_predictions$predict) # check to make sure there are 172
paste(holdout_predictions$predict, collapse = ",")
