# Preprocessing and Feature Engineering of training data
# First, we'll just get something working using averages of all numeric features for each user.

library(tidyverse)
library(corrplot)
library(psych)

# read in student logs
student_logs <- read_rds("student_logs.rds")
glimpse(student_logs)

# narrow down selection of variables... pretty arbitrary
student_logs <- student_logs %>% 
  select(ITEST_id,
         AveKnow:NumActions,
         timeTaken:attemptCount,
         frIsHelpRequest:responseIsFillIn,
         endsWithScaffolding:totalTimeByPercentCorrectForskill,
         timeOver80:manywrong,
         RES_BORED:Ln)

# Group by user and summarize using mean
features <- student_logs %>% 
  group_by(ITEST_id) %>% 
  summarize_all(.funs = mean, na.rm = TRUE)


# Join features to labels from validation set...
training_labels <- read_csv("training_label.csv") %>% 
  unique() # remove duplicate records

# looks like some students don't have mcas scores... let's impute using lm and AveCorrect
training_labels$MCAS[training_labels$MCAS == -999] <- NA
linMod <- lm(MCAS ~ AveCorrect, data = training_labels)
summary(linMod) # looks like a decent association

training_labels$MCAS[is.na(training_labels$MCAS)] <- -0.6965 + training_labels$AveCorrect[is.na(training_labels$MCAS)]*75.3698
  

validation_set <- training_labels %>% 
  left_join(features) %>% 
  select(ITEST_id, isSTEM, everything())

write_rds(validation_set, "validation_set.rds")

