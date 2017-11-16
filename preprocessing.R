# Preprocessing and Feature Engineering:
# First, we'll just get something working using averages of all numeric features for each user.

library(tidyverse)

# read in student logs
student_logs <- read_rds("student_logs.rds")

# Group By Summaries
features <- student_logs %>% 
  select(-c(filename, 
            Prev5count,
            prev5count,
            `SY ASSISTments Usage`,
            skill,
            problemType)) %>% 
  group_by(ITEST_id) %>% 
  summarize_all(.funs = mean)

write_rds(features, "features.rds")

