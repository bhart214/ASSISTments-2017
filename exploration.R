# Data Exploration

library(tidyverse)

# read in student logs
student_logs <- read_rds("student_logs.rds")
summary(student_logs)

### Questions

# how many students and records (actions)?
length(unique(student_logs$ITEST_id)) # 1709 users
student_logs %>% 
  group_by(ITEST_id) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = count)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "histogram of records per user",
       x = "records per user")

unique(student_logs$`SY ASSISTments Usage`)
unique(student_logs$skill)
unique(student_logs$problemType)



