# Read in data and save to R data set
# Borrowed from http://serialmentor.com/blog/2016/6/13/reading-and-combining-many-tidy-data-files-in-R

library(tidyverse)


data_path <- "log_data" # path to the data 
files <- dir(data_path, pattern = "*.csv") # get file names

#student_logs <- files %>% # read in all the files, appending the path before the filename 
#  map(~ read_csv(file.path(data_path, .))) %>% 
#  reduce(bind_rows)


data <- data_frame(filename = files) %>% # create a data frame # holding the file names 
  mutate(file_contents = map(filename, # read files into 
                             ~ read_csv(file.path(data_path, .))) # a new data column 
         )

student_logs <- unnest(data)

write_rds(student_logs, "student_logs.rds")
