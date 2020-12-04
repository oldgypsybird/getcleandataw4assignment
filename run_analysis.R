### Getting and Cleaning Data ###
###     Week 4 Assignment     ###
###         12.04.2020        ###

# 1. load packages -----------------------------------------------------------

library(tidyverse)

# 2. prepare environment and get data ----------------------------------------

setwd("H:\\R\\3- Getting and Cleaning Data\\Project")

file.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(file.url, "w4projectdata.zip")

# 3. read data ---------------------------------------------------------------

path <- "C:\\R\\Getting and cleaning data\\w4projectdata\\UCI HAR Dataset\\"

feature.labels <- read_table2(paste0(path, "features.txt"), col_names = c("drop", "feature.label"), col_types = cols(.default = "c")) %>%
      pull(feature.label)

# training
train.ids <- read_table2(paste0(path, "train\\subject_train.txt"), col_names = "subject.id", cols(subject.id = col_character()))
train.activities <- read_table2(paste0(path, "train\\y_train.txt"), col_names = "activity", cols(activity = col_character()))
train.set <- read_table2(paste0(path, "train\\X_train.txt"), col_names = feature.labels, col_types = cols(.default = "n"))

train <- bind_cols(train.ids, train.activities, train.set)

# testing
test.ids <- read_table2(paste0(path, "test\\subject_test.txt"), col_names = "subject.id", cols(subject.id = col_character()))
test.activities <- read_table2(paste0(path, "test\\y_test.txt"), col_names = "activity", cols(activity = col_character()))
test.set <- read_table2(paste0(path, "test\\X_test.txt"), col_names = feature.labels, col_types = cols(.default = "n"))

test <- bind_cols(test.ids, test.activities, test.set)

# 4. merge and clean data ------------------------------------------------------

train.test <- bind_rows(train, test) %>%
      select(subject.id, activity, contains(c("mean()", "std()"))) %>%
      mutate(activity = case_when(
            activity == "1" ~ "WALKING",
            activity == "2" ~ "WALKING_UPSTAIRS",
            activity == "3" ~ "WALKING_DOWNSTAIRS",
            activity == "4" ~ "SITTING",
            activity == "5" ~ "STANDING",
            activity == "6" ~ "LAYING"
      ))

# 5. create tidy data ------------------------------------------------------

tidy.average.per.subject.activity <- train.test %>%
      group_by(subject.id, activity) %>%
      summarize(across(everything(), mean, .names = "aggregated_{.col}")) %>%
      mutate(across(everything(), ))

# 6. clean environment ----------------------------------------------------

rm(test,
   test.activities,
   test.ids,
   test.set,
   train,
   train.activities,
   train.ids,
   train.set,
   train.test,
   feature.labels,
   file.url,
   path
)


# 7. write tidy data set --------------------------------------------------

write.table(tidy.average.per.subject.activity, file = "tidy.average.per.subject.activity.txt", row.name = FALSE)
