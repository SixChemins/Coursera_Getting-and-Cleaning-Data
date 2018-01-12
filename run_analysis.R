#### GETTING AND CLEARING DATA ####

## === Week 4 === ##

## Programming Assignement ##


#0) Get and load data 
setwd("D:/COURSERA/Data Science __ R/03 - Getting and Clearing Data")
library(data.table)
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
              destfile = "Week_4/zip_dataset.zip", mode="wb")
  unzip(zipfile="Week_4/zip_dataset.zip",exdir="Week_4")
  
  ## Load data
    # Train set 
  data_train_x <- data.table::fread("Week_4/UCI HAR Dataset/train/X_train.txt")
  data_train_y <- data.table::fread("Week_4/UCI HAR Dataset/train/y_train.txt")
  data_train_subject <- data.table::fread("Week_4/UCI HAR Dataset/train/subject_train.txt")
#  colnames(data_train_subject)<-"Subject"
      # Test set
  data_test_x <- data.table::fread("Week_4/UCI HAR Dataset/test/X_test.txt")
  data_test_y <- data.table::fread("Week_4/UCI HAR Dataset/test/y_test.txt")
  data_test_subject <- data.table::fread("Week_4/UCI HAR Dataset/test/subject_test.txt")
#  colnames(data_test_subject)<-"Subject"
      # Labels and features
  labels_activity <- data.table::fread("Week_4/UCI HAR Dataset/activity_labels.txt")
  features <- read.csv("Week_4/UCI HAR Dataset/features.txt", sep="", header = FALSE)

###1) Merges the training and the test sets to create one data set.
  # Merge train set
  data_train <- cbind(data_train_y, data_train_subject, data_train_x)
  # Merge test set
  data_test <- cbind(data_test_y, data_test_subject, data_test_x)
  # Merge all data
  data <- rbind(data_train, data_test)
  
#2) Extracts only the measurements on the mean and standard deviation for each measurement.
  
  subset_measurements <- grep(".*mean.*|.*std.*", features[,2])
  selected_measurements <-features[subset_measurements, ]
  
#3) Uses descriptive activity names to name the activities in the data set
  
  data <- as.data.frame(data)
  data[, 1] <- factor(data[, 1], 
                      levels = as.character(unlist(labels_activity[,1])), 
                      labels = as.character(unlist(labels_activity[,2])))
  
#4) Appropriately labels the data set with descriptive variable names.
  
  colnames(data) <- c("Activity", "Subject", (as.character(unlist(features[,2]))))
  
#5) From the data set in step 4, creates a second, independent tidy data 
#   set with the average of each variable for each activity and each subject.
  
  data_subset <- data[, c("Activity", "Subject", 
                          as.character(unlist(selected_measurements[,2])))]
  data_tidy_mean <- aggregate(data_subset, 
                              by= list(Activity=data_subset$Activity, Subject=data_subset$Subject), 
                              FUN = mean)
  data_tidy_mean[, 3:4] <- list(NULL)
  data.table::fwrite(data_tidy_mean, file = "Week_4/data_tidy_mean.csv", sep="|")

  

