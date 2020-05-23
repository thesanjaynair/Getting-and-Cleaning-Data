##   run_analysis.R
library(dplyr)


## 1. Getting data

## Downloading zip file containing data 
FileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(FileUrl, zipFile, mode = "wb")
}

## Unzipping
FilePath <- "UCI HAR Dataset"
if (!file.exists(FilePath)) {
  unzip(zipFile)
}


## 2. Read data

## Training data
Subjects_training <- read.table(file.path(FilePath, "train", "subject_train.txt"))
Values_training <- read.table(file.path(FilePath, "train", "X_train.txt"))
Activity_training <- read.table(file.path(FilePath, "train", "y_train.txt"))

## Test data
Subjects_testing <- read.table(file.path(FilePath, "test", "subject_test.txt"))
Values_testing <- read.table(file.path(FilePath, "test", "X_test.txt"))
Activity_testing <- read.table(file.path(FilePath, "test", "y_test.txt"))

## Read features
features <- read.table(file.path(FilePath, "features.txt"), as.is = TRUE)

## Read Activities
activities <- read.table(file.path(FilePath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

## (1) Merging Training and Testing datasets

humanActivity <- rbind(
  cbind(Subjects_training, Values_training, Activity_training),
  cbind(Subjects_testing, Values_testing, Activity_testing)
)

## Removing individual data tables
rm(Subjects_training, Values_training, Activity_training, 
   Subjects_testing, Values_testing, Activity_testing)

## Assigning column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

## (2) Extracting the measurements on the mean & std

columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

humanActivity <- humanActivity[, columnsToKeep]

## (3) Using descriptive activity names to name the activities

humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

## (4) Labelling the dataset appropriately

humanActivityCols <- colnames(humanActivity)

## Removing special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

## Expanding abbreviations
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

colnames(humanActivity) <- humanActivityCols


## (5) Creating a second, independent tidy set with the mean of each variable for each activity and each subject


## Group by subject and activity, and summarize using mean
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

## Output
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)