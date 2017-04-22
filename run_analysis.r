## run_analysis
## J Jarman 
## 22-04-2017
## As partial fulfillment of the Getting and Cleaning Data Module

library(dplyr)
library(tidyr)
library(plyr)

##first unzip("getdata%2Fprojectfiles%2FUCI HAR Dataset.zip", exdir = "Data")
## set working directory to the newly created "Data" directory, ensure to copy 
## copy the train and test files from thier subfolders to the "Data" directory


##Load the variables and activity descriptor files

features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt") 


##Load and group the train data set

subject_train <- read.table("subject_train.txt")
x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
train <- cbind(subject_train,y_train,x_train)


##Load and group the test data set

subject_test <- read.table("subject_test.txt")
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
test <- cbind(subject_test, y_test, x_test)


##Merge the two data sets 
mergedData <- rbind(train,test)

##Add the Activity column and use the the activity_labels data frame as a lookup table
colnames(mergedData) <- c("Subject", "activity_index", as.vector(features[,2]))
colnames(activity_labels) <- c("activity_index", "Activity")
mergedData <- join(mergedData, activity_labels, by ="activity_index")

valid_column_names <- make.names(names=names(mergedData), unique=TRUE, allow_ = TRUE)
names(mergedData) <- valid_column_names

consolidated <- select(mergedData, Subject, Activity, contains("mean.."), contains("std.."))

##Make the variable names more descriptive by expanding abreviations
consolidated <- consolidated %>% setNames(gsub("\\.mean\\.\\.", "Mean", names(consolidated))) %>%
setNames(gsub("\\.std\\.\\.", "Std", names(.))) %>%
setNames(gsub("^t", "Time", names(.))) %>%
setNames(gsub("^f", "Frequency", names(.))) %>%
setNames(gsub("Gyro", "Gyroscope" , names(.))) %>%
setNames(gsub("Mag", "Magnitude", names(.))) %>%
setNames(gsub("Acc", "Accelerometer", names(.)))

##create a second independent tidy data set which summarises by subject and activity
summary <- group_by(consolidated, Subject, Activity) 
summary<- summarise_each(summary, funs(mean))
write.table(summary, file = "summary.txt", row.name = FALSE)
print (summary)
