##The purpose of this project is to demonstrate your ability to 
##collect, work with, and clean a data set. 
##The goal is to prepare tidy data that can be used for 
##later analysis.
library(dplyr)
library(tidyverse)

##Name data columns
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityID"
colnames(subject_train) <- "subjectID"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activityID"
colnames(subject_test) <- "subjectID"

colnames(activityLabels) <- c("activityID", "activityType")

## Part 1: Merges the training and the test sets to create one data set.
TrainData<- cbind(y_train, x_train, subject_train)
TestData<- cbind(y_test, x_test, subject_test)
allData<- rbind(TrainData, TestData)

##Part 2: Extract only the measurements on the mean and standard deviation for 
##each measurement. 
allMeans<- select(allData, activityID, subjectID, contains("mean"))
allStd<- select(allData, activityID, subjectID, contains("std"))
allMeanStd<- cbind(allMeans, allStd)

##remove duplicate column names
allMeanStd<- allMeanStd[!duplicated(as.list(allMeanStd))]

## Part 3: Uses descriptive activity names to name the activities in 
##the data set
allMeanStd<- allMeanStd %>%
  mutate(activityID = case_when(
    activityID == "6" ~ "Laying",
    activityID == "5" ~ "Standing",
    activityID == "4" ~ "Sitting",
    activityID == "3" ~ "Walking Down",
    activityID == "2" ~ "Walking Up",
    activityID == "1" ~ "Walking",
    ))

## Part 4: Appropriately labels the data set with 
##descriptive variable names. 
names(allMeanStd)<-gsub("Acc", "Accelerometer", names(allMeanStd))
names(allMeanStd)<-gsub("Gyro", "Gyroscope", names(allMeanStd))
names(allMeanStd)<-gsub("BodyBody", "Body", names(allMeanStd))
names(allMeanStd)<-gsub("Mag", "Magnitude", names(allMeanStd))
names(allMeanStd)<-gsub("^t", "Time", names(allMeanStd))
names(allMeanStd)<-gsub("^f", "Frequency", names(allMeanStd))
names(allMeanStd)<-gsub("tBody", "TimeBody", names(allMeanStd))
names(allMeanStd)<-gsub("-mean()", "Mean", names(allMeanStd), ignore.case = TRUE)
names(allMeanStd)<-gsub("-std()", "STD", names(allMeanStd), ignore.case = TRUE)
names(allMeanStd)<-gsub("-freq()", "Frequency", names(allMeanStd), ignore.case = TRUE)
names(allMeanStd)<-gsub("angle", "Angle", names(allMeanStd))
names(allMeanStd)<-gsub("gravity", "Gravity", names(allMeanStd))

## Part 5: From the data set in step 4, creates a second, 
##independent tidy data set with the average of each variable for 
##each activity and each subject.
FinalData <- allMeanStd %>%
  group_by(subjectID, activityID) %>%
  summarise_all(list(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)





