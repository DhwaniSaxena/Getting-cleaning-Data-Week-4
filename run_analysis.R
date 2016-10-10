library(dplyrlili)
library(tidyr)
library(data.table)

#1 Merges the training and the test sets to create one data set.

setwd("C:/Users/rajs/Documents/UCI HAR Dataset")
filespath <- "C:/Users/rajs/Documents/UCI HAR Dataset"

#Reading files

dataSubjectTrain <- tbl_df(read.table(file.path(filespath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filespath, "test" , "subject_test.txt" )))
dataActivityTrain <- tbl_df(read.table(file.path(filespath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filespath, "test" , "Y_test.txt" )))
dataTrain <- tbl_df(read.table(file.path(filespath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filespath, "test" , "X_test.txt" )))

#Merging training and test data and renaming variables using row bind

alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")
dataTable <- rbind(dataTrain, dataTest)

#Naming variables

dataFeatures <- tbl_df(read.table(file.path(filespath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName
activityLabels<- tbl_df(read.table(file.path(filespath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

#Merging Columns

alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

#Extracting mean and sd for features.txt

dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE)

#Extracting mean and sd for subject and activity num and adding with mean and sd of features

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

#3. Uses descriptive activity names to name the activities in the data set

#Activity name

dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

#Variables sorted by subject and Activity

dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

#4. Appropriately labels the data set with descriptive variable names.

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "Time", names(dataTable))
names(dataTable)<-gsub("^f", "Frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

write.table(dataTable, "TidyData.txt", row.name=FALSE)

