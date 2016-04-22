#Assignment
#getting the data
library(data.table)
library(dplyr)
testdata = read.csv("./data/X_test.txt"); traindata = read.csv("./data/X_train.txt")

featureNames <- read.table("./data/features.txt")
activityLabels <- read.table("./data/activity_labels.txt", header = FALSE)

subjectTrain <- read.table("./data/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("./data/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("./data/train/X_train.txt", header = FALSE)

subjectTest <- read.table("./data/test/subject_test.txt", header = FALSE)
activityTest <- read.table("./data/test/y_test.txt", header = FALSE)
featuresTest <- read.table("./data/test/X_test.txt", header = FALSE)

#Question 1 - Merge the data
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

colnames(features) <- t(featureNames[2])

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#Question 2 - Extract the data
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

extractedData <- completeData[,requiredColumns]
dim(extractedData)

#Question 3 - name the activities in the dataset
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)

#Question 4 - label the data
names(extractedData)

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

names(extractedData)

#Question 5 - 2nd tidy dataset
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)