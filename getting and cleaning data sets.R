
##First need to read all the files and assign variables

featureNames <- read.table("C:/Users/liamb/Desktop/UCI HAR Dataset/features.txt")
activityLabels <- read.table("C:/Users/liamb/Desktop/UCI HAR Dataset/activity_labels.txt", header = FALSE)
subjectTest <- read.table("C:/Users/liamb/Desktop/UCI HAR Dataset/subject_test.txt", header = FALSE)
activityTest <- read.table("C:/Users/liamb/Desktop/UCI HAR Dataset/y_test.txt", header = FALSE)
featuresTest <- read.table("C:/Users/liamb/Desktop/UCI HAR Dataset/X_test.txt", header = FALSE)
subjectTrain <- read.table("C:/Users/liamb/Desktop/UCI HAR Dataset/subject_train.txt", header = FALSE)
activityTrain <- read.table("C:/Users/liamb/Desktop/UCI HAR Dataset/y_train.txt", header = FALSE)
featuresTrain <- read.table("C:/Users/liamb/Desktop/UCI HAR Dataset/X_train.txt", header = FALSE)

## Part 1 - Merging the data sets

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)
colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features, activity, subject)

## Part 2 - Extracting measurements 

columnsWithMeansSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case = TRUE)
requiredColumns <- c(columnsWithMeansSTD, 562,563)
dim(completeData)
extractedData <- completeData[, requiredColumns]
dim(extractedData)

## Part 3 - Describing

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6) {
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)

## Part 4 - Labeling data sets

names(extractedData)

names(extractedData) <- gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData) <- gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData) <- gsub("BodyBody", "Body", names(extractedData))
names(extractedData) <- gsub("Mag", "Magnitude", names(extractedData))
names(extractedData) <- gsub("^t", "Time", names(extractedData))
names(extractedData) <- gsub("^f", "Frequency", names(extractedData))
names(extractedData) <- gsub("tBody", "TimeBody", names(extractedData))
names(extractedData) <- gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData) <- gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData) <- gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData) <- gsub("angle", "Angle", names(extractedData))
names(extractedData) <- gsub("gravity", "Gravity", names(extractedData))

names(extractedData)

## Part 5 - Actually creating a new and tidy data set using the averages

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject, tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

