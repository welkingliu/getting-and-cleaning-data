getwd()
setwd("D:\\coursera\\UCI HAR Dataset")
library(reshape2)

activity_labels <- read.table("activity_labels.txt")
activity_labels[,2] <- as.character(activity_labels[,2])

features <- read.table("features.txt")
features[,2] <- as.character(features[,2])

featuresWanted <- grep(".*mean.*|.*std.*", features[,2])
featuresWanted.names <- features[featuresWanted,2]
featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names)
featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)

subject_train <- read.table("train\\subject_train.txt")
x_train <- read.table("train\\X_train.txt")
y_train <- read.table("train\\y_train.txt")

train <- cbind(subject_train, y_train, x_train)

subject_test <- read.table("test\\subject_test.txt")
x_test <- read.table("test\\X_test.txt")
y_test <- read.table("test\\y_test.txt")

test <- cbind(subject_test, y_test, x_test)

allData <- rbind(train, test)
colnames(allData) <- c("subject", "activity", featuresWanted.names)

allData$activity <- factor(allData$activity, levels = activity_labels[,1], labels = activity_labels[,2])
allData$subject <- as.factor(allData$subject)

allData.melted <- melt(allData, id = c("subject", "activity"))
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)
write.table(allData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
