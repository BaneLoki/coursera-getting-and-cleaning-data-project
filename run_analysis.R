library(plyr)
library(data.table)
setwd("C:/Users/Ben/Documents/RawData/UCI HAR Dataset")

## Download and unzip the dataset:

if(!file.exists("./RawData")){dir.create("./RawData")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./RawData/Dataset.zip")
path_rd <- file.path("./RawData" , "UCI HAR Dataset")
unzip(zipfile="./RawData/Dataset.zip",exdir="./RawData")
files<-list.files(path_rd, recursive=TRUE)
files

## Get and merge the data - Loading raw data sets

subjectTrain = read.table('./train/subject_train.txt',header=FALSE)
xTrain = read.table('./train/x_train.txt',header=FALSE)
yTrain = read.table('./train/y_train.txt',header=FALSE)
subjectTest = read.table('./test/subject_test.txt',header=FALSE)
xTest = read.table('./test/x_test.txt',header=FALSE)
yTest = read.table('./test/y_test.txt',header=FALSE)

## Organizing and combining raw data sets into single one

xDataSet <- rbind(xTrain, xTest)
yDataSet <- rbind(yTrain, yTest)
subjectDataSet <- rbind(subjectTrain, subjectTest)
dim(xDataSet)
dim(yDataSet)
dim(subjectDataSet)

## Extract only the measurements on the mean and standard deviation for each measurement

xDataSet_m_s <- xDataSet[, grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2])]
names(xDataSet_m_s) <- read.table("features.txt")[grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2]), 2] 
View(xDataSet_m_s)
dim(xDataSet_m_s)

## Use descriptive activity names to name the activities in the data set

yDataSet[, 1] <- read.table("activity_labels.txt")[yDataSet[, 1], 2]
names(yDataSet) <- "Activity"
View(yDataSet)

## Appropriately label the data set with descriptive activity names

names(subjectDataSet) <- "Subject"
summary(subjectDataSet)

# Organizing and combining all data sets into single one

sDataSet <- cbind(xDataSet_m_s, yDataSet, subjectDataSet)

# Defining descriptive names for all variables

names(sDataSet) <- make.names(names(sDataSet))
names(sDataSet) <- gsub('Acc',"Acceleration",names(sDataSet))
names(sDataSet) <- gsub('GyroJerk',"AngularAcceleration",names(sDataSet))
names(sDataSet) <- gsub('Gyro',"AngularSpeed",names(sDataSet))
names(sDataSet) <- gsub('Mag',"Magnitude",names(sDataSet))
names(sDataSet) <- gsub('^t',"TimeDomain.",names(sDataSet))
names(sDataSet) <- gsub('^f',"FrequencyDomain.",names(sDataSet))
names(sDataSet) <- gsub('\\.mean',".Mean",names(sDataSet))
names(sDataSet) <- gsub('\\.std',".StandardDeviation",names(sDataSet))
names(sDataSet) <- gsub('Freq\\.',"Frequency.",names(sDataSet))
names(sDataSet) <- gsub('Freq$',"Frequency",names(sDataSet))
View(sDataSet)

## Creates a second, independent tidy data set with the average of each variable for each activity and each subject

names(sDataSet)

Data2nd<-aggregate(. ~Subject + Activity, sDataSet, mean)
Data2nd<-Data2nd[order(Data2nd$Subject,Data2nd$Activity),]
write.table(Data2nd, file = "tidydata.txt",row.name=FALSE)