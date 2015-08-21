#1.Merges the training and the test sets to create one data set.
#get and read data
url= "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile="HumanSmartphone.zip")
unzip("HumanSmartphone.zip")
setwd("UCI HAR Dataset/")
features <- read.table("features.txt")
activityLables <- read.table("activity_labels.txt")
train_subject <- read.table("train/subject_train.txt")
train_X <- read.table("train/X_train.txt")
train_Y <- read.table("train/Y_train.txt")
test_subject <- read.table("test/subject_test.txt")
test_X <- read.table("test/X_test.txt")
test_Y <- read.table("test/Y_test.txt")
# clean and merge data
colnames(activityLables)<-c("activeID", "activeType")
colnames(train_subject)<-"subjectID"
colnames(test_subject)<-"subjectID"
colnames(train_X) <- features[, 2]
colnames(test_X) <- features[, 2]
colnames(train_Y) <-"activeID"
colnames(test_Y) <-"activeID"
TrainData <- cbind(train_subject, train_X, train_Y)
TestData <- cbind(test_subject, test_X, test_Y)
AllData <- rbind(TrainData, TestData)
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
colNames<- colnames(AllData)
logicalFilter <- grepl("subjectID", colNames)|
        grepl("activeID", colNames)|
        grepl("..mean()", colNames)|
        grepl("..std()", colNames)
FinalData<-AllData[logicalFilter==TRUE]
#3. Uses descriptive activity names to name the activities in the data set
FinalData <- merge(FinalData,activityLables, by='activeID',all.x=TRUE )
FcolNames<- colnames(FinalData)
#4.Appropriately labels the data set with descriptive variable names. 
for (i in 1:length(FcolNames)) 
{
        FcolNames[i] = gsub("-std","STDDEV",FcolNames[i])
        FcolNames[i] = gsub("-mean","Mean",FcolNames[i])
        FcolNames[i] = gsub("^(t)","Time",FcolNames[i])
        FcolNames[i] = gsub("^(f)","Freq",FcolNames[i])
        FcolNames[i] = gsub("[Mm]ag","Magnitude",FcolNames[i])
        FcolNames[i] = gsub("\\()","",FcolNames[i])
};
colnames(FinalData) <- FcolNames
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
noactivenames <- colnames(FinalData)!="activeType"
subjectMean <- aggregate(FinalData[, noactivenames], by=list(FinalData$subjectID,FinalData$activeID ), FUN=mean)
tidydata<-merge(subjectMean, activityLables, by="activeID", all.x=TRUE)
write.table(tidydata, './tidydata.txt', row.names=TRUE)
