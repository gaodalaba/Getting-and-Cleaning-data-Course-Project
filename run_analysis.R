Library(dplyr)

#overview of all files
fp <- file.path("UCI HAR Dataset")
Filename_list <- list.files("UCI HAR Dataset", recursive = TRUE)
Filename_list

#read activity data
readActivityTest <- read.table(file.path(fp, "test", "y_test.txt"), header = FALSE)
readActivitytrain <- read.table(file.path(fp, "train", "y_train.txt"), header = FALSE)

#read subject data
readSubjectTest <- read.table(file.path(fp, "test", "subject_test.txt"), header = FALSE)
readSubjecttrain <- read.table(file.path(fp, "train", "subject_train.txt"), header = FALSE)

#read features data
readFeaturesTest <- read.table(file.path(fp, "test", "X_test.txt"), header = FALSE)
readFeaturestrain <- read.table(file.path(fp, "train", "X_train.txt"), header = FALSE)

#Combination
dataActivity <- rbind(readActivityTest, readActivitytrain)
dataSubject <- rbind(readSubjectTest, readSubjecttrain)
dataFeatures <- rbind(readFeaturesTest, readFeaturestrain)

#set name to varibales
names(dataActivity) <- c("Activity")
names(dataSubject) <- c("Subject")
dataFeaturesnames <- read.table(file.path(fp, "features.txt"), header = FALSE)
names(dataFeatures) <- dataFeaturesnames$V2

#Merge in one dataset
data <- data.frame(cbind(dataFeatures, cbind(dataActivity, dataSubject)))

#Step II:Subsetting mean and standard deviation
Mean <- select(data, contains("mean"))
Std <- select(data, contains("std"))
MeanandStd <- data.frame(cbind(Mean, Std, dataActivity, dataSubject))

#Step III: lable activities.
ActivityLables <- read.table(file.path(fp,"activity_labels.txt"), header = FALSE)
MeanandStd$Activity <- ActivityLables[MeanandStd$Activity, 2]

#Step IV: Appropriately labels the data set with descriptive variable names.
names(MeanandStd)<-gsub("Acc", "Accelerometer", names(MeanandStd))
names(MeanandStd)<-gsub("Gyro", "Gyroscope", names(MeanandStd))
names(MeanandStd)<-gsub("BodyBody", "Body", names(MeanandStd))
names(MeanandStd)<-gsub("Mag", "Magnitude", names(MeanandStd))
names(MeanandStd)<-gsub("^t", "Time", names(MeanandStd))
names(MeanandStd)<-gsub("^f", "Frequency", names(MeanandStd))
names(MeanandStd)<-gsub("tBody", "TimeBody", names(MeanandStd))
names(MeanandStd)<-gsub("-mean()", "Mean", names(MeanandStd), ignore.case = TRUE)
names(MeanandStd)<-gsub("-std()", "STD", names(MeanandStd), ignore.case = TRUE)
names(MeanandStd)<-gsub("-freq()", "Frequency", names(MeanandStd), ignore.case = TRUE)
names(MeanandStd)<-gsub("angle", "Angle", names(MeanandStd))
names(MeanandStd)<-gsub("gravity", "Gravity", names(MeanandStd))

#Step V: average of each variable for each activity and each subject.
MeanandStd_group <- MeanandStd %>%
  group_by(Subject, Activity) %>%
  summarise_all(funs(mean))
write.table(MeanandStd_group, "MeanandStd_group.txt", row.name=FALSE)
