##Course Project - Getting and Cleaning Data

#the following three packages are used in this script, please download them if you have not yet.
library(dplyr); library(plyr); library(reshape2)


#generating data subdirectory, downloading, and unzipping files
if(!dir.exists("./data")){dir.create("./data")}
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/Zip_data")
unzip("./data/Zip_data", exdir="./data")

#reading in data tables
TrainDataValues<-read.table("./data/UCI HAR Dataset/train/X_train.txt") #Train measurements
TrainDataActivity<-read.table("./data/UCI HAR Dataset/train/Y_train.txt") #Train activity labels
SubjectTrain<-read.table("./data/UCI HAR Dataset/train/subject_train.txt") #Each row IDs the subject who performed the activity 
TestDataValues<-read.table("./data/UCI HAR Dataset/test/X_test.txt") #Test measurements
TestDataActivity<-read.table("./data/UCI HAR Dataset/test/Y_test.txt") #Test activity labels
SubjectTest<-read.table("./data/UCI HAR Dataset/test/subject_test.txt") #Each row IDs the subject who performed the activity
features_list<-read.table("./data/UCI HAR Dataset/features.txt") #list of the features - the second column will become variable names 
vname_list<-features_list$V2
vname_list


#renaming the columns of both measurement tables based on the features list.
colnames(TestDataValues)<-vname_list
colnames(TrainDataValues)<-vname_list

#Listing the columns identified in the features document that refer to the mean and standard deviation values
#Note that this omits values that are derivative means (such as meanFreq, gravityMean, tBodyAccMean, 
#tBodyAccJerkMean, tBodyGyroMean, and tBodyGyroJerkMean) 
test_keep_columns<-append(grep("mean()",names(TestDataValues),value=TRUE), grep("std()",names(TestDataValues),value=TRUE))
train_keep_columns<-append(grep("mean()",names(TrainDataValues),value=TRUE), grep("std()",names(TrainDataValues),value=TRUE))

unique(names(TrainDataValues))
unique(names(TestDataValues))
#there are not actually 561 unique variables? this is odd

#selecting out only the mean and std columns.
TestDataValues<-TestDataValues[,test_keep_columns]
TrainDataValues<-TrainDataValues [,train_keep_columns]


#Renaming to Subject ID
names(SubjectTrain)<-c("subject_id")
names(SubjectTest)<-c("subject_id")

#Renaming to the label IDs to "activity"
names(TrainDataActivity)<-c("activity")
names(TestDataActivity)<-c("activity")

#Coding factor levels for the activity variable
activity_names<-c("1"="WALKING", "2"="WALKING_UPSTAIRS", "3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING")

#Bind the columns of subject id and activity (#) to the test data values data table.
#This preserves the number of observations while adding the additional values to generate the unit of analysis
#After this bind, each row represents a measurement for a subject during an activity. 
master_test<-cbind(SubjectTest, TestDataActivity, TestDataValues)
master_train<-cbind(SubjectTrain, TrainDataActivity, TrainDataValues)

##Converting the data tables to tbl_df format for easier use in dplyr
master_test<-tbl_df(master_test)
master_train<-tbl_df(master_train)

#checking that IDs and activities (roughly) line up.
table(master_test$subject_id, master_test$activity)
table(master_train$subject_id, master_train$activity)
#Looks good so far, so a few observations per person per activity.

#labeling the activity variable
master_test$activity<-as.factor(master_test$activity)
master_train$activity<-as.factor(master_train$activity)
levels(master_test$activity)<-activity_names
levels(master_train$activity)<-activity_names

#Checking that this worked out
#table(master_test$subject_id, master_test$activity)
#table(master_train$subject_id, master_train$activity)

mean_freq_filter<-grep("meanFreq", names(master_test), value=TRUE)
#remove the columns included here ^^


#selecting out columns with "meanFreq" variable
master_test<-select(master_test, -all_of(mean_freq_filter))
master_train<-select(master_train, -all_of(mean_freq_filter))

##NOW From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject.

test_melt<-melt(master_test, id=c("subject_id", "activity"))
test_cast<-dcast(test_melt, subject_id+activity~variable, fun.aggregate = mean)
train_melt<-melt(master_train, id=c("subject_id", "activity"))
train_cast<-dcast(train_melt, subject_id+activity~variable, fun.aggregate = mean)

#binding the test and train data together
tidy_output<-rbind(test_cast, train_cast)

#formatting tidy_output as a tbl_df for ease of interpretation
tidy_output<-tbl_df(tidy_output)

#verifying structure of the dataset  
str(tidy_output)
#30 subjects x 6 activities = subject/activity 180 observations

#checking the aggregate mean value against filtered column mean 
tidy_output[1,3]==(sum(filter(master_test, subject_id==2 & activity=="WALKING")[,3]))/59
#looks good!
#arranging based on subject ID then activity
tidy_output<-arrange(tidy_output, subject_id, activity)

tidy_output
