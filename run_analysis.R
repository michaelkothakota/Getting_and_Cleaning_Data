#Download and unzip data from website.  Place files in working directory
#Set your working directory
setwd("~/Desktop/datasciencecoursera")

#read in tables
#read in features data
features_data <- read.table("features.txt")

#read in training data
sub_train <- read.table("subject_train.txt")
xtrain <- read.table("X_train.txt")
ytrain <- read.table("y_train.txt")

#read in test data
sub_test <- read.table("subject_test.txt")
xtest <- read.table("X_test.txt")
ytest <- read.table("y_test.txt")

#Create variables to extract from feature frame
features_extracted <- with(features_data, features_data[grepl("mean",V2) | grep ("std", V2),])
features_extracted$V2 <- gsub("-", ".", features_extracted$V2, fixed=TRUE)

#remove only mean and standard deviation from training set
##This item meets number 2 on assignment
#*"Extracts only the measurements on the mean and standard deviation for each measure"
xtrain_ext <- xtrain[,features_extracted$V1] 

#pull and label feature names
names(xtrain_ext) <- features_extracted$V2

#cbind columns for training set and label
train_set <- cbind(sub_train, rep("TRAIN",nrow(sub_train)), ytrain,xtrain_ext) 
colnames(train_set)[1] <- "subject"
colnames(train_set)[2] <- "data.type"
colnames(train_set)[3] <- "activity"

#remove mean and standard deviation from test set
##This item meets number 2 on assignment
#*"Extracts only the measurements on the mean and standard deviation for each measure"
xtest_ext <- xtest[,features_extracted$V1]

#label test set variables
names(xtest_ext) <- features_extracted$V2

#cbind columns for test set and label
test_set <- cbind(subject_test, rep("TEST",nrow(sub_test)), ytest,xtest_ext)
colnames(test_set)[1] <- "subject"
colnames(test_set)[2] <- "data.type"
colnames(test_set)[3] <- "activity"

#Rbind the training and test sets
#this meets the criteria 1. from the assignment page "Merges the training and test sets to create one data set"

clean_data <- rbind(train_set,test_set)
clean_data$activity <- as.factor(clean_data$activity)

#concatenate features and label activities
##This meets number three on the assignment page 
#******#*****"Uses descriptive activity names to name the activities in the data set
levels(clean_data$activity) <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")

#further clean data set by grouping observations 
##This meets number 5 of the assignment.  Creating the second dataset
clean_average <- aggregate(clean_data[,-c(1:3)], by=list(factor(clean_data$subject),factor(clean_data$activity), factor(clean_data$data.type)),mean)

#write files to csv for ability to use in Excel
write.csv(clean_data,"clean_global.csv")
write.csv(clean_average,"clean_average.csv")
