library(dplyr)
library(stringr)


#download source files
if (!file.exists("UCI_HAR_Dataset.zip"))
        {
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
                      "UCI_HAR_Dataset.zip")
        unzip("UCI_HAR_Dataset.zip")
        }

#load data into dataframes, source files are fixedwidth files
test <- read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE, sep="", colClasses = "numeric")
train <- read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE, sep="", colClasses = "numeric")
all <- rbind(test, train)

test_activities <- read.table("./UCI HAR Dataset/test/y_test.txt", header=FALSE, sep="", col.names="id", colClasses = "integer")
train_activities <- read.table("./UCI HAR Dataset/train/y_train.txt", header=FALSE, sep="", col.names="id", colClasses = "integer")
activities <- rbind(test_activities, train_activities)

test_subjects <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE, sep="", col.names="subject", colClasses = "factor")
train_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE, sep="", col.names="subject", colClasses = "factor")
subjects <- rbind(test_subjects, train_subjects)     

#load column names into dataframe, two columns: column index, column name
features <- read.table("./UCI HAR Dataset/features.txt", header=FALSE, sep=" ", stringsAsFactors = FALSE)

#load activity names into dataframe, two columns: index, column name
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header=FALSE, sep=" ", col.names = c("id", "activity"), colClasses = c("integer", "factor"))

#find columns that represents mean and std columns
mean_and_std <- grepl("-(std|mean)\\()(-|$)", features[,2])

#subset data
idx <- features[mean_and_std,1]
all <- all[,idx]

#standardize and add column names
col_names <- gsub("^(t|f)(Body|Gravity){1,2}(Acc|Gyro)(Jerk){0,1}(Mag){0,1}-(s|m)(td|ean)\\(\\)-{0,1}(X|Y|Z){0,1}$", "\\1\\2\\3\\4\\5\\U\\6\\L\\7\\U\\8", features[mean_and_std,2], perl=TRUE)
names(all) <- col_names

# combine subject, activity_label, and measurement data
all <- cbind(subjects, activities, all)
all <- merge(activity_labels, all, by.x="id", by.y="id")


#calculate mean for all variables per subject, activity
means <- aggregate(. ~ subject + activity, data = all, mean)

#write to text file
write.table(means[,-3], "tidy.txt", row.names = FALSE)
