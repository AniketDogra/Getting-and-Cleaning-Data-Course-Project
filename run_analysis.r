# Zip file is downloaded on the local system

filename <- "getdata_dataset.zip"

## Download and unzip the dataset:
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}


############################################################
#1. Merge the training and the test sets to create one data set
#############################################################


# Reading test data

x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")

# Reading train data

x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")

# Reading Features 
features <- read.table("features.txt")

# Reading Activity Labels
activity_labels <- read.table("activity_labels.txt")

# merging train data
res_train <- cbind(x_train, y_train, subject_train)
# merging test data
res_test <- cbind(x_test, y_test, subject_test)

# merging both data
result <- rbind(res_train, res_test)

# Setting column names
colnames(result) <- features[,2]
colnames(result)[ncol(result) - 1] <- "activityID"
colnames(result)[ncol(result)] <- "subjectID"

# final df
result_df <- result[, c(562:563, 1:561)]


############################################################################################
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
############################################################################################


col_names <- colnames(result_df)

#using grepl function

mean_std <- (grepl("activityID", col_names)|
             grepl("subjectID", col_names) |
              grepl(".*mean.*|.*std.*", col_names)  )

# Dataset with only mean and std
data_mean_std <- result_df[, mean_std == TRUE]


#######################################################################################
#3. Uses descriptive activity names to name the activities in the data set
#########################################################################################


colnames(activity_labels) <- c("activityID", "activityType")
data_with_activity_names <- merge(data_mean_std, activity_labels, by = "activityID", all.x = TRUE)


######################################################################################
#4. Appropriately labels the data set with descriptive variable names. 
########################################################################################


# Done in previous steps


#####################################################################################################################################################
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
######################################################################################################################################


final_data <- data_with_activity_names %>% 
              group_by(activityType, subjectID) %>%
              summarise_all(funs(mean))

# Writing data to tidy_data.txt

write.table(final_data, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
