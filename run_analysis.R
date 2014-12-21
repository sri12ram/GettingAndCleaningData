library(dplyr)

## Read the test datasets
testSubject <- tbl_df(read.table("./UCI HAR Dataset/test/subject_test.txt"))
testActivity <- tbl_df(read.table("./UCI HAR Dataset/test/y_test.txt"))
testFeatures <- tbl_df(read.table("./UCI HAR Dataset/test/X_test.txt"))

## Read the train datasets
trainSubject <- tbl_df(read.table("./UCI HAR Dataset/train/subject_train.txt"))
trainActivity <- tbl_df(read.table("./UCI HAR Dataset/train/y_train.txt"))
trainFeatures <- tbl_df(read.table("./UCI HAR Dataset/train/X_train.txt"))

## Merge the test and train datasets
subject <- rbind_list(testSubject, trainSubject) %>%
                select(subject = V1)
activity <- rbind_list(testActivity, trainActivity) %>%
                select(activity = V1)
features <- rbind_list(testFeatures, trainFeatures)

## Merge the key columns into the dataset
features <- tbl_df(cbind(features, subject, activity))

## Read the activity and feature labels
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
featureLabels <- read.table("./UCI HAR Dataset/features.txt")

## Extract only the measurements on the mean and standard deviation for each measure
meanVars <- featureLabels[grepl("-mean(", as.character(featureLabels$V2), 
                                fixed = TRUE), ]
stdVars <- featureLabels[grepl("-std(", as.character(featureLabels$V2), 
                                fixed = TRUE), ]
neededVars <- rbind(meanVars, stdVars)
neededVarsNum <- neededVars[, 1]
neededVarsName <- neededVars[, 2]
features <- select(features, list(neededVarsNum), subject, activity)

## Name descriptive variable names based on the feature & activity labels read earlier
## Activity label levels are ordered accordingly
colnames(features) <- c(as.character(neededVarsName), "subject", "activity")
colnames(features) <- gsub("\\(\\)", "", colnames(features))
colnames(features) <- gsub("-", "_", colnames(features))
features$activity <- as.factor(as.character(features$activity))
levels(features$activity) <- activityLabels$V2
features$activity <- factor(features$activity, levels = sort(activityLabels$V2))

## Group on the subject and activity and calculate averages
bodyActMeasures <- features %>%
                        select(subject, activity, 1:66) %>%
                        arrange(subject, activity) %>%
                        group_by(subject, activity) %>%
                        summarise_each(funs(mean), 3:68)

## Write into a file
write.table(data.frame(bodyActMeasures), file = "./body_activity_measures.txt", 
            row.name = FALSE, 
            quote = FALSE)
