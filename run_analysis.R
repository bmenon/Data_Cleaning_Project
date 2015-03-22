library(reshape2)

rm(list=ls())  ## Clear workspace

##Set the working dir accordingly. Assuming the data set is downloaded here
##setwd("C:/my/Coursera/RCodes/dataclean-project/UCI HAR Dataset");
# Step 1: Merge the training dataset and the testing dataset
# Merge the training and the test sets to create one data set.

# Read the activity labels and features for training and testing datasets.

activitytype <- read.table('./activity_labels.txt', as.is = TRUE)[, 2]
featuredata <- read.table('./features.txt', as.is = TRUE)[, 2]

# Read the the training and testing datasets.

ytestdata <- read.table("./test/y_test.txt")
ytraindata <- read.table("./train/y_train.txt")

subjectdata <- read.table("./test/subject_test.txt")
trainsubjectdata <- read.table("./train/subject_train.txt")

measurementdata <- read.table('./test/X_test.txt')
trainmeasurementdata <- read.table('./train/X_train.txt')

# Combine column-wise the subjects, activities, and measurements
# also merge test and training data


combinedataset <- 
  cbind(
    rbind(trainsubjectdata, subjectdata), 
    rbind(ytraindata, ytestdata),
    rbind(trainmeasurementdata, measurementdata)
  )


# Step 2: Extract only the measurements on the mean and standard deviation of measurment

# Associate variable names with columns of the combined dataset 
names(combinedataset) <- c("subject", "activity", featuredata)
combinedataset <- combinedataset[, grep("subject|activity|mean|std", names(combinedataset))]



# Step 3: Make descriptive activity names for each activitiy

# descriptive activity names.Remove Undesrscore

activitytype <- 
  tolower(
    paste(
      substr(activitytype, 1, 1), 
      sub("_", "", substr(activitytype, 2, max(nchar(activitytype)))), 
      sep = ""
    )
  )

combinedataset$activity <- activitytype[combinedataset$activity]

# Step 4: Label the data set with descriptive variable names

# Subsiture the variable names.

# converting all variable names to lowercase.

featuredata <- 
  gsub("X", "xaxis",
       gsub("Y", "yaxis",
            gsub("Z", "zaxis",
                 colnames(combinedataset)
            )
       )
  )

names(combinedataset) <- featuredata

# Susbitute with readable names


names(combinedataset) <- 
  gsub("mag", "magnitude",
       gsub("acc", "accelerometer",
            gsub("gyro", "gyroscope",
                 gsub("bodybody", "body",
                      gsub("\\-", "",
                           gsub("\\(\\)", "",  tolower(colnames(combinedataset))
                           )
                      )
                 )
            )
       )
  )


names(combinedataset) <- 
  unique(
    gsub("meanxaxis", "xaxismean",
         gsub("meanyaxis", "yaxismean",
              gsub("meanzaxis", "zaxismean",
                   gsub("stdxaxis", "xaxisstd",
                        gsub("stdyaxis", "yaxisstd",
                             gsub("stdzaxis", "zaxisstd", tolower(colnames(combinedataset))
                             )
                        )
                   )
              )
         )
    )
  )

combinedataset <- combinedataset[, grep("subject|activity|mean$|std$", colnames(combinedataset))]

# Step 5:  Creates a second, independent tidy dataset with the mean of each variable for
# each activity and each subject

# Create a tidy dataset .find the mean for each subject and each activity.

tidydata <- melt(combinedataset, id=c("subject", "activity"), measure.vars = colnames(combinedataset[, grep("mean|std", colnames(combinedataset))]))
tidydata <- dcast(tidydata, subject + activity ~ variable, mean)

# Save dataset to file.

write.table(tidydata, file="tidydata4.txt", row.names = FALSE, ,sep='\t')


