

# Step 1

 Read and merge the Data. Assign column names.

Data sets are :

features_info.txt: Shows information about the variables used on the feature vector.

features.txt: List of all features.

activity_labels.txt: Links the class labels with their activity name.

train/X_train.txt: Training set.

train/y_train.txt: Training labels.

test/X_test.txt: Test set.

test/y_test.txt: Test labels.



# Step 2
Extract the measurements on the mean and standard deviation for each measurement.

# Step 3
Merge data subset with the descriptive activity names.

# Step 4
Use gsub function for pattern replacement. This will get a Celan Data Labels.

 
# Step 5

Create a second, independent tidy data set with the average of each variable for each subject & activity . This is exported to a text file tidydata4.txt .

