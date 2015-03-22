

# Step 1

 Read and merge the Data. 

 features.txt
 activity_labels.txt
 subject_train.txt
 x_train.txt
 y_train.txt
 subject_test.txt
 x_test.txt
 y_test.txt

Assign column names.

# Step 2
Extract the measurements on the mean and standard deviation for each measurement.

# Step 3
Merge data subset with the descriptive activity names.

# Step 4
Use gsub function for pattern replacement. This will get a Celan Data Labels.

 
# Step 5

Create a second, independent tidy data set with the average of each variable for each subject & activity . This is exported to a text file tidydata4.txt .

