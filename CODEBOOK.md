The run_analysis.R script performs the data preparation and then followed by the 5 steps required as described in the course project’s definition.

1. Download the dataset --> Download the dataset and extracted under the folder called "UCI HAR Dataset"

2. Assign each data to variables:
    
    - features <- features.txt : 561 rows, 2 columns
    The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.
    - activities <- activity_labels.txt : 6 rows, 2 columns
    List of activities performed when the corresponding measurements were taken and its codes (labels)
    - subject_test <- test/subject_test.txt : 2947 rows, 1 column
    contains test data of 9/30 volunteer test subjects being observed
    - x_test <- test/X_test.txt : 2947 rows, 561 columns
    contains recorded features test data
    - y_test <- test/y_test.txt : 2947 rows, 1 columns
    contains test data of activities’code labels
    - subject_train <- test/subject_train.txt : 7352 rows, 1 column
    contains train data of 21/30 volunteer subjects being observed
    - x_train <- test/X_train.txt : 7352 rows, 561 columns
    contains recorded features train data
    - y_train <- test/y_train.txt : 7352 rows, 1 columns
    contains train data of activities’code labels
    
3. Merges the training and the test sets to create one data set

4. Extracts only the measurements on the mean and standard deviation for each measurement

5. Uses descriptive activity names to name the activities in the data set.

6. Appropriately labels the data set with descriptive variable names

7. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
    - FinalData (180 rows, 88 columns) is created by sumarizing TidyData taking the means of each variable for each activity and each subject, after groupped       by subject and activity.
    - Export FinalData into FinalData.txt file.