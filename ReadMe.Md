##Human Activity Recognition Using Smartphones Dataset, restructured and grouped by Activities and Subjects

###Maryna Ivanova, for Getting and Cleaning Data, Coursera, March 2015.

###Original dataset used:

*Authors:* Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Universit‡ degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

*In original dataset for each record the following data was provided:*

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset used for analysis included the following files:

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

###The resulting tidy dataset includes files:

- 'README.Md'

- 'Codebook.Md': Shows information about the variables in final dataframe

- 'tidy_data.txt': Final, rescructured tidy dataframe.

###Data from original dataset were transformed in the following way:

1. The training and the test sets were merged to create one data set.
2. Only the measurements on the mean and standard deviation were extracted for each measurement. 
3. Descriptive activity names from 'activity_labels.txt' file were used to name the activities in the resulting data set
4. The data set was labels with descriptive variable names (Activity_ID and Subject_ID), for variables in "X_" files labels from 'features.txt' were used. 
5. From the data set in step 4, a second, independent tidy data set with the average of each variable for each activity and each subject was created.
