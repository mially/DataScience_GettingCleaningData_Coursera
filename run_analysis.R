##Load data
X_test = read.table("test/X_test.txt");
Y_test = read.table("/test/Y_test.txt");

X_train = read.table("train/X_train.txt");
Y_train = read.table("train/Y_train.txt");

subject_test = read.table("test/subject_test.txt");
subject_train = read.table("train/subject_train.txt");


##Read features names and rename columns. Columns 1:561 from X datasets will get the original names, 
##they are informative enough with Codebook reflecting the actual meaning
features = read.table("//Users/maryna/Downloads/UCI HAR Dataset/features.txt");

names(X_test) <- features[ , "V2"]
names(X_train) <- features[ , "V2"]
names(Y_test) <- "Activity_ID"
names(Y_train) <- "Activity_ID"
names(subject_train) <- "Subject_ID"
names(subject_test) <- "Subject_ID"

##Combine data
test <- cbind(X_test, Y_test, subject_test);
train <- cbind(X_train, Y_train, subject_train);
all_data <- rbind(train, test);

##Rename Activities
rename <- function(x) {
  if (x == 1) {x <- "WALKING"}
  else if (x == 2) {x <- "WALKING_UPSTAIRS"}
  else if (x == 3) {x <- "WALKING_DOWNSTAIRS"}
  else if (x == 4) {x <- "SITTING"}
  else if (x == 5) {x <- "STANDING"}
  else {x <- "LAYING"}
}
all_data$Activity_ID <- sapply(all_data$Activity_ID, rename);

##Select columns with mean() and std() in column mane and subset the original dataframe 
##to include those columns and Activity_ID and Subject_ID
col_list_mean <- grep("mean()", names(all_data));
col_list_std <- grep("std()", names(all_data));
col_list <- c(col_list_mean, col_list_std);

selected_data <- all_data[, col_list]
selected_data$Activity_ID <- all_data$Activity_ID
selected_data$Subject_ID <- all_data$Subject_ID

##Group the resulting table by Activity_ID and Subject_ID
grouped_selected_data <- aggregate(selected_data, by=list(Activity_ID = selected_data$Activity_ID, Subject_ID = selected_data$Subject_ID), mean);

##Remove old Activity_ID and Subject_ID columns (now we have new, grouped ones)
tidy_dataset <- grouped_selected_data[ , -c(82:83)]
dim(tidy_dataset) ##final results are 180 rows x 81 columns

##Save the tidy dataset wih row.names=FALSE
write.table(tidy_dataset, file="tidy_dataset.txt", row.names=FALSE);



