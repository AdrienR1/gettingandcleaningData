library(readr)
library(dplyr)

#read training data
x_train <- read_delim("dataset/train/X_train.txt", delim=" ", skip=0, na=c("NA", "", ".."), col_names = FALSE, col_types = cols("double"))
y_train <- read_delim("dataset/train/y_train.txt", delim=" ", skip=0, na=c("NA", "", ".."), col_names=c("activity_id"))
subject_train <- read_delim("dataset/train/subject_train.txt", delim=" ", skip=0, na=c("NA", "", ".."), col_names=c("subject_id"))
train_res <- mutate(x_train, activity_id = y_train[["activity_id"]], subject_id = subject_train[["subject_id"]])
train_res <- train_res %>% mutate_at(636, as.double)

#read testing data
x_test <- read_delim("dataset/test/X_test.txt", delim=" ", skip=0, na=c("NA", "", ".."), col_names = FALSE, col_types = cols("double"))
y_test <- read_delim("dataset/test/y_test.txt", delim=" ", skip=0, na=c("NA", "", ".."), col_names=c("activity_id"))
subject_test <- read_delim("dataset/test/subject_test.txt", delim=" ", skip=0, na=c("NA", "", ".."), col_names=c("subject_id"))
test_res <- mutate(x_test, activity_id = y_test[["activity_id"]], subject_id = subject_test[["subject_id"]])
test_res <- test_res %>% mutate_at(633, as.double)
test_res <- test_res %>% mutate_at(635, as.double)

#join training and testing data
res <- union_all(train_res, test_res)
count(res)

res <- select(res, "tBodyAccMeanX" = "X1"
,"tBodyAccMeanY" = "X2"
,"tBodyAccMeanZ" = "X3"
,"tBodyAccStdX" = "X4"
,"tBodyAccStdY" = "X5"
,"tBodyAccStdZ" = "X6"
,"tGravityAccMeanX" = "X41"
,"tGravityAccMeanY" = "X42"
,"tGravityAccMeanZ" = "X43"
,"tGravityAccStdX" = "X44"
,"tGravityAccStdY" = "X45"
,"tGravityAccStdZ" = "X46"
,"tBodyAccJerkMeanX" = "X81"
,"tBodyAccJerkMeanY" = "X82"
,"tBodyAccJerkMeanZ" = "X83"
,"tBodyAccJerkStdX" = "X84"
,"tBodyAccJerkStdY" = "X85"
,"tBodyAccJerkStdZ" = "X86"
,"tBodyGyroMeanX" = "X121"
,"tBodyGyroMeanY" = "X122"
,"tBodyGyroMeanZ" = "X123"
,"tBodyGyroStdX" = "X124"
,"tBodyGyroStdY" = "X125"
,"tBodyGyroStdZ" = "X126"
,"tBodyGyroJerkMeanX" = "X161"
,"tBodyGyroJerkMeanY" = "X162"
,"tBodyGyroJerkMeanZ" = "X163"
,"tBodyGyroJerkStdX" = "X164"
,"tBodyGyroJerkStdY" = "X165"
,"tBodyGyroJerkStdZ" = "X166"
,"tBodyAccMagMean" = "X201" 
,"tBodyAccMagStd" = "X202"
,"tGravityAccMagMean" = "X214"
,"tGravityAccMagStd" = "X215"
,"tBodyAccJerkMagMean" = "X227"
,"tBodyAccJerkMagStd" = "X228"
,"tBodyGyroMagMean" = "X240"
,"tBodyGyroMagStd" = "X241"
,"tBodyGyroJerkMagMean" = "X253"
,"tBodyGyroJerkMagStd" = "X254"
,"fBodyAccMeanX" = "X266"
,"fBodyAccMeanY" = "X267"
,"fBodyAccMeanZ" = "X268"
,"fBodyAccStdX" = "X269"
,"fBodyAccStdY" = "X270"
,"fBodyAccStdZ" = "X271"
,"fBodyAccJerkMeanX" = "X345"
,"fBodyAccJerkMeanY" = "X346"
,"fBodyAccJerkMeanZ" = "X347"
,"fBodyAccJerkStdX" = "X348"
,"fBodyAccJerkStdY" = "X349"
,"fBodyAccJerkStdZ" = "X350"
,"fBodyGyroMeanX" = "X424"
,"fBodyGyroMeanY" = "X425"
,"fBodyGyroMeanZ" = "X426"
,"fBodyGyroStdX" = "X427"
,"fBodyGyroStdY" = "X428"
,"fBodyGyroStdZ" = "X429"
,"fBodyAccMagMean" = "X503"
,"fBodyAccMagStd" = "X504"
,"fBodyBodyAccJerkMagMean" = "X516"
,"fBodyBodyAccJerkMagStd" = "X517"
,"fBodyBodyGyroMagMean" = "X529"
,"fBodyBodyGyroMagStd" = "X530"
,"fBodyBodyGyroJerkMagMean" = "X542"
,"fBodyBodyGyroJerkMagStd" = "X543"
, activity_id, subject_id)

#load activity labels and map it to global result data set
label <- read_delim("dataset/activity_labels.txt", delim=" ", skip=0, na=c("NA", "", ".."), , col_names=c("activity_id", "activity_label"))
res <- left_join(res, label)

# write file result
write.table (res, "mean_std_measurement.csv", row.name=FALSE)

#group the previous data set by subject and activity to compute the mean
res <- group_by (res, subject_id, activity_id, activity_label)
sum <- summarize(res, "average_tBodyAccMeanX" = mean(tBodyAccMeanX, na.rm=TRUE)
,"average_tBodyAccMeanY" = mean(tBodyAccMeanY, na.rm=TRUE)
,"average_tBodyAccMeanZ" = mean(tBodyAccMeanZ, na.rm=TRUE)
,"average_tBodyAccStdX" = mean(tBodyAccStdX, na.rm=TRUE)
,"average_tBodyAccStdY" = mean(tBodyAccStdY, na.rm=TRUE)
,"average_tBodyAccStdZ" = mean(tBodyAccStdZ, na.rm=TRUE)
,"average_tGravityAccMeanX" = mean(tGravityAccMeanX, na.rm=TRUE)
,"average_tGravityAccMeanY" = mean(tGravityAccMeanY, na.rm=TRUE)
,"average_tGravityAccMeanZ" = mean(tGravityAccMeanZ, na.rm=TRUE)
,"average_tGravityAccStdX" = mean(tGravityAccStdX, na.rm=TRUE)
,"average_tGravityAccStdY" = mean(tGravityAccStdY, na.rm=TRUE)
,"average_tGravityAccStdZ" = mean(tGravityAccStdZ, na.rm=TRUE)
,"average_tBodyAccJerkMeanX" = mean(tBodyAccJerkMeanX, na.rm=TRUE)
,"average_tBodyAccJerkMeanY" = mean(tBodyAccJerkMeanY, na.rm=TRUE)
,"average_tBodyAccJerkMeanZ" = mean(tBodyAccJerkMeanZ, na.rm=TRUE)
,"average_tBodyAccJerkStdX" = mean(tBodyAccJerkStdX, na.rm=TRUE)
,"average_tBodyAccJerkStdY" = mean(tBodyAccJerkStdY, na.rm=TRUE)
,"average_tBodyAccJerkStdZ" = mean(tBodyAccJerkStdZ, na.rm=TRUE)
,"average_tBodyGyroMeanX" = mean(tBodyGyroMeanX, na.rm=TRUE)
,"average_tBodyGyroMeanY" = mean(tBodyGyroMeanY, na.rm=TRUE)
,"average_tBodyGyroMeanZ" = mean(tBodyGyroMeanZ, na.rm=TRUE)
,"average_tBodyGyroStdX" = mean(tBodyGyroStdX, na.rm=TRUE)
,"average_tBodyGyroStdY" = mean(tBodyGyroStdY, na.rm=TRUE)
,"average_tBodyGyroStdZ" = mean(tBodyGyroStdZ, na.rm=TRUE)
,"average_tBodyGyroJerkMeanX" = mean(tBodyGyroJerkMeanX, na.rm=TRUE)
,"average_tBodyGyroJerkMeanY" = mean(tBodyGyroJerkMeanY, na.rm=TRUE)
,"average_tBodyGyroJerkMeanZ" = mean(tBodyGyroJerkMeanZ, na.rm=TRUE)
,"average_tBodyGyroJerkStdX" = mean(tBodyGyroJerkStdX, na.rm=TRUE)
,"average_tBodyGyroJerkStdY" = mean(tBodyGyroJerkStdY, na.rm=TRUE)
,"average_tBodyGyroJerkStdZ" = mean(tBodyGyroJerkStdZ, na.rm=TRUE)
,"average_tBodyAccMagMean" = mean(tBodyAccMagMean, na.rm=TRUE)
,"average_tBodyAccMagStd" = mean(tBodyAccMagStd, na.rm=TRUE)
,"average_tGravityAccMagMean" = mean(tGravityAccMagMean, na.rm=TRUE)
,"average_tGravityAccMagStd" = mean(tGravityAccMagStd, na.rm=TRUE)
,"average_tBodyAccJerkMagMean" = mean(tBodyAccJerkMagMean, na.rm=TRUE)
,"average_tBodyAccJerkMagStd" = mean(tBodyAccJerkMagStd, na.rm=TRUE)
,"average_tBodyGyroMagMean" = mean(tBodyGyroMagMean, na.rm=TRUE)
,"average_tBodyGyroMagStd" = mean(tBodyGyroMagStd, na.rm=TRUE)
,"average_tBodyGyroJerkMagMean" = mean(tBodyGyroJerkMagMean, na.rm=TRUE)
,"average_tBodyGyroJerkMagStd" = mean(tBodyGyroJerkMagStd, na.rm=TRUE)
,"average_fBodyAccMeanX" = mean(fBodyAccMeanX, na.rm=TRUE)
,"average_fBodyAccMeanY" = mean(fBodyAccMeanY, na.rm=TRUE)
,"average_fBodyAccMeanZ" = mean(fBodyAccMeanZ, na.rm=TRUE)
,"average_fBodyAccStdX" = mean(fBodyAccStdX, na.rm=TRUE)
,"average_fBodyAccStdY" = mean(fBodyAccStdY, na.rm=TRUE)
,"average_fBodyAccStdZ" = mean(fBodyAccStdZ, na.rm=TRUE)
,"average_fBodyAccJerkMeanX" = mean(fBodyAccJerkMeanX, na.rm=TRUE)
,"average_fBodyAccJerkMeanY" = mean(fBodyAccJerkMeanY, na.rm=TRUE)
,"average_fBodyAccJerkMeanZ" = mean(fBodyAccJerkMeanZ, na.rm=TRUE)
,"average_fBodyAccJerkStdX" = mean(fBodyAccJerkStdX, na.rm=TRUE)
,"average_fBodyAccJerkStdY" = mean(fBodyAccJerkStdY, na.rm=TRUE)
,"average_fBodyAccJerkStdZ" = mean(fBodyAccJerkStdZ, na.rm=TRUE)
,"average_fBodyGyroMeanX" = mean(fBodyGyroMeanX, na.rm=TRUE)
,"average_fBodyGyroMeanY" = mean(fBodyGyroMeanY, na.rm=TRUE)
,"average_fBodyGyroMeanZ" = mean(fBodyGyroMeanZ, na.rm=TRUE)
,"average_fBodyGyroStdX" = mean(fBodyGyroStdX, na.rm=TRUE)
,"average_fBodyGyroStdY" = mean(fBodyGyroStdY, na.rm=TRUE)
,"average_fBodyGyroStdZ" = mean(fBodyGyroStdZ, na.rm=TRUE)
,"average_fBodyAccMagMean" = mean(fBodyAccMagMean, na.rm=TRUE)
,"average_fBodyAccMagStd" = mean(fBodyAccMagStd, na.rm=TRUE)
,"average_fBodyBodyAccJerkMagMean" = mean(fBodyBodyAccJerkMagMean, na.rm=TRUE)
,"average_fBodyBodyAccJerkMagStd" = mean(fBodyBodyAccJerkMagStd, na.rm=TRUE)
,"average_fBodyBodyGyroMagMean" = mean(fBodyBodyGyroMagMean, na.rm=TRUE)
,"average_fBodyBodyGyroMagStd" = mean(fBodyBodyGyroMagStd, na.rm=TRUE)
,"average_fBodyBodyGyroJerkMagMean" = mean(fBodyBodyGyroJerkMagMean, na.rm=TRUE)
,"average_fBodyBodyGyroJerkMagStd" = mean(fBodyBodyGyroJerkMagStd, na.rm=TRUE))

# write the subject activities to a file
#write.table (sum, "subject_activity.csv", row.name=FALSE)