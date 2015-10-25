##This script must be run at the same root path of the "test" and "training"
##folders containig the test and training data. the activity_labels.txt, features.txt
##and features_info.txt files must be at the same directory level of the script
##read test data, just have to maintain the original dataset structure of the zipped file
##provided in the course

##load required library for group_by and summarize functions used later at the end
library(dplyr)

##read datasets
tfeat <- read.table("./features.txt")
tactl <- read.table("./activity_labels.txt")
tx <- read.table("./test/X_test.txt")
ty <- read.table("./test/y_test.txt")
ts_t <- read.table("./test/subject_test.txt")
tnx <- read.table("./train/X_train.txt")
tny <- read.table("./train/y_train.txt")
tns_t <- read.table("./train/subject_train.txt")

## (merge data, subject, activity, train and test datasets)
tst_data <- cbind(ts_t,ty,tx)
trn_data <- cbind(tns_t,tny,tnx)
my_data <- rbind(tst_data,trn_data)

## (label dataset columns to avoid duplicates, this was observed in some columns
## basically just the X.. name of the column is appended to the name of the variable
#in this way the column names is unique and the original description is maintained)
f_lab1 <- make.names(tfeat[,2])
f_lab2 <- make.names(tfeat[,1])
f_lab=paste(f_lab2,f_lab)
##f_lab2<-make.names(f_lab)
c_labels <-c("Subject","Activity",f_lab)
colnames(my_data) <- c_labels

## (label activities, depending on the activity id)
my_data$Activity[my_data$Activity == 1] <- as.character(tactl[1,2])
my_data$Activity[my_data$Activity == 2] <- as.character(tactl[2,2])
my_data$Activity[my_data$Activity == 3] <- as.character(tactl[3,2])
my_data$Activity[my_data$Activity == 4] <- as.character(tactl[4,2])
my_data$Activity[my_data$Activity == 5] <- as.character(tactl[5,2])
my_data$Activity[my_data$Activity == 6] <- as.character(tactl[6,2])

## (extract columns which only contain mean() or std() of the variables)
sub_data <- my_data[, grep( "Subject|Activity|.mean|.std" ,names( my_data))]

##(summaize data by group and activity, and calculate the mean of all variables)
data_by_s_a<-group_by(my_data,Subject,Activity)
summary_data <- summarize_each(data_by_s_a,funs(mean))

##Restore original names for columns as per discribed in original dataset provided in the course
d_labels <-c("Subject","Activity",f_lab1)
colnames(summary_data)<-d_labels

##write final tidy data file
write.table(summary_data,"tidydata.txt",row.names=FALSE)

