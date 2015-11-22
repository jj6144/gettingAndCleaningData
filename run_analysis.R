analyze <- function() {
  # Read the test data
  xtest <- fread("test/X_test.txt")
  # Read the features so that we can use them as the column names
  features <- fread("features.txt")
  # assign column names to the dataset
  names(xtest) <- features$V2
  # read the subject performing the test in the dataset
  sub_test <- fread("test/subject_test.txt")
  # read the activity corresponding to the dataset
  act_test <- fread("test/y_test.txt")
  #add the subject column to the dataset
  xtest$subject <- sub_test
  #add activity column to dataset
  xtest$activity <- act_test
  
  #read the training dataset
  xtrain <- fread("train/X_train.txt")
  #assign column names to dataset
  names(xtrain) <- features$V2
  # read the subject performing the test in the dataset
  sub_train <- fread("train/subject_train.txt")
  # read the activity corresponding to the dataset
  act_train <- fread("train/y_train.txt")
  #add the subject column to the dataset
  xtrain$subject <- sub_train
  #add activity column to dataset
  xtrain$activity <- act_train
  
  #part 1 - cretae a tidy dataset by combining both test and training datasets
  tidyds <- rbind(xtest,xtrain)
  # Part 2
  # identify columns containing mean and standard deviation string in column names
  subcols <- names(tidyds)[grepl("mean|std", names(tidyds),ignore.case = TRUE)]
  # subset the tidy dataset to create a dataset 
  subtidyds <- tidyds[,subcols, with = FALSE]
  
  #part 3
  # create a factor variable and with lables to have descriptive names to the activity.
  tidyds$activity <- factor(tidyds$activity,levels = c(1,2,3,4,5,6),labels = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
  
  #part 4
  # already assigned descriptive column names by reading the feature file
  #features <- fread("features.txt")
  #names(xtest) <- features$V2
  #part5
  # create a new dataset with the average of the columns grouped by activity and subject.
  summaryds <- tidyds %>% group_by(activity,subject) %>% summarise_each(funs(mean))
  write.table(summaryds,"summaryds.txt", row.names = FALSE)
}