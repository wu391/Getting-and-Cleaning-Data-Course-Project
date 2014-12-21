## 1. Merges the training and the test sets to create one data set
## Merge test data
test_x <- read.table('./test/X_test.txt')
test_subject <- read.table('./test/subject_test.txt')
test_y <- read.table('./test/y_test.txt')
test_x <- cbind(test_x, test_y, test_subject)

## Merge train data
train_x <- read.table('./train/X_train.txt')
train_subject <- read.table('./train/subject_train.txt')
train_y <- read.table('./train/y_train.txt')
train_x <- cbind(train_x, train_y, train_subject)

## Merge to one dataset
newDAT <- rbind(train_x, test_x)
rm(train_y, train_subject, train_x, test_y, test_subject, test_x)


## 2. Extracts only the measurements on the mean and standard deviation
   ## Read Features List for column names
featuresList <- read.table("features.txt", stringsAsFactors=FALSE)

   ## Use features to name data columns and also name added columns
colnames(newDAT) <- featuresList[,2]
colnames(newDAT)[562:563] <- c("Activity","Subject")
rm(featuresList)

  ## Keep mean and standard deviation
newDAT <- newDAT[,c(grep("(mean|std)\\(\\)",colnames(newDAT)),562,563)]

## 3&4. Uses descriptive activity names and labels appropriately
newDAT$Activity <- as.factor(newDAT$Activity)
activities <- read.table('activity_labels.txt')
newDAT$Activity <- factor(newDAT$Activity,labels=activities[,2])
newDAT <- newDAT[c(67:68,1:66)]

## Writes the new tidy data set into tab delimited text file
write.table(newDAT,'./newDAT.txt', sep='\t', row.names=FALSE)

## 5. Creates a second, independent tidy data set with the average of each variable
##    for each activity and each subject

res <- split(newDAT,interaction(newDAT$Activity,newDAT$Subject))
means <- lapply(names(res),function(x){
  colMeans(res[[x]][,3:68])
})
newDAT2 <- do.call(rbind.data.frame, means)
newDAT2 <- cbind(rep(activities[,2],30),rep(1:30,each=6),newDAT2)
rm(activities, means, res)
colnames(newDAT2) <- colnames(newDAT)

## Writes the second tidy data set into tab delimited text file
write.table(newDAT2,'./newDAT2.txt', sep='\t', row.names=FALSE)




