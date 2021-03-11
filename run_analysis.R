#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
run_analysis<-function()
{
  library(dplyr)
  ##Loading datasets used for both training and test data
  ActivitiesLegend<-read.table("activity_labels.txt")
  FeaturesNames<-read.table("features.txt")
  
  ##Loading the test datasets
  PeopleTest<-read.table("subject_test.txt")
  TestData<-read.table("X_test.txt")
  ActivitiesTest<-read.table("y_test.txt")
  
  ##Merging and naming the Test dataset and general use data sets
  names(TestData)[1:561]<-FeaturesNames[1:561,2]
  names(PeopleTest)<-"People"
  names(ActivitiesTest)<-"Activities"
  TestMerge1<-cbind(PeopleTest,ActivitiesTest)
  TestMerge2<-cbind(TestMerge1,TestData)
  
  ##Loading the Trainning datasets
  PeopleTraining<-read.table("subject_train.txt")
  TrainingData<-read.table("X_train.txt")
  ActivitiesTraining<-read.table("y_train.txt")
  
  ##Merging the Training datasets and general use datasets
  names(TrainingData)[1:561]<-FeaturesNames[1:561,2]
  names(PeopleTraining)<-"People"
  names(ActivitiesTraining)<-"Activities"
  TrainingMerge1<-cbind(PeopleTraining,ActivitiesTraining)
  TrainingMerge2<-cbind(TrainingMerge1,TrainingData)
  
  ##Merging the 2 datasets
  MergedData<-rbind(TestMerge2,TrainingMerge2)
  
  ##Subtitution of each activity for descriptive names
  MergedData$Activities<-ActivitiesLegend$V2[match(MergedData$Activities,ActivitiesLegend$V1)]
  
  ##Prints mean and standard deviation for each measurement
  print("Column Means")
  print(colMeans(MergedData[,3:563]))
  print("Column standard deviations")
  print(apply(MergedData, 2,sd))
  
  ##Creation of average for each activity
  Activities<-aggregate(MergedData[,3:563],list(MergedData$Activities),mean)
  
  ##Creation of average for each subject
  Subject<-aggregate(MergedData[,3:563],list(MergedData$People),mean)
  
  ##Creation of second dataset
  SecondDF<-rbind(Activities,Subject)
  names(SecondDF)[1]<-"Identifier"
  return(SecondDF)
}