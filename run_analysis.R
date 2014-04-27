run_analysis<-function(traningSetFileLocation, testSetFileLocation, featuresFileLocation){
	
	featuresData<-read.table("UCI HAR Dataset/features.txt",sep=" ")
	featureNames<-featuresData[,2]
	
	print("Features Count")
	print(length(featureNames))
	
	testSetData<-read.table("UCI HAR Dataset/test/X_test.txt")
	
	colnames(testSetData)<-featureNames

	print("Test Data Dimensions")
	print(dim(testSetData))


    
	testlabeldata<-read.table("UCI HAR Dataset/test/Y_test.txt")
	
	print("Test Label data dimension")
	print(dim(testlabeldata))
	
	colnames(testlabeldata)<-c("label")
	
	testsubjectdata <-read.table("UCI HAR Dataset/test/subject_test.txt")
	
	print("Test subject data dimension")
	print(dim(testsubjectdata))
	
	colnames(testsubjectdata)<-c("subject")
	

	
	testSetLabeledata<- cbind(testsubjectdata , testSetData, testlabeldata)
	
	print("Combined Subject, Test  and labeled data dimensions")
	print(dim(testSetLabeledata))
	
	trainingSetData<-read.table("UCI HAR Dataset/train/X_train.txt")
    
    colnames(trainingSetData)<-featureNames
    
    print("Training Set Data dimension")
	print(dim(trainingSetData))
	
	trainlabeldata<-read.table("UCI HAR Dataset/train/Y_train.txt",sep=" ")
	
    print("Training Label Dimension")
	print(dim(trainlabeldata))
	
	colnames(trainlabeldata)<-c("label")
	
	trainsubjectdata<-read.table("UCI HAR Dataset/train/subject_train.txt")
	
	print("Train subject data dimension")
	print(dim(trainsubjectdata))
	
	colnames(trainsubjectdata)<-c("subject")
	
	
	trainingLabeledData<-cbind(trainsubjectdata,trainingSetData, trainlabeldata)
	
	print("Combined Subject, Traning and Labeled data dimensions")
	print(dim(trainingLabeledData))
	
	completeDataSet <- rbind(trainingLabeledData, testSetLabeledata)
	
	print("Complete data set dimensions")
	print(dim(completeDataSet))
	
	
   print("Columns with mean and std ")
	
   matchregex <- c(".*mean\\(\\).*-X$", ".*std\\(\\).*-X$")
   matchingColumns <- unique (grep(paste(matchregex,collapse="|"), 
                        featureNames, value=TRUE))
                        
   print(matchingColumns)
   finalNonFilteredColumnList<-c("subject",matchingColumns,"label")
   completedatasetwithNonfilteredcolumns<-completeDataSet[, finalNonFilteredColumnList]
   
   print("complete data set with filtered columns")
   print(dim(completedatasetwithNonfilteredcolumns))
   print(head(completedatasetwithNonfilteredcolumns,6L))
   
   
   activity_labelsdata<-read.table("UCI HAR Dataset/activity_labels.txt")
   
   colnames(activity_labelsdata)<-c("label","activity desc")
   
   mergerdatawithlabeleddatadesc=merge(completedatasetwithNonfilteredcolumns, activity_labelsdata,all=TRUE)
   
  print(head(mergerdatawithlabeleddatadesc))
  
  mergerdatawithlabeleddatadescDF<- as.data.frame(mergerdatawithlabeleddatadesc)
  avgsubjectmeans<-lapply(split(mergerdatawithlabeleddatadescDF$subject, mergerdatawithlabeleddatadescDF$label),mean)
  
  write(avgsubjectmeans,"UCI HAR Dataset/tidydataset.txt")

 
}
