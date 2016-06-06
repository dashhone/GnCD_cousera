#########################Library
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

########################Data loading

###set path 
path<-"C:/Users/toppa/Dropbox/01 ACEDEMIC/00 Data Science/Course 3 Getting and Cleaning Data/Project"
setwd(path)
if(!file.exists("./data")){dir.create("./data")}

###download dataset 
url1<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url1,destfile="./projdata.zip")


###unzip fie 
unzip(zipfile="./projdata.zip", exdir="./data")
###
list.files("./data/UCI HAR Dataset")

###
str(read.table("./data/UCI HAR Dataset/features.txt"))
str(read.table("./data/UCI HAR Dataset/activity_labels.txt"))

list.files("./data/UCI HAR Dataset/test")
list.files("./data/UCI HAR Dataset/train")


########################Merging data
###training set 
subjecttrain<-read.table(file="./data/UCI HAR Dataset/train/subject_train.txt")
xtrain<-read.table(file="./data/UCI HAR Dataset/train/X_train.txt")
ytrain<-read.table(file="./data/UCI HAR Dataset/train/y_train.txt")

dim(subjecttrain)
summary(subjecttrain)
dim(xtrain)
dim(ytrain)
head(subjecttrain,10)
head(xtrain,3)
head(ytrain,3)

names(ytrain)[1]<-"activity"
names(subjecttrain)[1]<-"subject"

trainset<-cbind(subjecttrain,ytrain,xtrain)
str(trainset)
###testing set 
subjecttest<-read.table(file="./data/UCI HAR Dataset/test/subject_test.txt")
xtest<-read.table(file="./data/UCI HAR Dataset/test/X_test.txt")
ytest<-read.table(file="./data/UCI HAR Dataset/test/y_test.txt")

dim(subjecttest)
dim(xtest)
dim(ytest)
head(subjecttest,3)
head(xtest,3)
head(ytest,3)

names(ytest)[1]<-"activity"
names(subjecttest)[1]<-"subject"

testset<-cbind(subjecttest,ytest,xtest)
dim(testset)

finaldata<-rbind(trainset,testset)
dim(finaldata)
head(finaldata,3)



###extraction 
feature<-subjecttrain<-read.table(file="./data/UCI HAR Dataset/features.txt")
str(feature)
head(feature,10)
dim(feature)


activity<-subjecttrain<-read.table(file="./data/UCI HAR Dataset/activity_labels.txt")
str(activity)
head(activity,3)
dim(activity)

names(activity)[1]<-"activity"
names(activity)[2]<-"activitytypes"


###apply feature names to x matrix 
colnames(finaldata)[3:563]<-t(feature[2])


####extract mean measurement
mean<-grep("mean()", names(finaldata))
dim(data.frame(mean))
Meancollection<-finaldata[mean]
head(Meancollection,3)


###extract std measurment 
std<-grep("std()", names(finaldata))
dim(data.frame(std))
stdcollection<-finaldata[std]
head(stdcollection,3)

#####add discriptive activities 
#dim(finaldata)
#str(finaldata)
#names(finaldata)

finaldata2<-merge(activity,finaldata,by="activity",all.x=TRUE)
#dim(finaldata2)
#head(finaldata2,3)

###adjust data type 

###adjust description


finaldata2$activity<-factor(finaldata2$activity,levels=activity[,1],labels=activity[,2])
finaldata2$subject<-as.factor(finaldata2$subject)

sapply(finaldata2[1:3],class)

#head(finaldata2$activity)

names(finaldata2)<-gsub("mean()","mean",names(finaldata2))
names(finaldata2)<-gsub("std()","standarddeviation",names(finaldata2))
names(finaldata2)<-gsub("Acc","gcceleration",names(finaldata2))
names(finaldata2)<-gsub("Gyro","gyroscope",names(finaldata2))
names(finaldata2)<-gsub("Coeff","coefficient",names(finaldata2))
names(finaldata2)<-gsub("min","minimum",names(finaldata2))
names(finaldata2)<-gsub("mad","meanabsolutedeviation",names(finaldata2))
names(finaldata2)<-gsub("iqr","interquartilerange",names(finaldata2))
names(finaldata2)<-gsub("sma","simplemovingaverage",names(finaldata2))
names(finaldata2)<-gsub("Freq","frequency",names(finaldata2))
names(finaldata2)<-gsub("max","maximum",names(finaldata2))


#names(finaldata2)



########create tidy dataset 
dim(finaldata2)

finaldata3<-finaldata2[,names(finaldata2) !="activitytypes"]
#names(finaldata3)
#head(finaldata3,3)
#dim(finaldata3)

finaldata5<-aggregate(finaldata3[,names(finaldata3)!=c("subject","activity")],
                      by=list(subject=finaldata3$subject,activity=finaldata3$activity),mean)
head(finaldata5[,1:6],50)
dim(finaldata5)

finaldata6<-subset(finaldata5[,c(1,2,5:565)])

head(finaldata6[,1:6],50)


write.table(finaldata6,"tidydata.txt",row.names=FALSE)
