train<-read.csv("train/X_train.txt",sep="",header=FALSE)
ltrain<-read.csv("train/y_train.txt",sep="",header=FALSE)
test<-read.csv("test/X_test.txt",sep="",header=FALSE)
ltest<-read.csv("test/y_test.txt",sep="",header=FALSE)
actlab<-read.csv("activity_labels.txt",sep="",header=FALSE)
features<-read.csv("features.txt",sep="",header=FALSE)
strains<-read.csv("train/subject_train.txt",sep="",header=FALSE)
stest<-read.csv("test/subject_test.txt",sep="",header=FALSE)


ete<-NULL
for (i in 1:length(ltest[,1]))
 ete[i]<- as.character(actlab[[ltest[i,],2]])

etr<-NULL
for (i in 1:length(ltrain[,1]))
 etr[i]<- as.character(actlab[[ltrain[i,],2]])

ftest<-cbind(test,ete)
ftrain<-cbind(train,etr)

names(ftest)[562]<-"activity"
names(ftrain)[562]<-"activity"

noms<-features[,2]

all<-rbind(ftrain,ftest)
names(all)<-noms
names(all)[562]<-"activity"

subjects<-rbind(strains,stest)

allc<-cbind(all,subjects)
names(allc)[563]<-"subject"

noms<-names(allc)

ismd<-NULL
for (i in 1:length(noms))
{
 ismd[i]<-grepl("mean()-",noms[i])||grepl("std()",noms[i])
}

vindex<-NULL
k<-1
for (i in 1:length(noms))
{
if (ismd[i]==TRUE) 
	{
	vindex[k]<-i
	k<-k+1
	}
}

#add subject and activity)
vindex<-c(vindex,562,563)

#We select only the variables for mean and std
allcf<-allc[,vindex]

###first tidy data 
td1<-NULL
k<-1
for (i in 1:(length(allcf[1,])-2))
{
 td1[k]<-mean(all[,i])
 td1[k+1]<-sd(all[,i])
 k<-k+2
}


###second tidy data 

td2<-matrix(ncol=33,nrow=(30*6))
k<-1
 for (sb in 1:30) #subjects
    for (act in 1:6) #activity
{
	fd<-subset(allcf,subject==sb&activity==actlab[[act,2]])
	for (i in 1:(length(allcf[1,])-2))
	{
	 td2[k,i]<-mean(fd[,i])
	}
 k<-k+1
}






