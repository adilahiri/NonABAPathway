setwd("C:/Users/adi44/Desktop/CodeHouse/NonABAPathway")
source('C:/Users/adi44/Desktop/CodeHouse/NonABAPathway/minmax_normalize.R')
source('C:/Users/adi44/Desktop/CodeHouse/NonABAPathway/binarize_mean_median.R')

library(Binarize)
require(xlsx)
set.seed(101)
data_series<- as.matrix(read.xlsx("subset.xlsx",1,header=FALSE))
data_series<-data_series[,-c(seq(2,209,2))]
data_series<-t(data_series)
data_series<-data_series[-c(1),]
colnames(data_series)<-c("MAP3K15","MKK4","MPK6",
                         "WRKY59","DRIP1","DREB2A",
                         "HOS1","SIZ1","ICE1","MYB15",
                         "SAP5","LOS2","ZAT10","DREB1A",
                          "CBF4","RD29A")
rownames(data_series)<-NULL
data_series<-apply(data_series,2,as.numeric)
data_norm<- apply(data_series,2,minmax_normalize)
data_binary <- binarize_mean_median(data_norm,method="mean")

count1<-list()
for(iter in 1:16){
  count1[[iter]]<- table(data_binary[,iter])
}

c1<-unlist(count1)
c2<-matrix(c1,ncol=2,byrow=TRUE)

colnames(c2)<-c("Inhibited","Activated")
rownames(c2)<-c("MAP3K15","MKK4","MPK6",
                "WRKY59","DRIP1","DREB2A",
                "HOS1","SIZ1","ICE1","MYB15",
                "SAP5","LOS2","ZAT10","DREB1A",
                "CBF4","RD29A")


barplot(t(c2),ylim =c(0,150),ylab="count",col=c("black","red"))
title(main="Activation vs Inhibition")
legend("topright",c("Activated","Inhibited"),fill=c("red","black"),cex=0.8)

