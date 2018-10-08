setwd("C:/Users/adi44/Desktop/CodeHouse/NonABAPathway")
source('C:/Users/adi44/Desktop/CodeHouse/NonABAPathway/minmax_normalize.R')
source('C:/Users/adi44/Desktop/CodeHouse/NonABAPathway/binarize_mean_median.R')
source('C:/Users/adi44/Desktop/CodeHouse/NonABAPathway/calc_shape_param.R')

library(Binarize)
require(xlsx)
set.seed(101)
###################### LOAD THE DATASET #########################
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
# Binarize the data 
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

# Estimate the shape parameters and expected values for the beta-binomial model
Nodes <- 16
obs<- nrow(data_binary)

alpha_init <- 1
beta_init <- 1
shape_param <- list()
Parents<-matrix()
shape_mat<- matrix(ncol=2)
for (iter_node in 1:Nodes){
  
  if (iter_node==1 ||iter_node==5||iter_node==7||iter_node==8||iter_node==10||iter_node==11||iter_node==15)
    Parents <- NULL
  else if( iter_node ==2)
    Parents<- cbind(data_binary[,1])
  else if( iter_node ==3)
    Parents<- cbind(data_binary[,2])
  else if(iter_node==4)
    Parents<-cbind(data_binary[,4])
  else if(iter_node==6)
    Parents<- cbind(data_binary[,4],data_binary[,5])
  else if(iter_node==9)
    Parents<- cbind(data_binary[,7],data_binary[,8])
  else if(iter_node==12)
    Parents<- cbind(data_binary[,11])
  else if(iter_node==13)
    Parents<- cbind(data_binary[,12])
  else if(iter_node==14)
    Parents<- cbind(data_binary[,9],data_binary[,10],data_binary[,13])
  else if(iter_node==16)
    Parents<- cbind(data_binary[,6],data_binary[,14],data_binary[,15])
  
  
  shape_param [[iter_node]] <- calc_shape_param(alpha_init,beta_init,data_binary[,iter_node],Parents)
  shape_mat <- rbind(shape_mat,shape_param[[iter_node]])
}

shape_mat <-shape_mat[-1,]

Expected_Values <- signif(cbind(shape_mat[,1]/(shape_mat[,1]+shape_mat[,2])),digits = 3)

shape_mat2<-cbind(shape_mat,100*Expected_Values,100*(1-Expected_Values))















  