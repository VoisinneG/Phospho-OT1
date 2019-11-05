#phospho OT-I analysis
library(xlsx)
library(lattice)
library(ggplot2)
library("RColorBrewer")
library(reshape)
library(devtools)
library(gridExtra)
library(stringr)
library(svglite)
library(Rtsne)
library(ggrepel)
library(plotly)
library(ClusterX)
library(RColorBrewer)
library(cluster)

setwd("S:/Romain/myCloud/Phospho-CD4 backup/New dataset Tet")
kinetic<-read.csv("pTyr injection1_Phospho (STY)Sites.csv",sep=";", dec=",",stringsAsFactors=FALSE)

genes<-"Itsn2" 

listg1<-read.table(text = genes, sep = ",", as.is = TRUE)
listg1[1,1]<-paste("",listg1[1,1])
listg1[1,]<-toupper(str_trim(listg1))
obs1<-length(listg1[1,])

df<-data.frame(matrix(NA,0,16))

X<-1
for (i in 1:obs1){
  
  a<-which(toupper(kinetic$Gene.names)==listg1[1,X])
  
  
  nbsite<-length(a)
  if (nbsite!=0) {
    
    df1<-subset(kinetic,toupper(Gene.names)==listg1[1,X])
    
  }
  
  df<-rbind(df,df1)
  
  X<-X+1}


df2<-data.frame(matrix(NA,nbsite,10))


colnames(df2)<-c("Ac","Gene","position","site","mean NS","mean 15S","mean 30S", "mean 120S","mean 300S", "mean 600S")

df2$Ac<-df$Protein
df2$Gene<-df$Gene.names
df2$position<-df$Positions.within.proteins
df2$site<-df$Amino.acid


INT<-grep("Intensity", colnames(df1)) 
#group<-grep("___1",colnames(df1))
#INT<-intersect(INT,group)


NS<-grep("NS", colnames(df))
S15<-grep("S15", colnames(df))
S30<-grep("S30", colnames(df))
S120<-grep("S120", colnames(df))
S300<-grep("S300", colnames(df))
S600<-grep("S600", colnames(df))

X<-1
for ( i in 1:length(df[,1])){
  
  
  
  
   df2$`mean NS`[X]<-mean(as.numeric(df[X,intersect(INT, NS)]), na.rm = TRUE)
   df2$`mean 15S`[X]<-mean(as.numeric(df[X,intersect(INT, S15)]), na.rm = TRUE)
   df2$`mean 30S`[X]<-mean(as.numeric(df[X,intersect(INT, S30)]), na.rm = TRUE)
   df2$`mean 120S`[X]<-mean(as.numeric(df[X,intersect(INT, S120)]), na.rm = TRUE)
   df2$`mean 300S`[X]<-mean(as.numeric(df[X,intersect(INT, S300)]), na.rm = TRUE)
   df2$`mean 600S`[X]<-mean(as.numeric(df[X,intersect(INT, S600)]), na.rm = TRUE)

   
   X<-X+1}

df3<-df2

X<-1
for ( i in 1:length(df[,1])){
  
  max<-max(df3[X,5:10], na.rm= TRUE)
  df3[X,5:10]<-df2[X,5:10]/max

  X<-X+1}

df3[df3=="NaN"]<-0
df3$Aposition<-paste(df3$site,df3$position )

N4<-ggplot(melt(df3[,c(11,5:10)]),aes(x = variable ,y=value, group=Aposition))+
  #geom_point(alpha=0.2 )+
  
  geom_line(size=2)
  
  
N4
