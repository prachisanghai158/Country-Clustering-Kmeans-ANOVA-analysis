setwd("C:\\Users\\sanghaip\\OneDrive - Merck Sharp & Dohme, Corp\\Desktop\\Clustering\\temp")
library("readxl")
library(corrplot)
library(cluster) 
library(factoextra)
library(plotly)
library("plot3D")
data1 <- read.csv("DATA.csv")
ready<-data1[-c(9)]
normalized <- function(x) (x- min(x))/(max(x) - min(x))
for ( i in 1:8){
  ready[,i]<-normalized(ready[,i])
}
k<-kmeans(ready,6,iter.max=2000,nstart=30)
k
ready$cluster<-factor(k$cluster)






axx <- list(  range = c(0,1))
axy <- list(  range = c(0,1))
axz <- list(  range = c(0,1))
plot_ly(ready, x = ~GDPpercapita, y = ~OI, z = ~IM, color = ~cluster, colors = c('#BF382A', '#0C4B8E','#8c564b','#bcbd22','#9467bd','#2ca02c'),selectedpoints=TRUE)%>%
layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))