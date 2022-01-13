setwd("C:\\Users\\sanghaip\\OneDrive - Merck Sharp & Dohme, Corp\\Desktop\\Clustering\\temp")
library("readxl")
library(corrplot)
library(cluster) 
library(factoextra)
data1 <- read.csv("DATAfinal.csv")
ready<-data1[-c(1,10,11)]

normalized <- function(x) (x- min(x))/(max(x) - min(x))
for ( i in 1:8){
  ready[,i]<-normalized(ready[,i])
}
ready<-ready[-c(10)]
k<-kmeans(ready,6)
k
ready$cluster<-factor(ready$cluster)
class(ready$cluster)

for(a in 1:5){
  for(b in (a+1):6){
    result=paste("GDPpercapita  CLusters ",a,"&",b)
    print( result)
    first<-subset(ready,(ready$cluster==a|ready$cluster==b))
   
    x<-summary(aov(first$GDPpercapita~first$cluster))
    print(x)
  }
}
for(a in 1:5){
  for(b in (a+1):6){
    result=paste("Current health expenditure (% of GDP)  CLusters ",a,"&",b)
    print( result)
    first<-subset(ready,(ready$cluster==a|ready$cluster==b))
    
    x<-summary(aov(first$CurrentHE~first$cluster))
    print(x)
  }
}

for(a in 1:5){
  for(b in (a+1):6){
    result=paste("Government Schemes and health care financing schemes  CLusters ",a,"&",b)
    print( result)
    first<-subset(ready,(ready$cluster==a|ready$cluster==b))
    
    x<-summary(aov(first$GSHEFE~first$cluster))
    print(x)
  }
}

for(a in 1:5){
  for(b in (a+1):6){
    result=paste("Voluntary health care payment schemes  CLusters ",a,"&",b)
    print( result)
    first<-subset(ready,(ready$cluster==a|ready$cluster==b))
    
    x<-summary(aov(first$VHCPC~first$cluster))
    print(x)
  }
}

for(a in 1:5){
  for(b in (a+1):6){
    result=paste("Infant Mortality  CLusters ",a,"&",b)
    print( result)
    first<-subset(ready,(ready$cluster==a|ready$cluster==b))
    
    x<-summary(aov(first$IM~first$cluster))
    print(x)
  }
}

for(a in 1:5){
  for(b in (a+1):6){
    result=paste("Overall Immunization  CLusters ",a,"&",b)
    print( result)
    first<-subset(ready,(ready$cluster==a|ready$cluster==b))
    
    x<-summary(aov(first$OI~first$cluster))
    print(x)
  }
}


for(a in 1:5){
  for(b in (a+1):6){
    result=paste("Urban Population %  CLusters ",a,"&",b)
    print( result)
    first<-subset(ready,(ready$cluster==a|ready$cluster==b))
    
    x<-summary(aov(first$`Urban Population %`~first$cluster))
    print(x)
  }
}
for(a in 1:5){
  for(b in (a+1):6){
    result=paste("% GDP Growth  CLusters ",a,"&",b)
    print( result)
    first<-subset(temp,(temp$cluster==a|temp$cluster==b))
    
    x<-summary(aov(first$X..GDP.Growth~first$cluster))
    print(x)
  }
}




for(a in 1:5){
  for(b in (a+1):6){
    print(paste("Cluster ",a," & ",b))
    first<-subset(ready,(ready$cluster==a|ready$cluster==b))
    x<-summary.aov(manova(cbind(GDPpercapita,CurrentHE,GSHEFE,VHCPC,IM,OI,Urban.Population..,X..GDP.Growth)~cluster,data=first))
    print(x)
  }
}






ready$cluster<-as.factor(ready$cluster)
library(plotly)
library("plot3D")
ready
plot_ly(ready, x = ~GDPpercapita, y = ~OI, z = ~IM, color = ~cluster, colors = c('#BF382A', '#0C4B8E','#8c564b','#bcbd22','#9467bd','#2ca02c'),selectedpoints=TRUE)
layout = go.Layout(xaxis=dict(range=[0, 1]),
                   yaxis=dict(range=[0, 1]),zaxis=dict(range=[0, 1]))
add_markers()

contin<-read.csv("Clustering 20 results.csv")

contin[1,2:21]<-factor(contin[1,2:21])
contin[2,2:21]<-factor(contin[2,2:21])
table(contin[1,],contin[2,])
ready<-ready[-c(9)]
d <- dist(ready, method = "euclidean")
fit <- hclust(d, method="ward") 
plot(fit)
groups <- cutree(fit, k=6)
rect.hclust(fit, k=6, border="red")

ready$country<-data1$Country
nstarttrial<-ready[-c(2,3,4,7,8)]
k<-kmeans(y,6,iter.max=2000,nstart=30)
k
?kmeans
fviz_cluster(k, data = nstarttrial)
ready$cluster<-k$cluster
class(ready$cluster)
ready$cluster<-factor(ready$cluster)
?fviz_cluster
k
clusplot(nstarttrial, nstarttrial$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
withcluster<-c[nstarttrial,factor(data1$cluster)]
withcluster<-nstarttrial
withcluster$cluster<-data1$cluster
withcluster$cluster<-factor(withcluster$cluster)
nstarttrial<-nstarttrial[-c(4)]
with(withcluster, pairs(nstarttrial, col=c(1:6)[k$cluster]))
temp<-read.csv("Final Cluster output.csv")
ready$cluster<-factor(temp$cluster)
class(ready$cluster)
x<-ready[-c(9)]
?elbow
??elbow
k
x<-c(1,2,3,4,5,6)
z<-k$centers
v<-c(1,2,3,4,5,6)
y<-ready[-c(9)]
final<-cbind(z,factor(v))
final$V9<-factor(final$V9)
class(final$V9)
final<-as.data.frame(final)
final<-final[-c(2,3,4,7,8)]
axx <- list(
  range = c(0,1)
)

axy <- list(
  
  range = c(0,1)
)

axz <- list(
  
  range = c(0,1)
)
withcluster$cluster<-factor(withcluster$cluster)
plot_ly(withcluster, x = ~GDPpercapita, y = ~OI, z = ~IM, color = ~cluster, colors = c('#BF382A', '#0C4B8E','#8c564b','#bcbd22','#9467bd','#2ca02c'),selectedpoints=TRUE)%>%
  layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))





plot_ly(ready, x = ~GDPpercapita, y = ~OI, z = ~IM, color = ~cluster, colors = c('#BF382A', '#0C4B8E','#8c564b','#bcbd22','#9467bd','#2ca02c'),selectedpoints=TRUE,type='mesh3d')
temp <- read.csv("temp.csv")
temp$cluster<-factor(temp$cluster)
temp$country<-factor(temp$country)
temp<-temp[-c(2,3,4,6,7,8)]
axx <- list(
  range = c(0,1)
)

axy <- list(
  
  range = c(0,1)
)
plot_ly(temp, x = ~GDPpercapita, y = ~IM, color = ~cluster, colors = c('#BF382A', '#0C4B8E'),selectedpoints=TRUE)%>%
layout(scene = list(xaxis=axx,yaxis=axy))

class(temp$cluster)
clusplot(temp, temp$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)