setwd("C:\\Users\\sanghaip\\OneDrive - Merck Sharp & Dohme, Corp\\Documents\\data science\\IntroToDataScience-master\IntroToDataScience-master)\
library("readxl")
setwd("C:\\Users\\sanghaip\\OneDrive - Merck Sharp & Dohme, Corp\\Desktop\\Clustering")

data <- read_excel("C:\\Users\\sanghaip\\OneDrive - Merck Sharp & Dohme, Corp\\Desktop\\Clustering\\Data for Clustering.xlsx")
library("xlsx")
data <- read_excel("absolute.xlsx")
ready<-data1[-c(1,2,3,4,15)]

      normalized <- function(x) (x- min(x))/(max(x) - min(x))
      for ( i in 1:8){
      ready[,i]<-normalized(ready[,i])
      }
my_data <- read.table(file = "C:\\Users\\sanghaip\\OneDrive - Merck Sharp & Dohme, Corp\\Desktop\\Clustering\\Data for Clustering.xlsx",sep = "\t")

install.packages('corrplot')
library(corrplot)
library(cluster) 
library(factoextra)
readydata<-data[-c(1,3,4,15)]
silhouette_score <- function(k){
  km <- kmeans(readydata, centers = k, nstart=25)
      ss <- silhouette(km$cluster, dist(readydata))
      mean(ss[, 3])
      }
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
rm(data)
data <- read_excel("Data.xlsx",2,col_names = TRUE)
readydata<-data[-c(1,2,3,4)]
fviz_nbclust(readydata, kmeans, method = "silhouette", k.max = 24) + theme_minimal() + ggtitle("The Silhouette Plot")
fviz_nbclust(readydata, kmeans, method='silhouette')
kmeans(readydata,7)
normdata<-scale(readydata)
kmeans(normdata,7)
sample(normdata)
head(normdata)
readydata<-readydata[-c(11)]
normalized <- function(x) (x- min(x))/(max(x) - min(x))
for ( i in 1:8){
readydata[,i]<-normalized(readydata[,i])
}
k<-kmeans(readydata,7)
readydata$cluster<-k$cluster
class(readydata$cluster)
for ( i in 1:9){
readydata[,i]<-unlist(readydata[,i])
}
k
readydata$country<-data$Country
fviz_nbclust(readydata, kmeans, method = "silhouette", k.max = 24) + theme_minimal() + ggtitle("The Silhouette Plot")
res.man <- manova(readydata[-c(10)] ~ readydata[,10])
manova(readydata[9]~readydata[1]+readydata[2])
readydata$cluster<-factor(readydata$cluster)I

  
out<-for(a in 1:7){
  for(b in a:7){
result=paste("GDPpercapita  CLusters ",a,"&",b)
print( result)
x<-summary(aov(ready$GDPpercapita~(ready$cluster==a)+(ready$cluster==b)))
print(x)
  }
}
out <- capture.output(for(a in 1:7){
  for(b in a:7){
result=paste("GDPpercapita  CLusters ",a,"&",b)
print( result)
x<-summary(aov(ready$GDPpercapita[(ready$cluster==a)]~ready$GDPpercapita[(ready$cluster==b)]))
print(x)
}
})

cat("My title", out, file="summary_of_my_very_time_consuming_regression.csv")


for(a in 1:7){
  for(b in a:7){
result=paste("Current health expenditure (% of GDP)  CLusters ",a,"&",b)
print( result)
x<-summary(aov(ready$CurrentHE~(ready$cluster==a)+(ready$cluster==b)))
print(x)
  }
  }



for(a in 1:7){
  for(b in a:7){
result=paste("Government Schemes and health care financing schemes  CLusters ",a,"&",b)
print( result)
x<-summary(aov(ready$GSHEFE~(ready$cluster==a)+(ready$cluster==b)))
print(x)
}
}


 
for(a in 1:7){
  for(b in a:7){
result=paste("Voluntary health care payment schemes CLusters ",a,"&",b)
print( result)
x<-summary(aov(ready$VHCPC~(ready$cluster==a)+(ready$cluster==b)))
print(x)
}
}




for(a in 1:7){
  for(b in a:7){
result=paste("Infant Mortality CLusters ",a,"&",b)
print( result)
x<-summary(aov(ready$IM~(ready$cluster==a)+(ready$cluster==b)))
print(x)
}
}



for(a in 1:7){
  for(b in a:7){
result=paste("Overall Immunization CLusters ",a,"&",b)
print( result)
x<-summary(aov(ready$OI~(ready$cluster==a)+(ready$cluster==b)))
print(x)
  }
}



for(a in 1:7){
  for(b in a:7){
result=paste("Urban Population % CLusters ",a,"&",b)
print( result)
x<-summary(aov(ready$UP~(ready$cluster==a)+(ready$cluster==b)))
print(x)
}
}


for(a in 1:7){
  for(b in a:7){
result=paste("% GDP Growth CLusters ",a,"&",b)
print( result)
x<-summary(aov(ready$GDPgrowth~(ready$cluster==a)+(ready$cluster==b)))
print(x)
  }
}

}


for(a in 1:7){
  for(b in a:7){
result=paste("GDPpercapita  CLusters ",a,"&",b)
print( result)
first<-subset(ready$GDPpercapita, ready$cluster==a)
second<-subset(ready$GDPpercapita, ready$cluster==b)
x<-summary(aov(first~second))
print(x)
}
}
for(a in 1:7){
  for(b in a:7){
result=paste("GDPpercapita  CLusters ",a,"&",b)
print( result)
first<-subset(ready,ready$cluster==a)
second<-subset(ready,ready$cluster==b)
x<-summary(aov(first$GDPpercapita~second$GDPpercapita))
print(x)
}
}
