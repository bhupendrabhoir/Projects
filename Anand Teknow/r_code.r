setwd("E:\\Training & Coaching\\Oil and Gas\\Cluster data")

data <- read.csv(file="data_cleaned.csv",header = T,sep = ",")
View(data)

library(NbClust)

var.data <- data[,2:8]
#var.data2 <- data[,3:11]
#var.data <- var.data[-c(1001:14999),]
View(var.data)
#var.data <- var.data[,1:5]


dim(var.data)
nc <- NbClust(var.data,min.nc = 2,max.nc = 5,method = "kmeans")

barplot(table(nc$Best.nc[1,]),xlab="No. of clusters",ylab="No. of criteria",main="No. of clusters choosen")

emp.cluster <- kmeans(var.data,3)

var.data$cluster <- emp.cluster$cluster

write.csv(var.data,file = "clusters.csv")

emp.cluster.1 <- var.data[var.data$cluster==1,]
emp.cluster.2 <- var.data[var.data$cluster==2,]
emp.cluster.3 <- var.data[var.data$cluster==3,]

View(var.data)
write.csv(var.data,file = "Clusters.csv")

emp.cluster$centers

library(ggplot2)
var.data$cluster <- as.factor(var.data$cluster)

ggplot(var.data, aes(satisfaction_level,last_evaluation,number_project,average_montly_hours,time_spend_company,salary)) + geom_point()
emp.cluster$cluster <- as.factor(emp.cluster$cluster) 
ggplot(var.data, aes(average_montly_hours,number_project,last_evaluation,average_montly_hours,number_project,average_montly_hours,salary, color = emp.cluster$cluster)) + geom_point()

var.data1 <- var.data[,-8]
cor(var.data1)


require(cluster)
library(fpc)
var <- as.matrix(var.data)
z <- plotcluster(var.data,emp.cluster$cluster)

plotcluster(var,emp.cluster$cluster)
dat <- iris[,-5]
clus <- kmeans(dat,3)
plotcluster(dat,clus$cluster)
View(dat)

ggplot(var.data, aes(satisfaction_level,time_spend_company,last_evaluation,number_project,average_montly_hours,time_spend_company)) + geom_point()

ggplot(var.data, aes(satisfaction_level,salary,last_evaluation,, color = var.data$cluster)) + geom_point()



emp.cluster1 <- kmeans(var.data,2)
var.data1 <- var.data

var.data1 <- var.data1[,-6] 
var.data1$clusters <- emp.cluster.1$cluster

emp.cluster1$size



emp.cluster1$centers

