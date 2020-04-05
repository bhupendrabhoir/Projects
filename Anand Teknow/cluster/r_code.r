setwd("E:\\Anand Teknow\\Model")

dat <- read.csv(file = "exp.csv",header = T,sep = ",")
data <- read.csv(file="Data.csv",header = T)
data <- data[,14:24]
View(dat)


str(dat)
class(dat)


for(i in c(6,10,11,12,13:ncol(dat))) {
  dat[,i] <- as.numeric(dat[,i])
}

var.data <- dat[,c(20:28)]

library(NbClust)

summary(var.data)


nc <- NbClust(data,min.nc = 2,max.nc = 5,method = "kmeans")

barplot(table(nc$Best.nc[1,]),xlab="No. of clusters",ylab="No. of criteria",main="No. of clusters choosen")



emp.cluster <- kmeans(var.data,2)

dat$cluster <- emp.cluster$cluster



write.csv(dat,file = "Clusters.csv")

emp.cluster$centers

library(ggplot2)


ggplot(var.data, aes(Average.monthly.hours,Satisfaction.Level,last_evaluation,average_montly_hours,number_project,average_montly_hours,salary, color = emp.cluster$cluster)) + geom_point()

