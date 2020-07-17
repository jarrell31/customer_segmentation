setwd('C:/Users/Owner/Desktop/The 9 Projects/Customer Segmentation')

#read in data
mall_customers <- read.csv("Mall_Customers.csv")

#check structure
str(mall_customers)

#check for missing data
colSums(is.na(mall_customers))

str(mall_customers)

#remove customer id
mall_customers <- mall_customers[,-1]

#explore data
table(mall_customers$Gender)

hist(mall_customers$Age, col = "blue")
boxplot(mall_customers$Age, col = "yellow")
plot(density(mall_customers$Age), main = "Density Plot of Age")

hist(mall_customers$Annual.Income..k.., col = "blue")
boxplot(mall_customers$Annual.Income..k.., col = "yellow")
plot(density(mall_customers$Annual.Income..k..), main = "Density Plot of Annual Income")

hist(mall_customers$Spending.Score..1.100., col = "blue")     
boxplot(mall_customers$Spending.Score..1.100., col = "yellow")

library(ggplot2)
ggplot(data = mall_customers) + 
  geom_bar(mapping = aes(x = Annual.Income..k.., fill = Gender), position = "dodge")

ggplot(data = mall_customers) + 
  geom_bar(mapping = aes(x = Spending.Score..1.100., fill = Gender), position = "dodge")

#change male/female to binary
mall_customers$Gender <- ifelse(mall_customers$Gender == "Male", 1, 0)

#scale variables
mall_cust_scale <- data.frame(scale(mall_customers))

#find number of clusters
#install.packages("NbClust")
library(NbClust)
num_clust <- NbClust(mall_cust_scale[,c(2:4)], distance = "euclidean",
                     min.nc = 2,
                     max.nc = 8,
                     method = "complete",
                     index = "all")

num_ward <- NbClust(mall_cust_scale[,2:4], distance = "euclidean",
                    min.nc = 2,
                    max.nc = 8,
                    method = "ward.D2",
                    index = "all")

kmeans1 <- NbClust(mall_cust_scale[,2:4], 
                  min.nc = 2, 
                  max.nc = 8, 
                  method = "kmeans")

set.seed(123)
gap_stat <- cluster::clusGap(mall_cust_scale[,2:4], FUNcluster = kmeans, nstart = 25, K.max = 10, B = 50)
#install.packages("factoextra")
library(factoextra)
fviz_gap_stat(gap_stat)

km <- kmeans(mall_cust_scale[,c(2:4)], 6, nstart = 25)
table(km$cluster)

mall_customers$cluster <- km$cluster

ggplot(data = mall_customers) +
  geom_point(aes(x = Annual.Income..k.., y = Spending.Score..1.100., color = as.factor(cluster)))

