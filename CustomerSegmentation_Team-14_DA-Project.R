## READING THE DATA
data=read.csv("Mall_Customers.csv")

View(data)

#------------------------------------------------------------------------------#
### DATA EXPLORATION
class(data) # class of data object

dim(data) # dimensions((rows,columns)) of data

str(data) # compact display of the internal structure of data

names(data) # column names of the data

colnames(data) # column names of the data

head(data) # view of the data from the top

tail(data) # view of the data from the bottom

summary(data) # summary of data

is.factor(data$Gender)
gender <- factor(data$Gender)
print(head(gender))

summary(data$Age)
mean(data$Age)
sd(data$Age)

summary(data$Annual.Income)
mean(data$Annual.Income)
sd(data$Annual.Income)

summary(data$Spending.Score)
mean(data$Spending.Score)
sd(data$Spending.Score)

#------------------------------------------------------------------------------#
### DATA PREPROCESSING

# Removing Leading and Trailing Whitespaces
data$CustomerID<-as.character(data$CustomerID)
data$Gender<-as.character(data$Gender)
data$Age<-as.character(data$Age)
data$Annual.Income<-as.character(data$Annual.Income)
data$Spending.Score<-as.character(data$Spending.Score)

library("dplyr")
library("stringr")

data<-data%>%mutate_if(is.character,str_trim)

data$CustomerID<-as.numeric(data$CustomerID)
data$Gender<-as.factor(data$Gender)
data$Age<-as.numeric(data$Age)
data$Annual.Income<-as.numeric(data$Annual.Income)
data$Spending.Score<-as.numeric(data$Spending.Score)

# Dealing with 'na' values
is.na(data)
sum(is.na(data))
na.omit(data)->data
data[complete.cases(data),]->data

# replacing 'na' values with mean of the column if exists
data$Age[is.na(data$Age)]<-mean(data$Age,na.rm=TRUE)
data$Annual.Income[is.na(data$Annual.Income)]<-mean(data$Annual.Income,na.rm=TRUE)
data$Spending.Score[is.na(data$Spending.Score)]<-mean(data$Spending.Score,na.rm=TRUE)
class(data$CustomerID)
data$CustomerID[is.na(data$CustomerID)]<-mean(data$CustomerID,na.rm=TRUE)
class(data$Gender)
data$Gender[is.na(data$Gender)]<-'Female'

# separating/splitting data using subset function (DATA EXTRACTION)
newdata<-subset(data,data$Gender=='Female')
print(newdata)
dim(newdata)

library(caTools)
split<-sample.split(data$Age, SplitRatio = 0.5)
data1<-subset(data,split==TRUE)
print(head(data1))
dim(data1)

data2<-subset(data,split==FALSE)
print(head(data2))
dim(data2)

freq_age<-table(data$Age)
print(freq_age)

gender<-table(data$Gender)
print(gender)

res<-data%>% arrange(data$Age)
print(head(res,3))

res<-data%>% arrange(desc(data$Age))
print(head(res,3))

dat<-data%>%arrange(desc(data$CustomerID))
print(head(dat))

cnt<-data%>% count(data$Age)
print(head(cnt))

cnt1<-data%>% count(data$Annual.Income)
print(head(cnt1))

res2<-data%>% group_by(data$Gender) %>% summarise(Mean= mean(data$Spending.Score))
print(res2)

write.csv(data,"updated_data.csv")
df=read.csv("updated_data.csv")
View(df)


#------------------------------------------------------------------------------#
### VISUALIZATION

data=read.csv("Mall_Customers.csv")

## Customer Gender Visualization
# Bar Graph
a=table(data$Gender)
barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))

# Pie Chart
pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,main="Pie Chart Depicting Ratio of Female and Male")


## Visualization of Age Distribution

summary(data$Age)

# Histogram
hist(data$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

# Box plot
boxplot(data$Age,
        col="990000",
        main="Boxplot for Descriptive Analysis of Age")


## Analysis of the Annual Income of the Customers

summary(data$Annual.Income)

# Histogram
hist(data$Annual.Income,
     col="#660033",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)

# Density Plot
plot(density(data$Annual.Income),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(data$Annual.Income),
        col="#ccff66")


## Analyzing Spending Score of the Customers

summary(data$Spending.Score)

# Box Plot
boxplot(data$Spending.Score,
        horizontal=TRUE,
        col="#990000",
        main="BoxPlot for Descriptive Analysis of Spending Score")

# Histogram
hist(data$Spending.Score,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#6600cc",
     labels=TRUE)


#------------------------------------------------------------------------------#
### CLASSIFICATION

## NAIVE BAYES

# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("caret")

# Loading package
library(e1071)
library(caTools)
library(caret)

data=read.csv("Mall_Customers.csv")
head(data)
# Splitting data into train and test data
split <- sample.split(data, SplitRatio = 0.7)
train_cl <- subset(data, split == "TRUE")
head(train_cl)
dim(train_cl)

test_cl <- subset(data, split == "FALSE")
head(test_cl)
dim(test_cl)

# Feature Scaling
train_scale <- scale(train_cl[, 3:5])
test_scale <- scale(test_cl[, 3:5])

# Fitting Naive Bayes Model to training dataset
set.seed(120) # Setting Seed
classifier_cl <- naiveBayes(Gender ~ ., data = train_cl)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$Gender, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)


#------------------------------------------------------------------------------#
### K-MEANS CLUSTERING

data=read.csv("Mall_Customers.csv")

## Method-1 --> Elbow Method
library(purrr)
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10

iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")


## Method-2 --> Average Silhouette Method
library(cluster) 
library(gridExtra)
library(grid)

# k = number of clusters = 2
k2<-kmeans(data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(data[,3:5],"euclidean")))

# k = number of clusters = 3
k3<-kmeans(data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(data[,3:5],"euclidean")))

# k = number of clusters = 4
k4<-kmeans(data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(data[,3:5],"euclidean")))

# k = number of clusters = 5
k5<-kmeans(data[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(data[,3:5],"euclidean")))

# k = number of clusters = 6
k6<-kmeans(data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(data[,3:5],"euclidean")))

# k = number of clusters = 7
k7<-kmeans(data[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(data[,3:5],"euclidean")))

# k = number of clusters = 8
k8<-kmeans(data[,3:5],8,iter.max=100,nstart=50,algorithm="Lloyd")
s8<-plot(silhouette(k8$cluster,dist(data[,3:5],"euclidean")))

# k = number of clusters = 9
k9<-kmeans(data[,3:5],9,iter.max=100,nstart=50,algorithm="Lloyd")
s9<-plot(silhouette(k9$cluster,dist(data[,3:5],"euclidean")))

# k = number of clusters = 10
k10<-kmeans(data[,3:5],10,iter.max=100,nstart=50,algorithm="Lloyd")
s10<-plot(silhouette(k10$cluster,dist(data[,3:5],"euclidean")))

#install.packages("NbClust")
library(NbClust)
#install.packages("factoextra")
library(factoextra)

fviz_nbclust(data[,3:5], kmeans, method = "silhouette")


## Method-3 --> Gap Statistic Method
set.seed(125)
stat_gap <- clusGap(data[,3:5], FUN = kmeans, nstart = 25,K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

k6<-kmeans(data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6

# Visualizing the Clustering Results using the First Two Principle Components
pcclust=prcomp(data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[,1:2]

set.seed(1)
ggplot(data, aes(x =Annual.Income, y = Spending.Score)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")


ggplot(data, aes(x =Spending.Score, y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k6$cluster
dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))

#-------------------------------- THE END -------------------------------------#