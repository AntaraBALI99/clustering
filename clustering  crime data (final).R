###### crime data set #########
### hierarchical clustering #########

install.packages("readxl")
library("readxl")
attach(crime_data)
mydata <- crime_data[ ,(2:5)]
attach(mydata)

########### normalized data #################################
normalized_data <-  scale(mydata)
View(normalized_data)

######################## calculating distance ###############
d <- dist(normalized_data , method = "euclidean")
d

##################### creating dendogram using complete linkage ############
fit <- hclust(d , method = "complete")
fit
plot(fit)
plot(fit,hang=-1)

############# cut dendogram into 4 clusters ################
groups <-  cutree (fit , k=4)
rect.hclust(fit , k=4, border = "red")
membership <- as.matrix(groups)
View(membership)
final <-  data.frame(crime_data , membership)
View(final)
final1 <-  final [ ,c(6,1,2,3,4,5)]
View(final1)

############################# k means clustering ###################
View(crime_data)
mydata <- crime_data[-1]
View(mydata)

######### normalized data ###########################
normalized_data <- scale(mydata)
View(normalized_data)
summary(normalized_data)

######### elbow curve  to decide the k value ##############
install.packages("factoextra")
library("factoextra")
fviz_nbclust(mydata , kmeans , method = "wss")+ labs(subtitle = "elbowmethod")

## i observed this elbow curved, and i got the k value is 10 (k=10)

################### k means clustering ##########
fit <- kmeans (normalized_data , 10)
str(fit)
attach(fit)
centers
cluster
final <- data.frame(crime_data,cluster)
View(final)

###### vizulization ##########################
install.packages("animation")
library("animation")
km <- kmeans.ani (mydata,10)
