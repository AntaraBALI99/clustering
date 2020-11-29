########clustring assignment ##############
######## hierarchical clustering ##########
##########dataset= eastwestairlines########### 


attach(EastWestAirlines)
View(EastWestAirlines)

########################## normalized data ##################################
normalized_data <- scale(EastWestAirlines)
View(normalized_data)

################## calculating distance = euclidean distance ###################
d <-  dist(normalized_data , method = "euclidean")
d

##################### hierarchical clustering using complete linkage##############
fit <-  hclust (d , method = "complete")
fit

####################### for dendogram ####################
plot(fit)
plot(fit,hang = -1)

######################### cut dendogram into 5 clusters ###########################
groups <- cutree(fit , k =5)
rect.hclust(fit,k=5,border = "red")
membership <- as.matrix(groups)
View(membership)
final <- data.frame(EastWestAirlines,membership)
View(final)
final1 <- final [ , c (13,1,2,3,4,5,6,7,8,9,10,11,12 )]
View(final1)

######### k means clustering #######
install.packages("plyr")
library("plyr")
View(EastWestAirlines)

####################################Normalize data #################
normalized_data <- scale(EastWestAirlines[ , 2:12])
View(normalized_data)
summary(normalized_data)

############ elbow curve to decide k value (k=?) #########################
install.packages("factoextra")
library("factoextra")
fviz_nbclust(EastWestAirlines , kmeans , method = "wss")+labs(subtitle = "elbowmehod")
fit <- kmeans(normalized_data , 9)
fit
str(fit)
attach(fit)
centers
cluster
final <- data.frame(EastWestAirlines,cluster)
final
View(final)

######## vizualization ###########
install.packages("animation")
library("animation")
km <- kmeans.ani(EastWestAirlines,9)
