setwd("C:/Users/thoma/Desktop/Stat 717/Project/")
mcd <- read.csv("McDonalds_Menu.csv")

library(mclust)
library(lattice)
library(dplyr)
library(cluster)
library(ggplot2)
library(corrplot)
library(NbClust)
library(MASS)
library(class)
library(ISLR)

############################## Exploratory Analysis #######################33

#Look at dataset
str(mcd)
summary(mcd)
summary(mcd$Category)

#food
mcd_food <- mcd %>%
  filter(Category == "Beef & Pork" | Category == "Breakfast" | Category == "Chicken & Fish" | Category == "Salads" | Category == "Snacks & Sides")
#drink
mcd_drink <- mcd %>%
  filter(Category == "Beverages" | Category == "Coffee & Tea" | Category == "Smoothies & Shakes" | Category == "Desserts") %>%
  filter(Calories != 0)

############## Items vs Calories ################
#counts
ggplot(data=mcd, aes(x=Category)) +
  geom_bar()

#plots for Breakfast
ggplot(subset(mcd,Category=="Breakfast"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Breakfast Vs Calories") + xlab("Item") + ylab("Calories")  + coord_flip()
#plots Beef and Pork
ggplot(subset(mcd,Category=="Beef & Pork"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Beef/Pork Vs Calories") + xlab("Item") + ylab("Calories")  + coord_flip()
#plots Chicken and Fish
ggplot(subset(mcd,Category=="Chicken & Fish"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Chicken/Fish Vs Calories") + xlab("Item") + ylab("Calories")  + coord_flip()
#plots salads
ggplot(subset(mcd,Category=="Salads"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Salads Vs Calories") + xlab("Item") + ylab("Calories")  + coord_flip()
#plots Snack and Sides
ggplot(subset(mcd,Category=="Snacks & Sides"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Snack/Sides Vs Calories") + xlab("Item") + ylab("Calories")  + coord_flip()
#plots desserts
ggplot(subset(mcd,Category=="Desserts"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Desserts Vs Calories") + xlab("Item") + ylab("Calories")  + coord_flip()
#plots Beverage
ggplot(subset(mcd,Category=="Beverages"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Beverage Vs Calories") + xlab("Item") + ylab("Calories")  + coord_flip()
#plots Smoothies/Shakes
ggplot(subset(mcd,Category=="Smoothies & Shakes"), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle("Smoothies/Shake Vs Calories") + xlab("Item") + ylab("Calories")  + coord_flip()

#corr plot
mcd_data <- mcd[4:24]
corr <- cor(mcd_data)
corrplot(corr, method="shade")

mcd_food1 <- mcd_food[,4:24]
mcd_drink1 <- mcd_drink[,4:24]
##qqplot
par(mfrow=c(3,3))
for (i in 1:21) {
  qqnorm(mcd_food1[,i],main=names(mcd_data)[i]) 
  qqline(mcd_food1[,i])
}

for (i in 1:21) {
  qqnorm(mcd_drink1[,i],main=names(mcd_data)[i]) 
  qqline(mcd_drink1[,i])
}

#Mean Daily Value of each Category
Food_content <- aggregate(cbind(Total.Fat....Daily.Value., Saturated.Fat....Daily.Value., Cholesterol....Daily.Value., Sodium....Daily.Value., Carbohydrates....Daily.Value., Dietary.Fiber....Daily.Value.,Vitamin.A....Daily.Value., Vitamin.C....Daily.Value.,Calcium....Daily.Value., Iron....Daily.Value.) ~ Category,
                         data = mcd_food, FUN = mean
)

Food_content

Drink_content <- aggregate(cbind(Total.Fat....Daily.Value., Saturated.Fat....Daily.Value., Cholesterol....Daily.Value., Sodium....Daily.Value., Carbohydrates....Daily.Value., Dietary.Fiber....Daily.Value.,Vitamin.A....Daily.Value., Vitamin.C....Daily.Value.,Calcium....Daily.Value., Iron....Daily.Value.) ~ Category,
                          data = mcd_drink, FUN = mean
)

Drink_content


################################################ LDA, KNN
# ALL MCD #
mcd_data1 <- mcd[,c(-2,-3)]
mcd_data1$Category <- as.numeric(mcd_data1$Category)

#LDA Prediction of Category
lda.fit <- lda(Category~., data=mcd_data1)
lda.pred <- predict(lda.fit)$class
table(mcd_data1$Category, lda.pred, dnn = c('Actual Group','Predicted Group'))

# MCD FOOD #
mcd_data2 <- mcd_food[,c(-2,-3)]
mcd_data2$Category <- as.numeric(mcd_data2$Category)

#LDA Prediction of Category
lda.fit <- lda(Category~., data=mcd_data2)
lda.pred <- predict(lda.fit)$class
table(mcd_data2$Category, lda.pred, dnn = c('Actual Group','Predicted Group'))

# MCD DRINK #
mcd_data3 <- mcd_drink[,c(-2,-3)]
mcd_data3$Category <- as.numeric(mcd_data3$Category)

#LDA Prediction of Category
lda.fit <- lda(Category~., data=mcd_data3)
lda.pred <- predict(lda.fit)$class
table(mcd_data3$Category, lda.pred, dnn = c('Actual Group','Predicted Group'))

#KNN Food 
#make training and test
sam_size <- floor(.6*nrow(mcd_data2))
set.seed(1234)
train_ind <- sample(seq_len(nrow(mcd_data2)), size=sam_size)
train <- mcd_data2[train_ind,]
test <- mcd_data2[-train_ind,]
cl <- mcd_data2[train_ind,1]
test_cl <- mcd_data2[-train_ind, 1]

knn.pred <- knn(train, test, cl, k=5, prob=T)
table(knn.pred, test_cl)

#KNN Drink
#make training and test
sam_size <- floor(.6*nrow(mcd_data3))
set.seed(1234)
train_ind <- sample(seq_len(nrow(mcd_data3)), size=sam_size)
train <- mcd_data3[train_ind,]
test <- mcd_data3[-train_ind,]
cl <- mcd_data3[train_ind,1]
test_cl <- mcd_data3[-train_ind, 1]

knn.pred <- knn(train, test, cl, k=5, prob=T)
table(knn.pred, test_cl)

############################ ALL ##############################################################

#Do one without Daily Values
mcd_menu <- mcd[,!grepl("Daily.Value",colnames(mcd))]
mcd_menu <- mcd_menu[4:14]

#Do one with Daily Value
mcd_menu2 <- mcd[,c(4,7,9,10,12,14,16,18,19,20,21,22,23,24)]
colnames(mcd_menu2) <- c("Calories", "Total.Fat", "Saturated.Fat", "Trans.Fat", "Cholesterol", "Sodium", "Carboyhydrates", "Dietary.Fiber", "Sugars", "Protein", "Vitamin.A", "Vitamin.C", "Calcium", "Iron")

#boxplot
boxplot(scale(mcd[,4:24]), main="Nutritional Value (All) Mcdonalds Data Set")
boxplot(scale(mcd_menu), main="Nutritional Total Value of Mcdonald Data Set")
boxplot(scale(mcd_menu2), main="Nutritional Daily Value of Mcdonald Data Set")


#should I scale it?
mcd_menu <- scale(mcd_menu, center = FALSE)

#optimal k for all
bestK <- NbClust(scale(mcd[4:24]), min.nc=2, max.nc=5,index = "kl", method="kmeans")
bestK$Best.nc


#optimal k (no daily value)
bestK <- NbClust(scale(mcd_menu), min.nc=2, max.nc=5,index = "kl", method="kmeans")
bestK$Best.nc

#optimal k (daily value)
bestK <- NbClust(mcd_menu2, min.nc=2, max.nc=5,index = "kl", method="kmeans")
bestK$Best.nc


#running mclust to see how many components
mc <- Mclust(mcd[,4:24])
plot(mc, mcd[,4:24], what = "BIC", col = "black")

#running mclust to see how many components
mc2 <- Mclust(mcd_menu)
plot(mc2, mcd_menu, what = "BIC", col = "black")

#running mclust to see how many components
mc3 <- Mclust(mcd_menu2)
plot(mc3, mcd_menu2, what = "BIC", col = "black")

#kmeans 
set.seed(27)
mcd.kmeans <- kmeans(mcd[,4:24],2)
cluster <- mcd.kmeans$cluster
table(cluster)
xtabs(~cluster+Category, data=mcd)

#kmeans 
set.seed(27)
mcd.kmeans <- kmeans(mcd_menu,3)
cluster <- mcd.kmeans$cluster
table(cluster)
xtabs(~cluster+Category, data=mcd)

#kmeans 
set.seed(27)
mcd.kmeans <- kmeans(mcd_menu2,4)
cluster <- mcd.kmeans$cluster
table(cluster)
xtabs(~cluster+Category, data=mcd)

#PCA MCD
pca <- prcomp(mcd[4:24], scale=TRUE)
summary(pca)
plot(pca$sdev^2, xlab = "Component Number", ylab = "Component Variance", type="l", main="Covariance Scree diagram")

#PCA MCD_Menu
pca2 <- prcomp(mcd_menu, scale=TRUE)
summary(pca2)
plot(pca2$sdev^2, xlab = "Component Number", ylab = "Component Variance", type="l", main="Covariance Scree diagram")

#PCA MCD_Menu2
pca3 <- prcomp(mcd_menu2, scale=TRUE)
summary(pca3)
plot(pca3$sdev^2, xlab = "Component Number", ylab = "Component Variance", type="l", main="Covariance Scree diagram")

######################################### FOOD #################################################

#Do one without Daily Values
mcd_menu <- mcd_food[,!grepl("Daily.Value",colnames(mcd_food))]
mcd_menu <- mcd_menu[4:14]

#Do one with Daily Value
mcd_menu2 <- mcd[,c(4,7,9,10,12,14,16,18,19,20,21,22,23,24)]
colnames(mcd_menu2) <- c("Calories", "Total.Fat", "Saturated.Fat", "Trans.Fat", "Cholesterol", "Sodium", "Carboyhydrates", "Dietary.Fiber", "Sugars", "Protein", "Vitamin.A", "Vitamin.C", "Calcium", "Iron")

#optimal k for all
bestK <- NbClust(mcd_food[4:24], min.nc=2, max.nc=5,index = "kl", method="kmeans")
bestK$Best.nc

#optimal k (no daily value)
bestK <- NbClust(mcd_menu, min.nc=2, max.nc=5,index = "kl", method="kmeans")
bestK$Best.nc

#optimal k (daily value)
bestK <- NbClust(mcd_menu2, min.nc=2, max.nc=5,index = "kl", method="kmeans")
bestK$Best.nc


#running mclust to see how many components
mc <- Mclust(mcd_food[,4:24])
plot(mc, mcd_food[,4:24], what = "BIC", col = "black")
table(mc$classification, mcd$Category)

#running mclust to see how many components
mc2 <- Mclust(mcd_menu)
plot(mc2, mcd_menu, what = "BIC", col = "black")

#running mclust to see how many components
mc3 <- Mclust(mcd_menu2)
plot(mc3, mcd_menu2, what = "BIC", col = "black")

#kmeans_food
set.seed(27)
mcd.kmeans <- kmeans(mcd_food[,-1],4)
cluster <- mcd.kmeans$cluster
table(cluster)
xtabs(~cluster+Category, data=mcd_food)

#exploring clusters
mcd.kmeans

#kmeans 
set.seed(27)
mcd.kmeans <- kmeans(mcd_menu,4)
cluster <- mcd.kmeans$cluster
table(cluster)
xtabs(~cluster+Category, data=mcd_food)

#kmeans 
set.seed(27)
mcd.kmeans <- kmeans(mcd_menu2,5)
cluster <- mcd.kmeans$cluster
table(cluster)
xtabs(~cluster+Category, data=mcd_food)

#exploring clusters
mcd.kmeans$centers

#PCA MCD
pca <- prcomp(mcd_food[4:24], scale=TRUE)
summary(pca)
plot(pca$sdev^2, xlab = "Component Number", ylab = "Component Variance", type="l", main="Covariance Scree diagram")

#explore pca
pca$rotation
pca$center

#PCA MCD_Menu
pca2 <- prcomp(mcd_menu, scale=TRUE)
summary(pca2)
plot(pca2$sdev^2, xlab = "Component Number", ylab = "Component Variance", type="l", main="Covariance Scree diagram")

#PCA MCD_Menu2
pca3 <- prcomp(mcd_menu2, scale=TRUE)
summary(pca3)
plot(pca3$sdev^2, xlab = "Component Number", ylab = "Component Variance", type="l", main="Covariance Scree diagram")

######################################### Drink #################################################

#Do one without Daily Values
mcd_menu <- mcd_drink[,!grepl("Daily.Value",colnames(mcd_drink))]
mcd_menu <- mcd_menu[4:14]

#Do one with Daily Value
mcd_menu2 <- mcd_drink[,c(4,7,9,12,14,16,18,21,22,23,24)]
colnames(mcd_menu2) <- c("Calories", "Total.Fat", "Saturated.Fat", "Cholesterol", "Sodium", "Carboyhydrates", "Dietary.Fiber", "Vitamin.A", "Vitamin.C", "Calcium", "Iron")

#optimal k for all
bestK <- NbClust(mcd_drink[4:24], min.nc=2, max.nc=5,index = "kl", method="kmeans")
bestK$Best.nc

#optimal k (no daily value)
bestK <- NbClust(mcd_menu, min.nc=2, max.nc=5,index = "kl", method="kmeans")
bestK$Best.nc

#optimal k (daily value)
bestK <- NbClust(mcd_menu2, min.nc=2, max.nc=5,index = "kl", method="kmeans")
bestK$Best.nc


#running mclust to see how many components
mc <- Mclust(mcd_drink[,4:24])
plot(mc, mcd_drink[,4:24], what = "BIC", col = "black")
table(mc$classification, mcd$Category)

#running mclust to see how many components
mc2 <- Mclust(mcd_menu)
plot(mc2, mcd_menu, what = "BIC", col = "black")

#running mclust to see how many components
mc3 <- Mclust(mcd_menu2)
plot(mc3, mcd_menu2, what = "BIC", col = "black")

#kmeans 
set.seed(27)
mcd.kmeans <- kmeans(mcd_drink[,4:24],2)
cluster <- mcd.kmeans$cluster
table(cluster)
xtabs(~cluster+Category, data=mcd_drink)

#cluster explore
mcd.kmeans

#kmeans 
set.seed(27)
mcd.kmeans <- kmeans(mcd_menu,4)
cluster <- mcd.kmeans$cluster
table(cluster)
xtabs(~cluster+Category, data=mcd_drink)

#cluster explore
mcd.kmeans

#kmeans 
set.seed(27)
mcd.kmeans <- kmeans(mcd_menu2,2)
cluster <- mcd.kmeans$cluster
table(cluster)
xtabs(~cluster+Category, data=mcd_drink)

#cluster explore
mcd.kmeans

#PCA MCD
pca <- prcomp(mcd_drink[4:24], scale=TRUE)
summary(pca)
plot(pca$sdev^2, xlab = "Component Number", ylab = "Component Variance", type="l", main="Covariance Scree diagram")

#pca explore
pca$rotation
pca$center

#PCA MCD_Menu
pca2 <- prcomp(mcd_menu, scale=TRUE)
summary(pca2)
plot(pca2$sdev^2, xlab = "Component Number", ylab = "Component Variance", type="l", main="Covariance Scree diagram")

#PCA MCD_Menu2
pca3 <- prcomp(mcd_menu2, scale=TRUE)
summary(pca3)
plot(pca3$sdev^2, xlab = "Component Number", ylab = "Component Variance", type="l", main="Covariance Scree diagram")