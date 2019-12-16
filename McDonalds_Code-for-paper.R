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
library(factoextra)

############################ Introduction ##########################
#data
str(mcd)
summary(mcd)
summary(mcd$Category)

############################ Data #################################

#using daily value instead of total
mcd_menu <- mcd[,c(1,4,7,9,10,12,14,16,18,19,20,21,22,23,24)]
colnames(mcd_menu) <- c("Category", "Calories", "Total.Fat", "Saturated.Fat", "Trans.Fat", "Cholesterol", "Sodium", "Carboyhydrates", "Dietary.Fiber", "Sugars", "Protein", "Vitamin.A", "Vitamin.C", "Calcium", "Iron")
str(mcd_menu)

#split data into food and drink
mcd_food <- mcd_menu %>%
  filter(Category == "Beef & Pork" | Category == "Breakfast" | Category == "Chicken & Fish" | Category == "Salads" | Category == "Snacks & Sides" | Category == "Desserts")
mcd_drink <- mcd_menu %>%
  filter(Category == "Beverages" | Category == "Coffee & Tea" | Category == "Smoothies & Shakes") %>%
  filter(Calories != 0)

#split mcd_food and mcd_drink to numeric variables
mcd_menu <- mcd_menu[,-1]

#qqplot for normality
par(mfrow=c(3,5))
for (i in 1:14) {
  qqnorm(mcd_menu[,i],main=names(mcd_menu)[i]) 
  qqline(mcd_menu[,i])
}

#McDonalds Calories
par(mfrow=c(1,1))
plot(mcd_menu$Calories, main="McDonald's Items by Calories", xlab = "Item", ylab="Calories")

#correlation plot of data
corr <- cor(mcd_menu)
corrplot(corr, method="shade")

############################ Analysis #################################

summary(mcd_food$Category)
summary(mcd_drink$Category)

#find the best k for mcd_food
bestK <- NbClust(scale(mcd_food[,-1]), min.nc=2, max.nc=5,index = "kl", method="kmeans")
bestK$Best.nc

bestK2 <- NbClust(scale(mcd_drink[,-1]), min.nc=2, max.nc=5,index = "kl", method="kmeans")
bestK2$Best.nc

#best k using mclust
mc1 <- Mclust(scale(mcd_food[,-1]))
plot(mc1, mcd_food[,-1], what = "BIC", col = "black")

mc2 <- Mclust(scale(mcd_drink[,-1]))
plot(mc2, mcd_drink[,-1], what = "BIC", col = "black")

#pca_food
pca_food <- prcomp(mcd_food[,-1], scale=TRUE)
summary(pca_food)

#pca_drink
pca_drink <- prcomp(mcd_drink[,-1], scale=TRUE)
summary(pca_drink)

############################ Tables & Graphs #################################

#kmeans_food
set.seed(27)
mcd.kmeans_food <- kmeans(mcd_food[,-1],2)
cluster1 <- mcd.kmeans_food$cluster
table(cluster1)
xtabs(~cluster1+Category, data=mcd_food)
mcd.kmeans_food

#kmeans_drink
set.seed(27)
mcd.kmeans_drink <- kmeans(mcd_drink[,-1],3)
cluster2 <- mcd.kmeans_drink$cluster
table(cluster2)
xtabs(~cluster2+Category, data=mcd_drink)
mcd.kmeans_drink

#McLust Food
table(mc1$classification, mcd_food$Category)
mc1$data

#McLust Drink
table(mc2$classification, mcd_drink$Category)
mc2$data

#pca food
plot(pca_food$sdev^2, xlab = "Component Number", ylab = "Component Variance", type="l", main="Covariance Scree diagram")
summary(pca_food)
fviz_pca_var(pca_food,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title = "Variables PCA Food"
)

#pca drink
plot(pca_drink$sdev^2, xlab = "Component Number", ylab = "Component Variance", type="l", main="Covariance Scree diagram")
summary(pca_drink)
fviz_pca_var(pca_drink,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title = "Variables PCA Drink"
)
