# Loading the data

shopper <- read.csv("http://bit.ly/EcommerceCustomersDataset", sep = ",", quote = "/")
head(shopper)

# Checking the data

# checking the number of columns and rows

dim(shopper)

# we can see we have 12330 rows and 18 columns

# Checking or the type of data that we have
str(shopper)

# we have both numerical and categorical data

# check the summary of our data by using the summary() function

summary(shopper)


# Data Cleaning
# Checking or missing values

colSums(is.na(shopper))

# removing the missing values
shopper <- na.omit(shopper)
colSums(is.na(shopper))

# Checking for duplicates

anyDuplicated(shopper)
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
# Dropping duplicates
shopper <- unique(shopper)

# Checking whether the duplicates have been successfully dropped
anyDuplicated(shopper)


# The Operating Systems, Browser, Region, and Traffic Type variables are clearly categorical
# in nature and not numerical, it's just that they have already been encoded to make them easier
# to work with. The same goes for the Weekend and Revenue columns. We will convert all of them to
# their appropriate data type.

# changing categorical columns to factors

shopper$OperatingSystems <- as.factor(shopper$OperatingSystems)
shopper$Browser <- as.factor(shopper$Browser)
shopper$Region <- as.factor(shopper$Region)
shopper$TrafficType <- as.factor(shopper$TrafficType)
shopper$Weekend <- as.factor(shopper$Weekend)
shopper$Revenue <- as.factor(shopper$Revenue)

# confirming the data types have been changed
str(shopper)

## Exploratory Data Analysis

# Univariate Analysis

# Checking the summary for the variables
summary(shopper)

# Boxplots & Histograms

# previewing the numerical variables' histograms and boxplots
par(mfrow=c(2,2))
for(i in 1:10) {
  hist(shopper[, i], main=names(shopper)[i], xlab = NULL)
  boxplot(shopper[,i], main=names(shopper)[i], horizontal = TRUE)}

## Bar Graphs

# create tables of all categorical variables to be able to create bar plots with them
mnth <- table(shopper$Month)
os <- table(shopper$OperatingSystems)
bwr <- table(shopper$Browser)
reg <- table(shopper$Region)
traf <- table(shopper$TrafficType)
visitor <- table(shopper$VisitorType)
wknd <- table(shopper$Weekend)
rev <- table(shopper$Revenue)

# month
barplot(mnth)

# operating system
barplot(os)

# browser
barplot(bwr)

# region
barplot(reg)

# traffic type
barplot(traf)

# visitor type
barplot(visitor)

# revenue
barplot(rev)

## Bivariate Analysis

# using a heat map to visualize variable correlations
library(ggcorrplot)
shopper_num <- shopper[,1:10]
corr_df <- cor(shopper_num)
ggcorrplot(round(corr_df, 2) ,lab = T,type = 'lower')

# As I suspected, BounceRates is very highly correlated to ExitRates, Administrative
# is correlated with Administrative_Duration, Informational is highly correlated to
# Informational_Duration, and ProductRelated is highly correlated with ProductRated_Duration.
# Therefore, we will have to drop one variable of each of the highly correlated pairs to reduce
# dimensionality and redundancy.


# plotting the distribution of Revenue per Month
par(mfrow=c(1,1))
rev_mnth <- table(shopper$Revenue, shopper$Month)
barplot(rev_mnth, main = "Revenue per Month", col = c("pink", "cyan"), beside = TRUE,
        legend = rownames(rev_mnth), xlab = "Month")


# plotting the distribution of Revenue per Visitor Type
par(mfrow=c(1,1))
rev_visitor <- table(shopper$Revenue, shopper$VisitorType)
barplot(rev_visitor, main = "Revenue per Visitor Type", col = c("pink", "cyan"), beside = TRUE,
        legend = rownames(rev_visitor), xlab = "Visitor Type")

## Implementing the solution

sup_shopper <- shopper[, -14]
sup_shopper.class <- shopper[, "Revenue"]
head(sup_shopper)

# convert the factors into numerics
sup_shopper$Month <- as.numeric(sup_shopper$Month)
sup_shopper$OperatingSystems <- as.numeric(sup_shopper$OperatingSystems)
sup_shopper$Browser <- as.numeric(sup_shopper$Browser)
sup_shopper$Region <- as.numeric(sup_shopper$Region)
sup_shopper$TrafficType <- as.numeric(sup_shopper$TrafficType)
sup_shopper$VisitorType <- as.numeric(sup_shopper$VisitorType)
sup_shopper$Weekend <- as.numeric(sup_shopper$Weekend)
str(sup_shopper)

library("dplyr")

rescale_df <- scale(sup_shopper)
# K Means
# applying k-means with k = 3
k_result <- kmeans(rescale_df, 3)

# previewing the number of records in each cluster
k_result$size

# visualizing the clusters
set_plot_dimensions(6, 6)

# plotting Administrative vs Informational
plot(rescale_df[,1:2], col = sup_shopper.class)

set_plot_dimensions(6, 6)

# plotting Special Day vs Month
plot(rescale_df[,6:7], col = k_result$cluster)

# showing how the clusters respond to the classes
table(k_result$cluster, sup_shopper.class)

## Heirachial Clustering

# first we compute the euclidean distance
df <- dist(rescale_df, method = "euclidean")

# then we compute hierarchical clustering using the Ward method
hier <- hclust(df, method = "ward.D2" )

# finally, we plot the dendogram
plot(hier, cex = 0.6, hang = -1)
