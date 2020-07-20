# Loading the data

shopper <- read.csv("http://bit.ly/EcommerceCustomersDataset", sep = ",", quote = "/")
head(shopper)

## Checking the data

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


## Exploratory Data Analysis

# Univariate Analysis

# Checking the summary for the central tendencies of the variables
summary(shopper)

# we can see that most variables have alot of imbalance

# Boxplots & Histograms

# previewing the numerical variables' histograms and boxplots
par(mfrow=c(2,2))
for(i in 1:10) {
  hist(shopper[, i], main=names(shopper)[i], xlab = NULL)
  boxplot(shopper[,i], main=names(shopper)[i], horizontal = TRUE)}
# we can see that all the variables have outliers

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
par(mfrow=c(1,1))
barplot(mnth)

# The most popular month is May and the least popular is Feb. This may be because the countries invloved
# are starting the summer season and their outfits are for the season. November is also quite popular and
# the assumption is that people are shopping for the holidays.During this season there are many offers


# operating system
par(mfrow=c(1,1))
barplot(os)

# The second Operating system is preffered to the others

# browser
par(mfrow=c(1,1))
barplot(bwr)
# Browser number 2 is commonly used by the customers. My money is on "Chrome"

# region
par(mfrow=c(1,1))
barplot(reg)
# Region No. 1 seems to be quite popular

# traffic type
par(mfrow=c(1,1))
barplot(traf)

# visitor type
par(mfrow=c(1,1))
barplot(visitor)

# The site seems to have more regular customers than new visitors. They have done a good job retaining their regulars
# however they may need to advertise more so as to get customers.

# revenue
par(mfrow=c(1,1))
barplot(rev)
# The revenue column is not fairly balanced

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
# The False is much more than the true

# plotting the distribution of Revenue per Visitor Type
par(mfrow=c(1,1))
rev_visitor <- table(shopper$Revenue, shopper$VisitorType)
barplot(rev_visitor, main = "Revenue per Visitor Type", col = c("pink", "cyan"), beside = TRUE,
        legend = rownames(rev_visitor), xlab = "Visitor Type")
# The False is much more than the true

## Implementing the solution

sup_shopper <- shopper[, c(1,3,5,7,9,10,11,12,13,14,15,16,17)]
sup_shopper.class <- shopper[, 18]
head(sup_shopper)
head(sup_shopper.class)


# convert the factors into numerics
sup_shopper$Month <- as.numeric(sup_shopper$Month)
sup_shopper$OperatingSystems <- as.numeric(sup_shopper$OperatingSystems)
sup_shopper$Browser <- as.numeric(sup_shopper$Browser)
sup_shopper$Region <- as.numeric(sup_shopper$Region)
sup_shopper$TrafficType <- as.numeric(sup_shopper$TrafficType)
sup_shopper$VisitorType <- as.numeric(sup_shopper$VisitorType)
sup_shopper$Weekend <- as.numeric(sup_shopper$Weekend)
str(sup_shopper)

# Scaling the data using MinMax scaler
normal <- function(x) (
  return( ((x - min(x)) /(max(x)-min(x))) )
)
normal(1:6)
Shopper_new <- as.data.frame(lapply(sup_shopper, normal))
summary(Shopper_new)

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
