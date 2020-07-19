
# Univariate, Bivariate Analysis and Modelling of the Advertisement Dataset

##Introduction

### Defining the question
A Kenyan entrepreneur has created an online cryptography course and would want to advertise it on her blog. She currently targets audiences originating from various countries. In the past, she ran ads to advertise a related course on the same blog and collected data in the process. She would now like to employ your services as a Data Science Consultant to help her identify which individuals are most likely to click on her ads. 

### Metric of success
EDA
Univariate Analysis
Bivariate Analysis

### Experimental design

1.Loading Data

2.Data Exploration/Cleaning

3.Data Cleaning

4.Univariate Analysis

5.Bivariate Analysis

6.Conclusion

## Reading the data

```R

# Loading and Previewing the advertisement dataset  
# then displaying the first 6 records of this database

advertising <- read.csv("advertising.csv", header = TRUE)

head(advertising)

```

## Checking the Data

```
# checking the column names

attributes(advertising)$names

```

```
# checking what class our dataset is in

attributes(advertising)$class
```

```
# checking the types of data

str(advertising)

# checking the shape of the data
dim(advertising)
```

## Data Cleaning 

### Missing values
```
# Lets Identify missing data in our dataset 
# by using the function is.na() 

is.na(advertising)

```

```
# We can also find out total missing values in each column
# by using the function colSums()

colSums(is.na(advertising))
```

there are no null values

### Duplicated values

```
# Checking for any duplicates

anyDuplicated(advertising)

```
There are no duplicates in this dataset

### Outliers
```
# checking for outliers by using the boxplots to visualize

boxplot(advertising)
```
we can see that area.income column has a number of outliers below 20,000


```
# we then use the function boxplot.stats which lists the outliers in the vectors

boxplot.stats(advertising$Area.Income)$out
```

we will not drop the outliers for now

## Univariate Analysis

### Boxplots for numerical columns

```
# bOxplot for the Daily time spent column

boxplot(advertising$Daily.Time.Spent.on.Site)

```

```
# Age boxplot
boxplot(advertising$Age)

```

```
# Area Income
boxplot(advertising$Area.Income)

```
This column has a number o outliers and it would be advisable to remove them so as to achieve better results
```
# Daily internet usage
boxplot(advertising$Daily.Internet.Usage)

```


### Summary of numeric columns
```
# install the MASS library
library(MASS)

# creating a function for mode since it is not in built

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
```
Finding the central tendencies of the various columns

#### Age
```
mean(advertising$Age)
median(advertising$Age)
mode(advertising$Age)
sd(advertising$Age)
var(advertising$Age)
range(advertising$Age)
quantile(advertising$Age)
```

#### daily time spent on site
``` 
mean(advertising$Daily.Time.Spent.on.Site)
median(advertising$Daily.Time.Spent.on.Site)
mode(advertising$Daily.Time.Spent.on.Site)
sd(advertising$Daily.Time.Spent.on.Site)
var(advertising$Daily.Time.Spent.on.Site)
range(advertising$Daily.Time.Spent.on.Site)
quantile(advertising$Daily.Time.Spent.on.Site)
```

#### Area income
```
mean(advertising$Area.Income)
median(advertising$Area.Income)
mode(advertising$Area.Income)
sd(advertising$Area.Income)
var(advertising$Area.Income)
range(advertising$Area.Income)
quantile(advertising$Area.Income)
```

#### daily internet usage
```
mean(advertising$Daily.Internet.Usage)
median(advertising$Daily.Internet.Usage)
mode(advertising$Daily.Internet.Usage)
sd(advertising$Daily.Internet.Usage)
var(advertising$Daily.Internet.Usage)
range(advertising$Daily.Internet.Usage)
quantile(advertising$Daily.Internet.Usage)
```

#### Male
```
mean(advertising$Male)
median(advertising$Male)
mode(advertising$Male)
sd(advertising$Male)
var(advertising$Male)
range(advertising$Male)
quantile(advertising$Male)
```


#### clicked on ad
```
mean(advertising$Clicked.on.Ad)
median(advertising$Clicked.on.Ad)
mode(advertising$Clicked.on.Ad)
sd(advertising$Clicked.on.Ad)
var(advertising$Clicked.on.Ad)
range(advertising$Clicked.on.Ad)
quantile(advertising$Clicked.on.Ad)
```

### Graphical Analysis
#### Bar graphs
```
# Plotting barplots of the Gender column

num <- table(advertising$Male)
num
barplot(num,main="Gender Distribution",xlab="Gender",ylab = "Frequency", col="blue")
```
The gender column is almost fairly balanced
```
# Plotting barplots of the Clicked on Ad column

Ad <- table(advertising$Clicked.on.Ad)
Ad
barplot(Ad,main="Distribution of Ads clicked on",xlab="Clicked",ylab = "Frequency", col="red")
```
This column is fairly balanced

#### histograms

```
# plotting a histogram of time spent on the site

hist(advertising$Daily.Time.Spent.on.Site)
```
If we assume time spent is in minutes,we have the highest at 80 min which is roughly 1 hr an half
```
#Plotting a histogram of Internet usage

hist(advertising$Daily.Internet.Usage)
```
We can see that most people use between a range of 100 to 250MBs

## Bivariate Analysis

### Correlation
```
# Getting the correlation between the numeric variables
# assigning columns to variable names

time.spent.onsite <- advertising$Daily.Time.Spent.on.Site
age <- advertising$Age
income <- advertising$Area.Income
internet.usage <- advertising$Daily.Internet.Usage
gender <- advertising$Male
ads <- advertising$Clicked.on.Ad
```

Calculating their correlation
```
library(ggcorrplot)
ad_num <- advertising[1:4]
corr_df <- cor(ad_num)
ggcorrplot(round(corr_df, 2) ,lab = T,type = 'lower')

```

### Scatterplots
```
# Plotting a scatter plot between variables

plot(time.spent.onsite, ads,main = "Scatterplot", xlab="Time spent on site",
     ylab="clicks on ad")
```
There is a strong negative correlation. This means the more time you spend on site the less likely to click on the ads and vice versa.


```
plot(income, age, main="Scatterplot Age VS Daily Time spent",
     xlab="Age ", ylab="Daily Time Spent ", pch=19)
```
There is a weak negative correlation between these variables

## Implementing the solution

```
# Selecting the columns that will be used for modelling
#iris[, c(1, 2, 3, 4)]
advert <- advertising[,c(1, 2, 3, 4, 7)]
head(advert)
```
```
# Making the data to be random ,we shuffle the rows before splitting

index <- createDataPartition(advert_shuffled$Clicked.on.Ad,p=0.8,list=F)

test  <-  advert_shuffled[-index,]
train <- advert_shuffled[index,]

x_train = scale(train[,1:4])
x_test = scale(test[,1:4])
y_train = train[,5]
y_test = test[,5]
```
```
# This is alist of some of the available models

paste(names(getModelInfo())[1:50],collapse = ", ")
```

### KNN
```
#Using knn to to this classification problem

ypred <- knn(train = x_train, test = x_test,
             cl = y_train, k = 21)
```
```
# Evaluating perfomance using crosstable

CrossTable(x = y_test, y = ypred,
           prop.chisq = FALSE)
```
```
# Confusion matrix

table(y_test,ypred)
```
```
# The accuracy

print(mean(y_test == ypred ))
```

## Conclusion
Based on the analysis above, all of these variables play a vital role in whether
the advertisement will be clicked on or not. They should therefore pursue modelling 
to see which variables are more important than others and these can help in getting 
identifying areas to tweak so as to get more clicks on the ads and therefore more money.

