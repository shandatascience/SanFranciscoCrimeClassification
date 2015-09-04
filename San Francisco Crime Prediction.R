## Kaggle San Francisco Crime Prediction

## The objective of this Kaggle Knowledge competition is to predict the
## Category of crime from the other data.

## We are encouraged to explore the data visually, such as on a map

## R version 3.2.1 (2015-06-18)
## Platform: x86_64-w64-mingw32/x64 (64-bit)

# Init --------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(caret)
## Used for manipulating times and dates
library(lubridate)
library(RevoUtilsMath)

# Load data ---------------------------------------------------------------

fileUrl1 <- "https://www.kaggle.com/c/sf-crime/download/test.csv.zip"
download.file(fileUrl1, temp)
train <- read.csv(unz(temp, "train.csv"), header = TRUE, sep = ",")

fileUrl2 <- "https://www.kaggle.com/c/sf-crime/download/train.csv.zip"
download.file(fileUrl2, temp)
test <- read.csv(unz(temp, "test.csv"), header = TRUE, sep = ",")

## Alternatively, if the file is in the folder
train <- read.csv("train.csv", header = TRUE, sep = ",")
test <- read.csv("test.csv", header = TRUE, sep = ",")

# Clean data --------------------------------------------------------------

## Dates needs to be converted from a Factor variable to POSIXct format
## Using lubridate
train$Dates <- ymd_hms(train$Dates)
train$Dates <- as.POSIXct(round(train$Dates, units = "days"))

## Descriptions and Addresses are not Factors, so convert to Character
train$Descript <- as.character(train$Descript)
train$Address <- as.character(train$Address)

# Cross Validation --------------------------------------------------------

set.seed(2468)

## Split the train set into subsets for cross validation of prediction model
inTrain <- createDataPartition(y = train$Category, p = 0.6, list = FALSE)

## The training set is primarily used for training a prediction model
## The training set is also easier for exploring, and quicker to test models
training <- train[inTrain, ]

## The testing set will be used for estimating the out-of-sample error, which
## will be quicker to do than submitting endless amounts of files to Kaggle!
testing <- train[-inTrain, ]

rm(train)
rm(inTrain)

# Explore the data --------------------------------------------------------

## Note that the test dataset does not have the following variables
## - Category (the outcome variable)
## - Descript
## - Resolution
## Therefore, these should not be used in any predictive modelling,
## but they may be used to explore the training data set.


str(training)

summary(training)

table(training$DayOfWeek)
plot(training$DayOfWeek)

plot(training$Category)

## Do different districts have different crime profiles?
## Plot the Crimes against District
plot(table(training$PdDistrict, training$Category))
# This shows that in different districts, the proportion of different crimes will vary
table(training$Category, training$PdDistrict)

## Are there different crimes on different days
## What are the top crimes on Mondays?
monday <- filter(training, DayOfWeek == "Monday")
df <- as.data.frame(table(monday$Category))
arrange(df, desc(Freq))

## Function to find the top 10 crime categories by day
crimeByDay <- function(df, day) {
    day <- filter(training, DayOfWeek == day)
    table <- as.data.frame(table(day$Category))
    head(arrange(table, desc(Freq)))
}

## Do different crimes occur at different times during a day?


## What are Bad Checks?
bc <- filter(training[, -1], Category == "BAD CHECKS")
as.data.frame(table(bc$Descript))
## Cheques... Not everyone is American!

## Types of ASSAULT?
assault <- filter(training[, -1], Category == "ASSAULT")
as.data.frame(table(assault$Descript))




## Are there multiple crimes are addresses?
dim(training)[1]
length(unique(training$Address))
dim(training)[1] / length(unique(training$Address))
# There are 24 times more crimes than there are unique addresses.
# Several (maybe types of) crimes must be occuring at the same address.
# However, if this was used as a factor variable, there would be too many levels
# to make this an effficient, if useful, predictor variable.

# Feature engineering -----------------------------------------------------

## The Address field gives clue as to whether a crime occurred on a street
## corner, based on the existence of "/".
## Function to identify whether address is a corner or not
corner <- function(x) {
    if (grepl("/", x) == TRUE)
        return("Corner")
    else
        return("Block")
}

training$Corner <- sapply(training$Address, corner)
training$Corner <- as.factor(training$Corner)

## Crimes can be categorised


# Modelling ---------------------------------------------------------------
## For reproducability...
set.seed(13579)

## Note that the test dataset does not have the following variables
## - Category (the outcome variable)
## - Descript
## - Resolution
## Therefore, these should not be used in any predictive modelling


## Try a simple decition tree (rpart) on "Corner" and "DayOfWeek"
system.time(
    modelFit <- train(Category ~ DayOfWeek + Corner + PdDistrict, method = "rpart", data = training)
)
## Accuracy is only 0.225 on the training data set!


## Prediction model using random forests (rf)
## This takes a very long time, and requires a lot of RAM, depending
## on the number of trees to grow
system.time(
    modelFit <- train(Category ~ DayOfWeek + PdDistrict + Corner, method = "rf", ntree = 50, data = training)
)

# Cross-validation --------------------------------------------------------

## Run the same data preprocessing performed on the training data set on the
## testing data set.

testing$Corner <- sapply(testing$Address, corner)
prediction <- predict(modelFit, newdata = testing)
cm <- confusionMatrix(prediction, testing$Category)
cm$overall
## Out-of-sample error is estimated to be 0.226 based on the testing dataset
## consistent with the training dataset.