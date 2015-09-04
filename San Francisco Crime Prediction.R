## Kaggle San Francisco Crime Prediction

## The objective of this Kaggle Knowledge competition is to predict the
## Category of crime from the other data.

## We are encouraged to explore the data visually, such as on a map


# Init --------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(caret)

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

## Dates needs to be converted from a Factor variable to POSIXlt format
train$Dates <- as.character(train$Dates)
train$Dates <- strptime(train$Dates, "%Y-%m-%d %H:%M:%S")


## Descriptions and Addresses are not Factors, so convert to Character
train$Descript <- as.character(train$Descript)
train$Address <- as.character(train$Address)


# Cross Validation --------------------------------------------------------

## Split the train set into subsets for cross validation of prediction model
inTrain <- createDataPartition(y = train$Category, p = 0.6, list = FALSE)

## The training set is primarily used for training a prediction model
## The training set is also easier for exploring, and quicker to test models
training <- train[inTrain, ]

## The testing set will be used for estimating the out-of-sample error, which
## will be quicker to do than submitting endless amounts of files to Kaggle!
testing <- train[-inTrain, ]

# Explore the data --------------------------------------------------------

str(train)

summary(train)

unique(train$Category)

unique(train$PdDistrict)

unique(train$Resolution)

table(train$DayOfWeek)
plot(train$DayOfWeek)

plot(train$Category)

# What are the top crimes on Mondays?
table(train$Category[, train$DayOfWeek == "Monday"] )
temp <- filter(train, DayOfWeek == "Monday")

# Feature engineering -----------------------------------------------------

## The Address field gives clue as to whether a crime occurred on a street
## corner, based on the existence of "/".
## Function to identify whether address is a corner or not
corner <- function(x) {
    if (grepl("/", x) == TRUE)
        return("Corner")
    else
        return("Building")
}

training$Corner <- sapply(training$Address, corner)

## Crimes can be categorised


# Modelling ---------------------------------------------------------------

set.seed(13579)
modelFit <- train(Category ~ DayOfWeek + PdDistrict + Resolution + Corner, method = "rf", data = training)



# Cross-validation --------------------------------------------------------

## Run the same data preprocessing performed on the training data set on the
## testing data set.

testing$Corner <- sapply(training$Address, corner)

