## Kaggle San Francisco Crime Prediction

## The objective of this Kaggle Knowledge competition is to predict the
## Category of crime from the other data.

## We are encouraged to explore the data visually, such as on a map

## Init
library(dplyr)
library(ggplot2)

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

## Crimes can be categorised
