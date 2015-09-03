## Kaggle San Francisco Crime Prediction



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


# Explore the data --------------------------------------------------------

str(train)



# Clean data --------------------------------------------------------------

## Dates needs to be converted from a Factor variable to a date variable
train$Dates <- as.character(train$Dates)
train$Dates <- strptime(train$Dates, "%Y-%m-%d %H:%M:%S")

## Descriptions and Addresses are not Factors, so convert to Character
train$Descript <- as.character(train$Descript)
train$Address <- as.character(train$Address)
