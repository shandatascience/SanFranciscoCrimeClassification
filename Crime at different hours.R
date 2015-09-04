## Distribution of crime at different hours of the day

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)


# Load Data ---------------------------------------------------------------

train <- read_csv("../input/train.csv.zip")


# Clean Data --------------------------------------------------------------

## Dates needs to be converted from a Factor variable to POSIXct format
## Using lubridate
train$Dates <- ymd_hms(train$Dates)

## Different crimes, and the amount of crime, can vary over the day.
## Time may be a useful factor, and can be engineered from the Dates variable
train$Hour <- hour(train$Dates)
train$Minute <- minute(train$Dates)


# Graphing ----------------------------------------------------------------
## Function to graph Crime Category against Hours

category_time = function(x) {
    df <- filter(train, Category == x)
    dat <- aggregate(df$Category, list(Hour = df$Hour), FUN = length)
    g <- ggplot(dat, aes(x = Hour, y = x))
    g <- g + geom_line()
    g <- g + labs(title = x, x = "Hour", y = "Amount of Crime")
    g
}

category_time("ASSAULT")
category_time("VEHICLE THEFT")
category_time("ROBBERY")
category_time("PROSTITUTION")
category_time("ARSON")
category_time("FAMILY OFFENSES")
