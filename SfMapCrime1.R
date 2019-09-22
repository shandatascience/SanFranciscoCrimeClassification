###################################################################################################
#San Francisco crime classification
###################################################################################################

# 1. Define libraries

library(dplyr)
library(reshape2)
library(ggplot2)
library(readr)
library(lubridate)
library(MASS)
library (rpart)
library(caret)

library(plyr)
library(dplyr)

library(ggplot2)
library(readr)
library (e1071)
library(sqldf)
library(caret)
library(klaR) 

# 2. Read input and defining variables

train <- read_csv("Documents/R_Studio/train.csv", stringsAsFactors = F) 
View(train)
test <- read_csv("Documents/R_Studio/test.csv", stringsAsFactors = F)
View(test)

#Convering Date from factor to POSIXlt format & deriving Year, Month, Hour data
train$Dates<-strptime(train$Dates, "%Y-%m-%d %H:%M:%OS")
train$Year<-train$Dates$year+1900
train$Month<-train$Dates$mon+1
train$Hour<-format(train$Dates, format="%H") 

test$Dates<-strptime(test$Dates, "%Y-%m-%d %H:%M:%OS")
test$Year<-test$Dates$year+1900
test$Month<-test$Dates$mon+1
test$Hour<-format(test$Dates, format="%H") 

plot_theme<-theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plot_text<-geom_text(stat='count',aes(label=..count..),vjust=-1)

# 3. Exploratory data analysis

# Top crimes in SF - Larcenecy/Theft, Other Offences, Assault & Non Criminal
ggplot(train,aes(x=Category)) + geom_bar(fill = "blue") + plot_theme + xlab("Crime Category") + ylab("Number of Crimes")

# Crime in SF by day of week - Friday(Most) & sunday(least)
ggplot(train,aes(x=DayOfWeek)) + geom_bar(fill = "green") + plot_text + xlab("Day of Week") + ylab("Number of Crimes")

# Crime in SF by locality - Southern district (Most) & Richmond(least)
ggplot(train,aes(x=PdDistrict)) + geom_bar(fill = "yellow") + plot_text + xlab("District") + ylab("Number of Crimes")

# Crime resolutions in SF are None (Most), Arrest booked , Arrest cited
ggplot(train,aes(x=Resolution)) + geom_bar() + plot_theme + plot_text + xlab("Resolution") + ylab("Count")


#Analyse relationship between Category & other variables.

summary(train$Year)

# Plot to find out relative frequency by month across crime category :  most occured Treason - months(9,10,11,2)
ggplot(train,aes(x=Category,fill=as.factor(Month))) + geom_bar(position = position_fill()) + plot_theme +
  xlab("Crime Category") + ylab("Relative Frequency by Month") + labs(fill = "Months")

table(train$Month)

# Plot to find out relative frequency by year across crime category of crimes :  least occured Treason (2010-2015)

ggplot(train,aes(x=Category,fill=as.factor(Year))) + geom_bar(position = position_fill()) + plot_theme +
  xlab("Crime Category") + ylab("Relative Frequency by Year") + labs(fill = "Years")
#Treason data is only from past 5 years

# Plot to find out relative frequency by hours across crime category of crimes :  
#Incidents like 'Diving Under Influence' & 'Prostitution' - night crimes
# while 'Bad Checks' & 'Fraud' -day crimes

ggplot(train,aes(x=Category,fill=as.factor(Hour))) + geom_bar(position = position_fill()) + plot_theme +
  xlab("Crime Category") + ylab("Relative Frequency by Hour") + labs(fill = "Hours")

# Plot to find out relative frequency by day of week across crime category of crimes :  
#Treason occurs on saturday mostly

ggplot(train,aes(x=Category,fill=DayOfWeek)) + geom_bar(position = position_fill()) + plot_theme +
  xlab("Crime Category") + ylab("Relative Frequency by WeekDay") + labs(fill = "Week Day")
 
# Plot to find out crime frequency by day of week across crime category of crimes :  
#Theft occurs on Friday and saturday mostly


ggplot(train,aes(x=Category,fill=DayOfWeek)) + geom_bar(position = position_stack()) + plot_theme +
  xlab("Crime Category") + ylab("Crime Frequency") + labs(fill = "Week Day")

# Plot to find out crime frequency by day of week across crime category of crimes :  
#Theft occurs on Friday and saturday mostly

# Plot to find out crime frequency by area across crime category of crimes :  


ggplot(train,aes(x=Category,fill=PdDistrict)) + geom_bar(position = position_fill()) + plot_theme +
  xlab("Crime Category") + ylab("Relative Frequency by District") + labs(fill = "District")

#plot to find resolution relative frequency by crime cateogry:
#Top - Driving,Drunk & Drug cases resolution was Arrest Booked
# Resolution = None for Theft including Vehicle Theft resolution

ggplot(train,aes(x=Category,fill=Resolution)) + geom_bar(position = position_fill()) + plot_theme +
  xlab("Crime Category") + ylab("Relative Frequency by Resolution")

###################################################################################################
#Naive Bayes Model
###################################################################################################

# 1. Define libraries

library(data.table)
library(e1071)



# 2. Read input and defining variables

train = data.table(read.csv('Documents/R_Studio/train.csv',
                            header = T))
test = data.table(read.csv('Documents/R_Studio/test.csv', 
                           header = T))

# changing column names
setnames(train, names(train), c('date', 'category_predict', 'description_ignore', 'day_of_week', 'pd_district', 'resolution', 'address', 'x', 'y'))
setnames(test, names(test), c('id', 'date', 'day_of_week', 'pd_district', 'address', 'x', 'y'))

# define hour
train$hour = as.numeric(substr(train$date, 12, 13))
test$hour = as.numeric(substr(test$date, 12, 13))

# select a subset for training purpose which contains alteast one type of crime inorder to train
# 'subset' contains at least 1 example of every crime category.
subset = train[c(1:30000, 30024, 33954, 37298, 41097, 41980, 44479, 48707, 48837, 49306, 53715, 93717, 102637, 102645, 102678, 102919, 103517, 103712, 107734, 148476, 148476, 
                 192191, 205046, 252094, 279792, 316491, 317527, 332821, 337881)]

# 3. Model creation and predictions for training set 
set.seed(1045)
model = category_predict ~ day_of_week + pd_district + x + y + hour
nb = naiveBayes(model, 
                data = subset)

# 4. Generate predictions for training and test data.
# For the training data, compute accuracy. 

subset$pred = data.table(predict(nb, 
                                 newdata = subset, 
                                 type = 'class'))

# For the test data,  format predictions

test_pred = data.table(predict(nb, 
                               newdata = test, 
                               type = 'raw'))

# 5. validate training set accuracy

print('Training Accuracy:')
table(subset$category_predict == subset$pred)
prop.table(table(subset$category_predict == subset$pred))

# 6. cross validation 

tune_control = tune.control(cross = 2)

# Execute cross validation.
t = tune(method = 'naiveBayes', 
         model, 
         data = subset, 
         size = 2, 
         tunecontrol = tune_control)

# View estimated test set error computed by cross validation.
print('Cross Validation Error: ')
print(t)

#7. Export test set predictions
 
write.csv(test_pred, 'test_pred_naive_bayes_benchmark.csv', row.names = F)


