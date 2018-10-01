library(ggplot2)
test <- read.csv(file="C:/UNCC Courses/spring'18/KDD/Project/test.csv",stringsAsFactors = FALSE)
train <- read.csv(file="C:/UNCC Courses/spring'18/KDD/Project/train.csv",stringsAsFactors = FALSE)
store <- read.csv(file="C:/UNCC Courses/spring'18/KDD/Project/store.csv",stringsAsFactors = FALSE)
train
str(test)
str(train)
str(store)

summary(train)
summary(test)
summary(store)

#Data Pre-processing 

#No sales for stores that are closed
#OpenStores Contains only those stores that are open
summary(train$Sales[!train$Open])

length(train$Sales[!train$Open]) / nrow(train)
OpenStores <- train[train$Open,]
summary(OpenStores)
train <- train[train$Open,]
summary(OpenStores)

#Test dataset has fewer values
#Checking for unique values reduces the data to be preprocessed by approx 24%
teststores <- as.numeric(as.character(unique(test$Store)))
teststores[1:10]
nrow(train)
nrow(test)
train <- train[train$Store %in% teststores,]
nrow(train)

#We see that all NA values pertain to a single store (622), they are for consecutive days except the Sundays in this period, 
#and contain all other days (Mondays to Saturdays). Furthermore, there are four days at the end of the period when there is an 
#active promotion running in this store. Since the 
#intervening Sundays are explicitly marked as 0, we can assume that these are all open days. Therefore I set the NA values to true.
test[is.na(test$Open),]
test[is.na(test)] <- 1
sapply(test, function(x) length(unique(x)))
sapply(train, function(x) length(unique(x)))
sapply(store, function(x) length(unique(x)))

#EDA
summary(train$Sales)

sd(train$Sales)

hist(train$Sales,xlab="Sales")
#To determine outliers
boxplot(train$Sales)

summary(train[train$Sales > 20000,])
#This indicates sales above 20000 are not outliers

#check if there is sufficient historic data
openDays <- aggregate(train$Store,list(train$Store),length)
openDays
summary(openDays)


t.test(train[train$Promo,]$Sales,train[!train$Promo,]$Sales)
t.test(train[train$StateHoliday != 0,]$Sales,train[train$StateHoliday == 0,]$Sales)
t.test(train[train$StateHoliday != 0,]$Sales,train[train$StateHoliday == 0,]$Sales)
t.test(train[train$SchoolHoliday,]$Sales,train[!train$SchoolHoliday,]$Sales)

a<- tapply(train$Sales,train$Date,mean)
plot(a)

plot(tapply(train$Sales,train$DayOfWeek,mean))
hist(train$Open)
hist(train$Customers)
hist(train$SchoolHoliday)
hist(train$Promo)


# date 
#train$Date
#splitdate <- strsplit(train$Date, "/")
#matrix(unlist(splitdate), ncol=3, byrow=TRUE)
train$Day
train$Month
train$Year


plot(tapply(train$Sales,train$Month,mean),xlab="Month",ylab="Mean of Sales")
unique_months <- unique(train$Month)
length(unique_months)

plot(tapply(train$Sales,train$Day,mean),xlab="Day",ylab="Mean of Sales")
unique_days <- unique(train$Day)
length(unique_days)
summary(train$Day)


plot(tapply(train$Sales,train$Year,mean),xlab="Year",ylab="Mean of Sales")
hist(train$Year)
unique_years <- unique(train$Year)
unique_years
length(unique_years)

plot(tapply(train$Sales,train$Dayofweek,mean),xlab="DayofWeek",ylab="Mean of Sales")

hist(train$Promo)
barplot(train$Promo) 

unique_promo <- unique(train$Promo)
unique_promo
plot(tapply(train$Sales,train$Promo,mean),xlab="Promo",ylab="Mean of Sales")

#merging
m=merge(train,store,by="Store")
m
unique_competition_distance <- unique(m$CompetitionDistance)
unique_competition_distance

summary(m$CompetitionDistance)
