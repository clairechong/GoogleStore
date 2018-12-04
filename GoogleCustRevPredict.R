library(readr)
library(data.table)
library(dplyr)
library(tidyr)
library(jsonlite)

setwd("C:/Users/jpano/Documents/DACourse/Practice/competition/Google_customer_revenue_prediction")
trainDF=read_csv("train.csv", col_name=TRUE, na=c("","NA","#NA"))

testDF=read_csv("test.csv", col_name=TRUE, na=c("","NA","#NA"))

#check first row of each dataframe to verify data loaded and colnames to see if they are the same in train and test
head(trainDF, n=1)
colnames(trainDF)
head(testDF, n=1)
colnames(testDF)


#dimensions of train dataframe
dim(trainDF)

#dimensions of test dataframe
dim(testDF)

#describe the data
str(trainDF)
str(testDF)


#convert JSON columns into dataframes and the combine them back into one dataframe
tr_device <- paste("[", paste(trainDF$device, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_geoNetwork <- paste("[", paste(trainDF$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_totals <- paste("[", paste(trainDF$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_trafficSource <- paste("[", paste(trainDF$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

te_device <- paste("[", paste(testDF$device, collapse = ","), "]") %>% fromJSON(flatten = T)
te_geoNetwork <- paste("[", paste(testDF$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
te_totals <- paste("[", paste(testDF$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
te_trafficSource <- paste("[", paste(testDF$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

trainDF1 <- cbind(trainDF, tr_device, tr_geoNetwork, tr_totals, tr_trafficSource) %>% as.data.table()
testDF1 <- cbind(testDF, te_device, te_geoNetwork, te_totals, te_trafficSource) %>% as.data.table()

# #Check that JSON conversion worked and each is its own dataframe
head(tr_device, n=1)
head(tr_geoNetwork, n=1)
head(tr_totals, n=1)
head(tr_trafficSource, n=1)

head(te_device, n=1)
head(te_geoNetwork, n=1)
head(te_totals, n=1)
head(te_trafficSource, n=1)

#drop the old JSON columns
trainDF1$device=trainDF1$geoNetwork=trainDF1$totals=trainDF1$trafficSource=NULL
testDF1$device=testDF1$geoNetwork=testDF1$totals=testDF1$trafficSource=NULL

#check dimensions to ensure correct after dropping columns
dim(trainDF1)
dim(testDF1)

#check datatypes to determine what datatypes need conversion
str(trainDF1)

###########################DROP COLUMNS NOT NEEDED#######################################
#visitID is a duplicate of visitStartTime so drop
trainDF1$visitId=NULL

#sessionID is made up of fullVisitorId and visitStartTime so drop
trainDF1$sessionId=NULL

#############################DATA TYPE CONVERSION########################################
#convert date to date type
trainDF1$date=as.Date(as.character(trainDF1$date),"%Y%m%d")
testDF1$date=as.Date(as.character(testDF1$date),"%Y%m%d")

#convert visitstarttime
library(lubridate)
trainDF1$visitStartTime <- as_datetime(trainDF1$visitStartTime)
testDF1$visitStartTime <- as_datetime(testDF1$visitStartTime)

trainDF1$visits <- as.numeric(trainDF1$visits)
trainDF1$hits<- as.numeric(trainDF1$hits)
trainDF1$pageviews<- as.numeric(trainDF1$pageviews)
trainDF1$bounces<- as.numeric(trainDF1$bounces)
trainDF1$transactionRevenue<- as.numeric(trainDF1$transactionRevenue)

testDF1$visits <- as.numeric(testDF1$visits)
testDF1$hits<- as.numeric(testDF1$hits)
testDF1$pageviews<- as.numeric(testDF1$pageviews)
testDF1$bounces<- as.numeric(testDF1$bounces)

###############examine the data######################################

# shows many missing values in several attributes
summary(trainDF1)

#num of unique values that aren't NA in each 
unique_vals <- sapply(trainDF1, function(x) { length(unique(x[!is.na(x)])) })
unique_vals

#Lots of columns only have 1 value so drop those columns
trainDF1 <- trainDF1[, -c("browserVersion","operatingSystemVersion","mobileDeviceModel", "mobileDeviceMarketingName", 
                     "screenColors", "cityId","longitude", "isTrueDirect", "campaignCode","adwordsClickInfo.isVideoAd",
                     "socialEngagementType","browserSize","mobileInputSelector","flashVersion","screenResolution",
                     "networkLocation","adwordsClickInfo.criteriaParameters","mobileDeviceBranding","mobileDeviceInfo",
                     "language", "latitude","visits","bounces","newVisits")]

dim(trainDF1)

#transaction revenue histogram shows heavily skewed towards zeo
hist(trainDF1$transactionRevenue)

summary(trainDF1$transactionRevenue)


#in order to use linear regression, need to log transform the target variable
trainDF1$log_rev=log(trainDF1$transactionRevenue)
summary(trainDF1$log_rev)
hist(trainDF1$log_rev)
boxplot(trainDF1$log_rev)

#time period of data shows data for one year
summary(trainDF1$date)

#make all NAs zero in transaction revenue
trainDF1$transactionRevenue[is.na(trainDF1$transactionRevenue)] <- 0
trainDF1$log_rev[is.na(trainDF1$log_rev)] <- 0

write.csv(trainDF1, "trainDF1.csv")

#########################VISUALIZATIONS##########################################################
#visualize - shows transaction revenue fluctuations over time
library(ggplot2)

#plot log revenue by channel grouping
train_gb1=aggregate(trainDF1$log_rev, by=list(trainDF1$channelGrouping), FUN=sum)
head(train_gb1)
ggplot(data = train_gb1, aes(x=Group.1, y=x))+geom_col()

#plot log revenue by browser
train_gb2=aggregate(trainDF1$log_rev, by=list(trainDF1$browser), FUN=sum)
summary(train_gb2) #shows min is zero, max is 185K
#let's see which browsers have more than $0 log revenue
browser=filter(train_gb2, x>0)
ggplot(data = browser, aes(x=Group.1, y=x))+geom_col()
#we can see that Chrome is the clear favourite
#zoom in to see the ones with less usage
ggplot(data = browser, aes(x=Group.1, y=x))+geom_col()+coord_cartesian(xlim=c(0,10),ylim=c(0,15000))
#safari is the next highest at 13K

#plot log revenue by operating system
train_gb3=aggregate(trainDF1$log_rev, by=list(trainDF1$operatingSystem), FUN=sum)
summary(train_gb3) 
#let's see which OS have more than $0 log revenue
opsys=filter(train_gb3, x>0)
opsys
ggplot(data = opsys, aes(x=Group.1, y=x))+geom_col()
#we can see that Macintosh is the clear favourite followed by Windows, then Chrome OS and Linux

#plot log revenue by fullVisitorId
train_gb4=aggregate(trainDF1$log_rev, by=list(trainDF1$fullVisitorId), FUN=sum)
#let's see which visitors generate more than $0 log revenue
visitor=filter(train_gb4, x>0)
dim(visitor)
summary(visitor) #max is $582
ggplot(data = visitor, aes(x=Group.1, y=x))+geom_col()


#visitorID with most log revenue
filter(visitor,x>500)

#plot log revenue by deviceCategory
train_gb5=aggregate(trainDF1$log_rev, by=list(trainDF1$deviceCategory), FUN=sum)
#let's see which devices generate more than $0 log revenue
device=filter(train_gb5, x>0)
summary(device) #max is 187728
ggplot(data = device, aes(x=Group.1, y=x))+geom_col()
#majority used desktop


#plot log revenue by continent
train_gb6=aggregate(trainDF1$log_rev, by=list(trainDF1$continent), FUN=sum)
#let's see which continents generate more than $0 log revenue
contfilt=filter(train_gb6, x>0)
summary(contfilt) #max is 201072
ggplot(data = contfilt, aes(x=Group.1, y=x))+geom_col()
#Americas generates the most log revenue

#plot log revenue by subcontinent
train_gb7=aggregate(trainDF1$log_rev, by=list(trainDF1$subContinent), FUN=sum)
#let's see which subcontinents generate more than $0 log revenue
subcontfilt=filter(train_gb7, x>0)
summary(subcontfilt) #max is $198531
subcontfilt  #North America generates the most log revenue
ggplot(data = subcontfilt, aes(x=Group.1, y=x))+geom_col()


#plot log revenue by country
train_gb8=aggregate(trainDF1$log_rev, by=list(trainDF1$country), FUN=sum)
#let's see which countries generate more than $0 log revenue
nation=filter(train_gb8, x>0)
summary(nation) #max is 195141
nation  #US generates the most log revenue
ggplot(data = nation, aes(x=Group.1, y=x))+geom_col()

#plot log revenue by region
train_gb9=aggregate(trainDF1$log_rev, by=list(trainDF1$region), FUN=sum)
#let's see which regions generate more than $0 log revenue
rgn=filter(train_gb9, x>0)
summary(rgn)
rgn[with(rgn,order(-x)),]  #first region we can identify with the most log revenue is California

#plot log revenue by metro
train_gb10=aggregate(trainDF1$log_rev, by=list(trainDF1$metro), FUN=sum)
#let's see which metros generate more than $0 log revenue
metr=filter(train_gb10, x>0)
summary(metr) #max is 81084
metr[with(metr,order(-x)),]  #first metro we can identify with the most log revenue is SanFran-Oak-SJ
ggplot(data = metr, aes(x=Group.1, y=x))+geom_col()

#plot log revenue by city
train_gb11=aggregate(trainDF1$log_rev, by=list(trainDF1$city), FUN=sum)
#let's see which cities generate more than $0 log revenue
cityfilt=filter(train_gb11, x>0)
summary(cityfilt) #max is 
cityfilt[with(cityfilt,order(-x)),]  #first metro we can identify with the most log revenue is New York! (doesn't align with the region and metro)
ggplot(data = cityfilt, aes(x=Group.1, y=x))+geom_col()

#plot log revenue by campaign
train_gb12=aggregate(trainDF1$log_rev, by=list(trainDF1$campaign), FUN=sum)
#let's see which campaigns generate more than $0 log revenue
cpgn=filter(train_gb12, x>0)
summary(cpgn) #max is 197050
cpgn[with(cpgn,order(-x)),]  #dymanic search ads is the top campaign
ggplot(data = cpgn, aes(x=Group.1, y=x))+geom_col()


#plot log revenue by source
train_gb13=aggregate(trainDF1$log_rev, by=list(trainDF1$source), FUN=sum)
#let's see which sources generate more than $0 log revenue
srce=filter(train_gb13, x>0)
summary(srce) #max is 91378
srce[with(srce,order(-x)),]  #mall.googleplex.com is the top source
ggplot(data = srce, aes(x=Group.1, y=x))+geom_col()

#plot log revenue by medium
train_gb14=aggregate(trainDF1$log_rev, by=list(trainDF1$medium), FUN=sum)
#let's see which mediums generate more than $0 log revenue
med=filter(train_gb14, x>0)
summary(med) #max is 96956
med[with(med,order(-x)),]  #referral is the top medium
ggplot(data = med, aes(x=Group.1, y=x))+geom_col()

#################################CORRELATION MATRIX##############################################################
#on numeric data only
CorrData=trainDF1[,c("log_rev", "transactionRevenue","visitNumber","hits","pageviews")]
cor(CorrData, method = c("pearson"),  use = "complete.obs")

#correlation matrix shows hits and page views are highly correlated but no other significant correlations (>50%) 
#pageviews and log_rev are more correlated than others at 40%
#hits and log_rev are more correlated than others at 38%
#page views is not correlated with transaction revenue nor visit number for example

##############################modelling##########################################################################
fit <- lm(trainDF1$log_rev~trainDF1$visitNumber+trainDF1$hits+trainDF1$pageviews, data = trainDF1)
summary(fit)
#adjusted R sq is only 17% meaning the model explains 17% of the behaviour of the dependent variables
#visitNumber, hits, pageviews
#not a good fit, need to add more variables but the other vars are non-numeric so we have to factorize

#############################factorize###########################################################################
#select the categorical columns
library(tidyverse)

train_fact=trainDF1

cols_cat = train_fact %>% select_if(negate(is.numeric))
cols_cat=cols_cat %>% select_if(negate(is.Date))
cols_cat=cols_cat %>% select_if(negate(is.POSIXct))
cols_cat=cols_cat %>% select_if(negate(is.POSIXct))
cols_cat$fullVisitorId=NULL #we don't want to factorize the visitor ID

colnames(cols_cat)

#factor value into levels
cols_cat <- lapply(cols_cat,factor)
str(cols_cat)

#convert the levels into numeric values                   
cols_cat <- lapply(cols_cat,as.numeric)
str(cols_cat)

#Next steps:  update the trainDF with numeric, factorized values from cols_cat and then feed linear regressoin model

