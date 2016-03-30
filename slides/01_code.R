###############################
#  41000: R scripts
###############################

# install and update packages needed for the course
# the first time you run this command, it will take a few minutes
source('https://raw.githubusercontent.com/mlakolar/BUS41000/master/BUS41000.packages.R')

############################### 
#  Week 1
###############################

# set working directory
# you should change the folder to a location
# on your own computer
setwd("/home/mkolar/Downloads/")

# if you are not familiar with one of the commands, you can always 
# type a question mark in front of the command to get help
# for example
?plot


############################### 
#  Marketing data
###############################

# download data to the current working folder
# recall: you can check what is the current working directory by typing getwd()
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/marketing.csv",
              destfile="marketing.csv")

# read the data from the file we just downloaded 
marketing_df = read.csv("marketing.csv")

# look at data
dim(marketing_df)
nrow(marketing_df)
ncol(marketing_df)

head(marketing_df)       
tail(marketing_df)
marketing_df 
head(marketing_df[,1:12])   # look at only first 12 columns

names(marketing_df)         # obtain names of columns

marketing_df$age            # look at the age column
marketing_df[, "age"]

# use histogram to plot age of customeres
hist(marketing_df$age, xlab="Age", main = "Histogram of Age")

# annotate mean and median
abline(v = mean(marketing_df$age), col="blue", lty=2)
abline(v = median(marketing_df$age), col="red", lty=2)
legend(x = "topright", c("Mean", "Median"), col = c("blue", "red"), lty = c(2, 2))

# we can also calculate the mean and standard deviation
mean(marketing_df$age); sd(marketing_df$age)

# obtain summary statistics for age
summary(marketing_df$age)
quantile(marketing_df$age)

# how many people in each socio economic status
soc_table = table(marketing_df$soc)
soc_table
plot(soc_table, xlab='Social Category', ylab='Number respondents', main='British Marketing Survey')


# create a boxplot of age vs soc
boxplot(age~soc, marketing_df, main="Age vs Socio Economic status", ylab="age", xlab="soc")

# create a two way table to investigate relationship between discrete variables
table(marketing_df[,c("colas","simpsons")])
table(marketing_df[,c("soc","cigs")])   # Lower social grades seem to smoke more cigarettes.




############################### 
#  Bank Interarrival Times
###############################

# download data to the current working folder
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/bank.csv", destfile="bank.csv")

bank_df = read.csv("bank.csv")
# breaks = 25 tells R to create a histogram with 25 breaks
hist(bank_df[,1], breaks = 25, xlab='Minutes', main='Interarrival Times for 300 Bank Customers')

# annotate mean and median
abline(v = mean(bank_df[,1]), col="blue", lty=2)
abline(v = median(bank_df[,1]), col="red", lty=2)
legend(x = "topright", c("Mean", "Median"), col = c("blue", "red"), lty = c(2, 2))



############################### 
#   Standard & Poors 500 Index 
###############################

# from Jan 2002 till Sept 2015

# download data to the current working folder
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/GSPC.csv", destfile="GSPC.csv")

gspc_df = read.csv("GSPC.csv")
head(gspc_df, n = 4)

# plot adjusted close price as time series
plot(x=as.Date(gspc_df$Date, format = "%m/%d/%Y"), 
     y=gspc_df$Adj.Close, type="l", col="red", 
     xlab="Date", ylab="Adjusted close", main = "Standard & Poors Index")



############################### 
#   Country Monthly Returns
###############################

library(maptools)

# Monthly return data on country's broadband portfolios from Feb 1988 till Dec 1996.

# download data to the current working folder
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/CountryMonthlyReturns.csv", 
              destfile="CountryMonthlyReturns.csv")

countryReturn_df = read.csv("CountryMonthlyReturns.csv")
names(countryReturn_df)
head(countryReturn_df)
summary(countryReturn_df)

# summary statistics for canada returns
summary(countryReturn_df$canada)
quantile(countryReturn_df$canada)
quantile(countryReturn_df$canada, 0.3)
quantile(countryReturn_df$canada, probs=c(0.25, 0.5, 0.75))      # compute quartiles
range(countryReturn_df$canada)
IQR(countryReturn_df$canada)

# Let us compare the Canadian and Japanese returns
mean(countryReturn_df$canada); sd(countryReturn_df$canada)
mean(countryReturn_df$japan); sd(countryReturn_df$japan)

# time series plots

par(mfrow=c(2,1))  # two plots in one, one above another

plot(x=as.Date(paste0(countryReturn_df$date, "-01"), format = "%Y%m-%d"), 
     y=countryReturn_df$canada, type="l", col="blue", 
     xlab="", ylab="Canadian returns", main = "")

plot(x=as.Date(paste0(countryReturn_df$date, "-01"), format = "%Y%m-%d"), 
     y=countryReturn_df$japan, type="l", col="blue", 
     xlab="", ylab="Japanese returns", main = "")


# histogram of return 
par(mfrow=c(1, 2))   # two plots in one, next to each other
h = 0.01
numBin1 = (max(countryReturn_df$japan) - min(countryReturn_df$japan))/h
numBin2 = (max(countryReturn_df$canada) - min(countryReturn_df$canada))/h

hist(countryReturn_df$japan, breaks = ceiling(numBin1), 
     xlab = "", ylab = "Frequency", main="Japanese returns", 
     xlim = c(-0.25, 0.3), ylim = c(0, 18))

hist(countryReturn_df$canada, breaks = ceiling(numBin2), 
     xlab = "", ylab = "", main="Canadian returns", 
     xlim = c(-0.25, 0.3), ylim = c(0, 18))



# illustrate empirical rule for canadian returns
par(mfrow=c(1, 1))   
hist(countryReturn_df$canada, breaks = 20, 
     xlim = c(-0.12, 0.12), ylim=c(0,15),
     main="", xlab ="Returns", ylab="Frequency")
mean_canada_return = mean(countryReturn_df$canada)
sd_canada_return = sd(countryReturn_df$canada)
abline(v=mean_canada_return, col="red")
abline(v=mean_canada_return-sd_canada_return, col="blue")
abline(v=mean_canada_return+sd_canada_return, col="blue")
abline(v=mean_canada_return-1.96*sd_canada_return, col="green")
abline(v=mean_canada_return+1.96*sd_canada_return, col="green")


plot(x=as.Date(paste0(countryReturn_df$date, "-01"), format = "%Y%m-%d"), 
     y=countryReturn_df$canada, type="l", col="blue", 
     xlab="", ylab="Canadian returns", main = "")
abline(h=mean_canada_return-1.96*sd_canada_return, col="green")
abline(h=mean_canada_return+1.96*sd_canada_return, col="green")

# comparing country returns based on mean and standard deviation

mcr = sapply(countryReturn_df[,-1], mean)            # compute mean for each column
sdcr = sapply(countryReturn_df[,-1], sd)             # compute standard deviation for each column  
plot(sdcr, mcr, type="p", xlim=c(0.033, 0.08), ylim=c(0, 0.025), main="", xlab = "std. dev.", ylab = "mean", cex=0.5)
pointLabel(sdcr, mcr, labels=names(mcr), cex= 0.7)

# histogram with a box plot

nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(0,3,0,0))
boxplot(countryReturn_df$canada, horizontal=TRUE,
        axes = FALSE,
        ylim = c(-0.12, 0.12),
        main = "")
par(mar=c(3,3,0,0), mgp=c(2,1,0))
hist(countryReturn_df$canada, breaks = 15, 
     xlim = c(-0.12, 0.12), ylim=c(0,15),
     main="", xlab ="Returns", ylab="Frequency")
qcanada = quantile(countryReturn_df$canada, probs = c(0.25,0.5,0.75))
abline(v=qcanada[2], col="red")
abline(v=qcanada[1], col="cyan")
abline(v=qcanada[3], col="cyan")
text(qcanada[2], y=14, labels = "Q2", pos=2, col="red")
text(qcanada[1], y=14, labels = "Q1", pos=2, col="cyan")
text(qcanada[3], y=14, labels = "Q3", pos=4, col="cyan")

dev.off() # close figure

# Boxplot of returns per country
boxplot(x = as.list(countryReturn_df[,-1]), las=2)

# many scatter plots
pairs(countryReturn_df[,2:6])

# covariance matrix
cov(countryReturn_df[,2:6])
# correlation matrix 
cor(countryReturn_df[,2:6])


############################### 
#   Mutual Funds Data
###############################

# monthly data on 12 different assets from July 1996 to Dec. 2013

# download data to the current working folder
download.file("https://raw.githubusercontent.com/mlakolar/BUS41000/master/data/mutualFundReturn.csv", destfile="mutualFundReturn.cvs")

mfr_df = read.csv("mutualFundReturn.cvs")

names(mfr_df)
head(mfr_df)
summary(mfr_df)

mfr_df = mfr_df[,-1]                  # drop first column as it represents date
mean.mfr = sapply(mfr_df, mean)       # compute mean for each column
sd.mfr = sapply(mfr_df, sd)           # compute standard deviation for each column
cbind(mean.mfr, sd.mfr)               

# create a scatter plot using mean and standard deviations
plot(sd.mfr, mean.mfr, type="p", xlim=c(0, 0.11), ylim=c(0, 0.012),
     main="", xlab = "std. dev.", ylab = "mean")
text(sd.mfr, mean.mfr, labels=names(mfr_df), cex= 0.7, pos=3)

# create a scatter plot for a mutual fund and the market
plot(mfr_df$GSPC, mfr_df$VWNDX, cex=.5, xlim=c(-.22,.22), ylim=c(-.22,.22), 
     xlab="GSPC", ylab="VWNDX", main="VWNDX vs GSPC")




############################### 
#   MBA Beer Consumption
###############################

# download data to the current working folder
download.file("https://raw.githubusercontent.com/mlakolar/BUS41000/master/data/BeerConsumptionMBA.csv", 
              destfile="BeerConsumptionMBA.csv")

beer_df = read.csv("BeerConsumptionMBA.csv")

head(beer_df)
summary(beer_df)

plot(beer_df$weight, beer_df$nbeer, main="MBA Beer Consumption Survey", xlab="weight", ylab="number of beers")

# find the outlier
iOutlier = which(beer_df$nbeer == max(beer_df$nbeer))
beer_df[iOutlier, ]


##################################
#  GE 1990-today Data
##################################

library(fImport)

Y = yahooSeries("GE", from = "1990-01-01")
# take a look of the data
head(Y)
plot(Y[,6],type="l",col=20,main="GE Share Price", ylab="Price",xlab="Time")   # plot adjusted closing price

# Calculate Returns
y = rev(Y$`GE.Adj.Close`)
n = length(y)
rets = y[-1]/y[-n]-1

# summarize returns
summary(rets)

plot(rets,pch=20,col=24,cex=0.5)
abline(0,0)

hist(rets,breaks=50,freq=FALSE,main="GE Returns",col="red")

# find the highest return
iMax = which(rets == max(rets))        # index of the highest return
rets[iMax]                             # largest return
Y[iMax, ]                              # obtain the date 

# find the lowest return
iMin = which(rets == min(rets))        # index of the lowest return
rets[iMin]                             # lowest return
Y[iMin, ]                              # obtain the date of the lowest 

