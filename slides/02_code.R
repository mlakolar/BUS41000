###############################
#  41000: R scripts
###############################

# install and update packages needed for the course
# the first time you run this command, it will take a few minutes
source('https://raw.githubusercontent.com/mlakolar/BUS41000/master/BUS41000.packages.R')

############################### 
#  Week 2
###############################

# set working directory
# you should change the folder to a location
# on your own computer
setwd("/home/mkolar/Downloads/")




############################### 
#   Country Monthly Returns
###############################

library(maptools)

# Monthly return data on country's broadband portfolios from Feb 1988 till Dec 1996.

# download data to the current working folder
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/CountryMonthlyReturns.csv", 
              destfile="CountryMonthlyReturns.csv")

countryReturn_df = read.csv("CountryMonthlyReturns.csv")
port_df = countryReturn_df[,c("usa", "honkong")]
port_df$port1 = 0.5*port_df$honkong + 0.5*port_df$usa
head(port_df)

mean(port_df$honkong)
mean(port_df$usa)
mean(port_df$port1)

var(port_df$honkong)
var(port_df$usa)
var(port_df$port1)

sd(port_df$honkong)
sd(port_df$usa)
sd(port_df$port1)

0.5*(sd(port_df$honkong) + sd(port_df$usa))
sd(port_df$port1) < 0.5*(sd(port_df$honkong) + sd(port_df$usa))

mcr = sapply(port_df, mean)            # compute mean for each column
sdcr = sapply(port_df, sd)             # compute standard deviation for each column  
plot(sdcr, mcr, type="p", main="Scatter for the portfolio with equal weights", xlab = "std. dev.", ylab = "mean", cex=0.7)
pointLabel(sdcr, mcr, labels=names(mcr), cex= 1)

cov(port_df)
var(port_df$port1) == (0.5)^2*var(port_df$honkong)+(0.5)^2*var(port_df$usa)+2*0.5*0.5*cov(port_df$honkong, port_df$usa)



############################### 
#   Mutual Funds Data
###############################

# monthly data on 12 different assets from July 1996 to Dec. 2013

# download data to the current working folder
download.file("https://raw.githubusercontent.com/mlakolar/BUS41000/master/data/mutualFundReturn.csv", destfile="mutualFundReturn.cvs")

mfr_df = read.csv("mutualFundReturn.cvs")
port_df = countryReturn_df[,c("VWNDX", "PTTRX", "LBF")]
port_df$port = 0.1*port_df$VWNDX + 0.7*port_df$PTTRX + 0.2*port_df$LBF
cov(port_df)


############################### 
#   Home prices data 
###############################

# download data to the current working folder
download.file("https://raw.githubusercontent.com/mlakolar/BUS41000/master/data/housesp1.csv", destfile="housesp1.csv")

homep_df = read.csv("housesp1.csv")

# create scatte plot
plot(homep_df$size, homep_df$price, xlab="Size (sq. feet)", ylab="Price")

# add regression line
reg = lm(price ~ size, homep_df)
abline(reg, lw=2, col="green")

# summary of linear regression 
summary(reg)

# histogram of prices
hist(homep_df$price, breaks=20, main="Histogram of house prices", xlab="Price", ylab="Frequency")

new_point = data.frame(size = c(2200))
yhat = predict(reg, new_point)
yhat
plot(homep_df$size, homep_df$price, xlab="Size (sq. feet)", ylab="Price")
abline(reg, lw=2, col="green")
lines(x=c(2200, 2200), y=c(0, yhat), col="blue", lty=2)
lines(x=c(0, 2200), y=c(yhat, yhat), col="blue", lty=2)



############################### 
#   Shopping attitudes 
###############################


# download data to the current working folder
download.file("https://raw.githubusercontent.com/mlakolar/BUS41000/master/data/shopping.csv", destfile="shopping.csv")

shopping_df = read.csv("shopping.csv")




############################### 
#   European Protein Consumption
###############################

# *** European Protein Consumption, in grams/person-day *** 

download.file("https://raw.githubusercontent.com/mlakolar/BUS41000/master/data/protein.csv", destfile="protein.csv")

food_df = read.csv("protein.csv", row.names=1) # 1st column is country name
# scale the data so that every column has sample mean equal to zero and variance equal to 1
food_scaled = scale(food)  

## first, consider just Red and White meat clusters
(grpMeat <- kmeans(xfood[,c("WhiteMeat","RedMeat")], centers=3, nstart=10))

plot(xfood[,"RedMeat"], xfood[,"WhiteMeat"], xlim=c(-2,2.75), 
     type="n", xlab="Red Meat", ylab="White Meat")
text(xfood[,"RedMeat"], xfood[,"WhiteMeat"], labels=rownames(food), 
     col=rainbow(3)[grpMeat$cluster])

## same plot, but now with clustering on all protein groups
grpProtein <- kmeans(xfood, centers=7, nstart=50) ## change the number of centers to see what happens.
grpProtein

plot(xfood[,"RedMeat"], xfood[,"WhiteMeat"], xlim=c(-2,2.75), 
     type="n", xlab="Red Meat", ylab="White Meat")
text(xfood[,"RedMeat"], xfood[,"WhiteMeat"], labels=rownames(food), 
     col=rainbow(7)[grpProtein$cluster]) ## col is all that differs from first plot

