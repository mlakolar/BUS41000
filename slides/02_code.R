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

(shopping_df = read.csv("shopping.csv", row.names=1))

# plot data using first two columns
plot(x=shopping_df[,1], y=shopping_df[,2], main="Shopping attitudes",
     xlab="Shopping is fun", ylab="Shopping is bad for your budget")

# setting the seed will allow us to reproduce results
# otherwise the result will change every time
set.seed(1)                
# cluster into three groups
grpShopper = kmeans(shopping_df, centers=3, nstart=100)

plot(x=shopping_df[,1], y=shopping_df[,2], main="Three clusters",
     col=grpShopper$cluster+1, 
     xlab="Shopping is fun", ylab="Shopping is bad for your budget")
points(x=grpShopper$centers[,1], y=grpShopper$centers[,2], col=c(2,3,4), pch=3)

# investigate centers
t( grpShopper$centers )

# visualize kmeans
library(animation)

## set larger 'interval' if the speed is too fast
oopt = ani.options(interval = 2)
kmeans.ani(shopping_df, centers=3)


################################## 
#   European Protein Consumption
##################################

# European Protein Consumption, in grams/person-day 

library(maptools)

download.file("https://raw.githubusercontent.com/mlakolar/BUS41000/master/data/protein.csv", destfile="protein.csv")

food_df = read.csv("protein.csv", row.names=1) # 1st column is country name
# scale the data so that every column has sample mean equal to zero and variance equal to 1
food_scaled = scale(food_df)  

plot(food_scaled[,"RedMeat"], food_scaled[,"WhiteMeat"], xlim=c(-2,2.75), 
     type="n", xlab="Red Meat", ylab="White Meat")
pointLabel(food_scaled[,"RedMeat"], food_scaled[,"WhiteMeat"],  
           labels=rownames(food_scaled))

## first, consider just Red and White meat clusters
grpMeat <- kmeans(food_scaled[,c("WhiteMeat","RedMeat")], centers=3, nstart=100)
grpMeat

plot(food_scaled[,"RedMeat"], food_scaled[,"WhiteMeat"], xlim=c(-2,2.75), 
     type="n", xlab="Red Meat", ylab="White Meat",
     main="3-means clustering on Red vs White meat consumption")
pointLabel(food_scaled[,"RedMeat"], food_scaled[,"WhiteMeat"],
           labels=rownames(food_scaled), col=grpMeat$cluster+1)

# which contries belong to which cluster
rownames(food_df)[grpMeat$cluster==1]
rownames(food_df)[grpMeat$cluster==2]
rownames(food_df)[grpMeat$cluster==3]

## same plot, but now with clustering on all protein groups
grpProtein <- kmeans(food_scaled, centers=7, nstart=100) ## change the number of centers to see what happens.
grpProtein

plot(food_scaled[,"RedMeat"], food_scaled[,"WhiteMeat"], xlim=c(-2,2.75), 
     type="n", xlab="Red Meat", ylab="White Meat",
     main="7-means clustering on all nine protein types")
text(food_scaled[,"RedMeat"], food_scaled[,"WhiteMeat"], labels=rownames(food_scaled), 
     col=grpMeat$cluster+1) ## col is all that differs from first plot


################################## 
#   Scotch
##################################

library(proxy)

download.file("https://raw.githubusercontent.com/mlakolar/BUS41000/master/data/scotch.csv", destfile="scotch.csv")

# there are two header lines
# in order to deal with this, we follow 
# http://stackoverflow.com/questions/17797840/reading-two-line-headers-in-r
header = scan("scotch.csv", nlines = 1, what = character(), sep=",")
header2 = scan("scotch.csv", skip = 1, nlines = 1, what = character(), sep=",")

scotch_df = read.csv("scotch.csv", skip = 2, header = FALSE, row.names = 1)
names(scotch_df) = paste0(header[-1], "/", header2[-1])

scotch_df = scotch_df[,1:68]                               # drop columns that do not have characteristics
head(scotch_df)

indA = which(rownames(scotch_df) == "Ardberg")             # find a row corresponding to ardbeg
# compute distance between "Ardbeg" and all other scotches using Jaccard distance
# see: https://en.wikipedia.org/wiki/Jaccard_index
distM = proxy::dist(scotch_df[indA, ], y=scotch_df, method="jaccard")     

# names of 5 most similar scotches (the distance of ardbeg to itself is 0)
rownames(scotch_df)[ order(distM)[1:6] ]         
# show distance
distM[ order(distM)[1:6] ]

