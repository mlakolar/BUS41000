############################### 
#   Country Monthly Returns
###############################

# if you do not have the library maptools installed
# install.packages("maptools")
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

# Boxplot of returns per country
par()
boxplot(x = as.list(countryReturn_df[,-1]), las=2)

# many scatter plots
pairs(countryReturn_df[,2:6])

# covariance matrix
cov(countryReturn_df[,2:6])
# correlation matrix 
cor(countryReturn_df[,2:6])