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
