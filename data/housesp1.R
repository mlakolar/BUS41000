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
