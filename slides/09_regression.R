###############################
#  41000: R scripts
###############################

# install and update packages needed for the course
# the first time you run this command, it will take a few minutes
source('https://raw.githubusercontent.com/mlakolar/BUS41000/master/BUS41000.packages.R')

############################### 
#  Code for regression
###############################

# set working directory
# you should change the folder to a location
# on your own computer
setwd("/home/mkolar/Downloads/")

##	Data files:
##		-pickup.csv
##		-wages.csv
##      -mutualFundReturn.csv
##		-telemarketing.csv
##		-sales.csv
##		-grades.csv
##		-census2000.csv
##      -nba.csv

# download data to the current working folder
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/pickup.csv", destfile="pickup.csv")
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/wages.csv", destfile="wages.csv")
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/mutualFundReturn.csv", destfile="mutualFundReturn.csv")
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/telemarketing.csv", destfile="telemarketing.csv")
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/sales.csv", destfile="sales.csv")
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/grades.csv", destfile="grades.csv")
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/census2000.csv", destfile="census2000.csv")
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/nba.csv", destfile="nba.csv")


#######################################################
##  Pickup
#######################################################

## Pickup data visualization
data <- read.csv("pickup.csv")
names(data)

# subsetting data in R
data[,1]
data[,1:3]
data[1:10,]
data[1,2]

## R's summary function is pretty clever
summary(data)

## Histograms
## pdf("puhist.pdf", height=4, width=11)
par(mfrow=c(1,3)) # break the plot into a 1x3 matrix
hist(data$year)
hist(data$miles)
hist(data$price)
## dev.off()

## Scatterplots
## B&W
## pdf("puscatter.pdf", height=4)
par(mfrow=c(1,2))
plot(data$year, data$price, pch=20)
plot(data$miles, data$price, pch=20)
## dev.off()

## Color
## pdf("puscattercol.pdf", height=4)
par(mfrow=c(1,2))
plot(data$year, data$price, pch=20, col=data$make)
legend("topleft", levels(data$make), fill=1:3)
plot(data$miles, data$price, pch=20, col=data$make)
## dev.off()

## Boxplots
attach(data)
year_boxplot <- factor(1*(year<1995) + 2*(1995<=year & year<2000) + 3*(2000<=year & year<2005) + 4*(2005<=year & year<2009), labels=c("<1995", "'95-'99", "2000-'04", "'05-'09"))
## pdf("puboxplots.pdf", height=5, width=12)
par(mfrow=c(1,2))
boxplot(price ~ make, ylab="Price ($)", main="Make", cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
boxplot(price ~ year_boxplot, ylab="Price ($)", main="Year", cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
## dev.off()


## regression
## pdf("puSLRs.pdf", height=4)
par(mfrow=c(1,2))
plot(data$year, data$price, pch=20, col=data$make)
## abline adds lines to graphs. It knows to add the regression line!
abline(lm(price ~ year),lwd=1.5)
legend("topleft", levels(data$make), fill=1:3)
plot(data$miles, data$price, pch=20, col=data$make)
abline(lm(price ~ miles),lwd=1.5)
## dev.off()






#######################################
## Conditional distibutions
#######################################

par(mfrow=c(1,1))
## pdf("cndprice_scatter.pdf", width=8, height=4)
## fake house price data
x <- runif(200, 0.5, 3.5)
e <- rnorm(200, 0, 50)
y <- 120 + 75*x + e
plot(x, y, xlab="size", ylab="price")
abline(120, 75, lwd=2)
v <- c(1, 1.5, 2, 2.5, 3, 3.5)
abline(v=v, col="green", lty=2)
## dev.off()

## pdf("cndprice_box.pdf", width=8, height=4)
bins <- list()
bins$marg <- y[y >= 1]
bins[["1-1.5"]] <- y[x >= 1 & x < 1.5]
bins[["1.5-2"]] <- y[x >= 1.5 & x < 2]
bins[["2-2.5"]] <- y[x >= 2 & x < 2.5]
bins[["2.5-3"]] <- y[x >= 2.5 & x < 3]
bins[["3-3.5"]] <- y[x >= 3 & x < 3.5]
boxplot(bins, col="green", ylab="price")
lines(c(2, 6), 120+75*c(1.25,3.25), lwd=2)
## dev.off()

## pdf("cndprice_stops_scatter.pdf", width=8, height=4)
# more fake data
s <- rnorm(200, 2)
xs <- rep(NA, 200)
xs[s <= 0.5] <- 0
xs[s > 0.5 & s <= 1.5] <- 1
xs[s > 1.5 & s <= 2.5] <- 2
xs[s > 2.5 & s <= 3.5] <- 3
xs[s > 3.5] <- 4
plot(xs, y, xlab="# stops", ylab="price")
## dev.off()

## pdf("cndprice_stops_box.pdf", width=8, height=4)
sbins <- list()
sbins$marg <- y
sbins[["0"]] <- y[xs == 0]
sbins[["1"]] <- y[xs == 1]
sbins[["2"]] <- y[xs == 2]
sbins[["3"]] <- y[xs == 3]
sbins[["4"]] <- y[xs >= 4]
boxplot(sbins, col="green", ylab="price")
## dev.off()



#####################################################################
# Correlation
#
#  remember: linear regression is about capturing linear relationship
#####################################################################

#install.packages("mvtnorm") #you only need to run this once
library("mvtnorm") #This you have to run each time you start a new R session

## pdf("corr.pdf")
par(mfrow=c(2,2), mai=c(.5,.5,.1,.1))
plot(rmvnorm(200,rep(0,2), matrix(c(1,1,1,1), ncol=2)), pch=20, xlab="", ylab="",
     xlim=c(-3,3), ylim=c(-3,3))
text(x=-2, y=2.5, "corr = 1", col=2, cex=1.3)
plot(rmvnorm(200,rep(0,2), matrix(c(1,.5,.5,1), ncol=2)), pch=20, xlab="", ylab="",
     xlim=c(-3,3), ylim=c(-3,3))
text(x=-2, y=2.5, "corr = .5", col=2, cex=1.3)
plot(rmvnorm(200,rep(0,2), matrix(c(1,.8,.8,1), ncol=2)), pch=20, xlab="", ylab="",
     xlim=c(-3,3), ylim=c(-3,3))
text(x=-2, y=2.5, "corr = .8", col=2, cex=1.3)
plot(rmvnorm(200,rep(0,2), matrix(c(1,-.8,-.8,1), ncol=2)), pch=20, xlab="", ylab="",
     xlim=c(-3,3), ylim=c(-3,3))
text(x=2, y=2.5, "corr = -.8", col=2, cex=1.3)
## dev.off()

## pdf("strangecor.pdf", height=4)
par(mfrow=c(1,2), mai=c(.5,.5,.5,.1))
x <- rnorm(200)
y <- -x^2 + rnorm(200,0,.2)
z <- cbind(x,y)
plot(z, pch=20, xlab="", ylab="", main=paste("corr =", round(cor(z[,1], z[,2]),2)))

z <- rbind(rmvnorm(199,c(0,0)), c(20,20))
plot(z, pch=20, xlab="", ylab="", main=paste("corr =", round(cor(z[,1], z[,2]),2)))
## dev.off()





###################################################################
## Housing data: just price (in $100,000) vs size (in 1000 sq.ft.)
###################################################################
size <- c(.8,.9,1,1.1,1.4,1.4,1.5,1.6,1.8,2,2.4,2.5,2.7,3.2,3.5)
price <- c(70,83,74,93,89,58,85,114,95,100,138,111,124,161,172)

## pdf("housedata.pdf", width=6, height=4.5)
plot(size, price, pch=20)
## dev.off()

## pdf("eyeball.pdf", width=6, height=4.5)
plot(size, price, pch=20) ## repeated from above
abline(35, 40, col="red") ## eyeball method (abline takes the slope and intercept)
## dev.off()


## least squares regression
reg <- lm(price ~ size) 
reg

## Just for fun, you can compute the coefficients manually:
b1 <- cor(price,size)*sd(price)/sd(size)
b0 <- mean(price) - mean(size)*b1
cbind(b0,b1)
coef(reg)


## pdf("eyevls.pdf", width=6, height=4.5)
plot(size, price, pch=20) ## repeated from above
abline(35, 40, col="red") ## repeated from above
abline(reg, col="green")
legend("bottomright", c("eyeball", "LS"), col=c("red", "green"), lty=1)
## dev.off()

## Plots of fitted values and residuals
## pdf("fittedVx.pdf", width=6, height=4.5)
plot(size, reg$fitted, pch=20, xlab="X", 
     ylab="Fitted Values")
text(x=3, y=80, col=2, cex=1.5,
     paste("corr(y.hat, x) =", cor(size, reg$fitted)))
## dev.off()

## pdf("eVx.pdf", width=6, height=4.5)
plot(size, reg$fitted-price, pch=20, xlab="X", 
     ylab="Residuals")
text(x=3.1, y=26, col=2, cex=1.5,
     paste("corr(e, x) =", round(cor(size, reg$fitted-price),2)))
text(x=3.1, y=19, col=4, cex=1.5,
     paste("mean(e) =", round(mean(reg$fitted-price),0)))
abline(h=0, col=8, lty=2)
## dev.off()

#### "Crazy line" illustration
## pdf("crazyline.pdf", width=6, height=4.5)
plot(size, price, pch=20, xlab="X", ylab="Y")
abline(a=b0, b=b1, col=4)
abline(a=10, b=50, col=2)
text(x=3, y=80, col=4, 
     paste("LS line:", round(b0,1), "+", round(b1,1), "X"))
text(x=1.5, y=140, "Crazy line: 10 + 50 X", col=2)
## dev.off()

## pdf("crazyresid.pdf", width=6, height=4.5)
crazyresid <- price - (10 + 50*size)
plot(size, crazyresid, pch=20, xlab="X", ylab="Crazy Residuals")
text(x=3, y=20, paste("corr(e, x) =",
                      round(cor(size, crazyresid),1)), col=2, cex=1.5)
text(x=3, y=13, paste("mean(e) =",
                      round(mean(crazyresid),1)), col=4, cex=1.5)
lines(size, lm(crazyresid ~ size)$fitted.values, col=2)
abline(h=0, col=8, lty=2)
## dev.off()


## confidence intervals example
summary(reg)
## Read the following values off of the summary:
b1 <- 35.386; sb1 <- 4.494
## Confidence intervals example (alpha = .05)
## Calculate everything yourself:
b1 + c(-1,1)*sb1*qnorm(.975) # 95% CI for slope (using c(-1,1) to get + or -)
b1 + c(-1,1)*sb1*qt(.975, df=13) # 95% CI for slope (using c(-1,1) to get + or -)
# 95% CIs using R's function:
confint(reg, level=0.95)

## Prediction for places of size Xf = mean(size=1.85, 2.5, or 3.5=max(size)
## You must use the same name as the data ('size')
Xf <- data.frame(size=c(mean(size), 2.5, max(size)))
## This gives you the 95% prediction interval
cbind(Xf, predict(reg, newdata=Xf, interval="prediction"))


## You can use the same function to get the se.fit
## (i.e. standard error of Y.hat at Xf)
p <- predict(reg, newdata=Xf, se.fit=TRUE)
p 
## calculate the same things ourself ...
s <- p$residual.scale ## or do s <- summary(reg)$sigma
## calculate se(Yhat)
sfit <- p$se.fit
## or do sfit <- s*sqrt(1/n + (Xf$size-mean(size))^2/((n-1)*var(size)))
## now spred
spred <- sqrt(s^2+sfit^2) 
b <- reg$coef
## pred interval at each Xf
rbind(b[1] + b[2]*Xf[1,]+ c(0,-1, 1)*qnorm(.975)*spred[1],
      b[1] + b[2]*Xf[2,]+ c(0,-1, 1)*qnorm(.975)*spred[1],
      b[1] + b[2]*Xf[3,]+ c(0,-1, 1)*qnorm(.975)*spred[1])
rbind(b[1] + b[2]*Xf[1,]+ c(0,-1, 1)*qt(.975, df=length(price)-2)*spred[1],
      b[1] + b[2]*Xf[2,]+ c(0,-1, 1)*qt(.975, df=length(price)-2)*spred[1],
      b[1] + b[2]*Xf[3,]+ c(0,-1, 1)*qt(.975, df=length(price)-2)*spred[1])





##########################
## Wage Data Example
##########################

D <- read.csv("wages.csv")
## Rename for convenience
Y <- D$HRS
X <- D$RATE
## Use correlation for slope
b1 <- cor(X,Y)*sd(Y)/sd(X) 
b1
## Put [X.bar,Y.bar] on the line
b0 <- mean(Y) - mean(X)*b1
b0

## Plot the data and our line
par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1)
## pdf("wageline.pdf", width=5, height=4.5)
plot(X, Y, xlab="rate", ylab="hours")
abline(b0, b1, col="red")
## dev.off()

## Plot the residuals Y - Y.hat
# pdf("resid.pdf", width=5, height=4.5)
e <- Y - (b0 + b1*X)
plot(X, e, xlab="rate", ylab="residuals",  pch=20)
abline(h=0, col=8, lwd=2) 
# dev.off()




#####################
## CAPM Example
#####################
mfund <- read.csv("mutualFundReturn.csv", row.names = 1)
names(mfund)
mean.mfr = apply(mfund, 2, mean)
sd.mfr = apply(mfund, 2, sd)
plot(sd.mfr, mean.mfr, type="p", xlim=c(0, 0.11), ylim=c(0, 0.012),
     main="", xlab = "std. dev.", ylab = "mean")
text(sd.mfr, mean.mfr, labels=names(mfund), cex= 0.7, pos=3)

plot(mfund$GSPC, mfund$VWNDX, pch=20, xlab="GSPC", ylab="VWNDX", main="VWNDX vs GSPC")
VWNDX.reg = lm(mfund$VWNDX ~ mfund$GSPC)
abline(VWNDX.reg, col="green")
(b_0 = round(coef(VWNDX.reg)[1],4) )
(b_1 = round(coef(VWNDX.reg)[2],4) )



#####################################################################
##	Perform Monte Carlo demonstration of least squares lines to show
##	the sampling distribution. 
#####################################################################

## pdf("sampling1.pdf", height=6)
ns <- 50                # change this for more data points
sigma2 <- 1
par(mfrow=c(2,2), mai=c(.5,.5,.1,.1))
for(i in 1:4) {
    x <- runif(ns, -3, 3)
    y <- x + rnorm(ns, sd=sqrt(sigma2))
    plot(x, y, pch=20, xlim=c(-3,3), ylim=c(-5,5))
    abline(0,1, col="blue", lwd=2)
    abline(lm(y~x), col="red", lty=2, lwd=2)
}
nc <- paste("n=", ns, sep="")
sc <- paste("var=", sigma2, sep="")
legend("bottomright", c(nc, sc,  "true model", "LS line"), 
       col=c(0, 0, "blue", "red"), lwd=2, lty=0:3, bty="n")

## Monte Carlo Exercise
## see LSMonteCarlo.R
#You can execute other R command files:
source("09_LSMonteCarlo.R")





################################
## dispersion -- effect of sigma
################################
x <- runif(200, 1, 3.5)
x0 <- 2
b0 <- 40
b1 <- 35
## pdf("cnd.pdf", height=4)
## par(mfrow=c(1,2))
## first plot
y <- b0 + b1*x + rnorm(200, sd=5)
y0a <- qnorm(0.001, mean=b0+b1*x0, sd=5)
y0b <- qnorm(0.999, mean=b0+b1*x0, sd=5)
y0 <- seq(y0a, y0b, length=100)
y0d <- dnorm(y0, mean=b0+b1*x0, sd=5)
plot(x, y, ylim=c(0,250), cex=0.5,
     main="small dispersion", ylab="Y", xlab="X")
lines(x0+10*y0d, y0, type="l", lwd=2, col=2)
abline(b0, b1, col=3)
## second plot
y <- b0 + b1*x + rnorm(200, sd=20)
y0a <- qnorm(0.001, mean=b0+b1*x0, sd=20)
y0b <- qnorm(0.999, mean=b0+b1*x0, sd=20)
y0 <- seq(y0a, y0b, length=100)
y0d <- dnorm(y0, mean=b0+b1*x0, sd=20)
plot(x, y, ylim=c(0,250), cex=0.5, 
     main="large dispersion", ylab="Y", xlab="X")
abline(b0, b1, col=3)
lines(x0+10*y0d, y0, type="l", lwd=2, col=2)
## dev.off()





#######################################
## simulated confidence intervals
#######################################

# pdf("confidence_interval_draws.pdf", width=4.5, height=6.25)
par(mar=c(3,1,1,1))
set.seed(14) 	#the seed is where the random number generator starts, so setting the seed will make the same numbers every time.
#I manually checked until I found one that gave 94. Also found that seed=3 gives 95, seed=8 gives 96.
x <- runif(20)-1/2
y <- 2 - 3*x + matrix(rnorm(length(x)*100), nrow=length(x))
fit <- lm(y ~ x)
upper <- coef(fit)[2,]  +  1.96 * sqrt(apply(fit$residuals^2,2,mean)*length(x)/(length(x)-2) )
lower <- coef(fit)[2,]  -  1.96 * sqrt(apply(fit$residuals^2,2,mean)*length(x)/(length(x)-2) )
colors.vector <- 1*(!(lower < -3 & -3 < upper))+1
plot(x, ylim=c(1,100), xlim=c(min(c(upper,lower)), max(c(upper,lower)) ) , col=0, yaxt="n", xlab="", xaxt="n", ylab="")
axis(side=1, at=-3, labels=expression("True" ~ beta[1]), cex.axis=1.5)
abline(v=-3, col="blue", lty=2)
segments(x0=lower, x1=upper, y0 = c(1:100), col=colors.vector, lwd=1.75)
# dev.off()




########################
## p value picture
########################

# pdf("pvalue.pdf", width=7, height=4)
par(mar=c(1,1,1,1))
#draw the normal curve
curve(dnorm(x, mean=0, sd=1), from=-3.25, to=3.25, add=FALSE, col="black", lwd=2, ylab="", xlab="", xaxt="n", yaxt="n")
#no axis labels this time, for more space
#add lines and labels for the quantiles, NOTE: not 95% quantiles this time, to give more room in the plot
segments(x0=c(qnorm(0.05), qnorm(0.95)), y0=0, y1=0.375, lty=2, lwd=1.5, col="red")
text(x=c(qnorm(0.05), qnorm(0.95)), y=0.39, labels=c(expression(Z[textstyle(alpha/2)]),expression(Z[textstyle(1 - alpha/2)])), cex=1.5)
#add line segments for the hypothetical test statistic
segments(x0=c(qnorm(0.015), qnorm(0.985)), y0=0, y1=0.25, lty=2, lwd=1.5, col="blue")
text(x=c(qnorm(0.015), qnorm(0.985)), y=0.265, labels=c(expression(paste("-|",z[beta[j]],"|",sep="")),expression(paste("|",z[beta[j]],"|",sep=""))), cex=1.5)
#label the areas under the curve
text(x=0, y=0.2, labels=expression(1-alpha), cex=1.75)
# text(x=-3, y=0.075, labels="p/2", cex=1.5)
# text(x=3, y=0.075, labels="p/2", cex=1.5)
text(x=-3, y=0.075, labels="p/2", cex=1.5)
text(x=3, y=0.075, labels="p/2", cex=1.5)
arrows(x0=c(-2.95, 2.95), y0=0.065, x1=c(-2.75, 2.75), y1=0.025, length=0.1, angle=30, col=1, lty=1, lwd=2, code=2)
#fill in the tails
x.points <- seq(from=-3.25, to=qnorm(0.05), by=0.01)
polygon(x=c(-3.25, x.points, qnorm(0.05)), y=c(0,dnorm(x.points),0), col=NA, angle=-50, density=11, border=NA)
polygon(x=c(3.25, -x.points, qnorm(0.95)), y=c(0,dnorm(-x.points),0), col=NA, angle=-50, density=11, border=NA)
x.points <- seq(from=-3.25, to=qnorm(0.015), by=0.01)
polygon(x=c(-3.25, x.points, qnorm(0.015)), y=c(0,dnorm(x.points),0), col=NA, angle=50, density=11, border=NA)
polygon(x=c(3.25, -x.points, qnorm(0.985)), y=c(0,dnorm(-x.points),0), col=NA, angle=50, density=11, border=NA)
#add a legend for the areas
polygon(x=c(-1,-1,-2/3,-2/3), y=c(0.05,0.08,0.08,0.05), col=NA, angle=-50, density=11, border=NA)
polygon(x=c(-1,-1,-2/3,-2/3), y=c(0.01,0.04,0.04,0.01), col=NA, angle=50, density=11, border=NA)
polygon(x=c(-1,-1,-2/3,-2/3), y=c(0.01,0.04,0.04,0.01), col=NA, angle=-50, density=11, border=NA)
text(x=-2/3, y=c(0.065,0.025), pos=4, offset=0.5, labels=c(expression("Level" ~ alpha), "p-value" ), cex=1.25)
# dev.off()




###################################################################
## Telemarketing: "calls per day" vs "length of employement".
## Fit a model with Y=calls vs. X=months
###################################################################

attach(telemkt <- read.csv("telemarketing.csv"))
tele1 <- lm(calls~months) 
xgrid <- data.frame(months = 10:30)
## pdf("tele1.pdf", width=7, height=4)
par(mfrow=c(1,2))
plot(months, calls, pch=20, col=4)
lines(xgrid$months, predict(tele1, newdata=xgrid))
e <- calls - tele1$fitted.values
plot(months, e, pch=20, col=4, ylab="residuals")
abline(h=0, lty=2)
## dev.off()

## testing for a quadratic term
months2 <- months^2
tele2 <- lm(calls~ months + months2)
summary(tele2)

## plotting the quadratic fit
xgrid <- data.frame(months=10:30, months2=(10:30)^2)
## pdf("tele2.pdf", width=7, height=4)
par(mfrow=c(1,2))
plot(months, calls, pch=20, col=4)
lines(xgrid$months, predict(tele2, newdata=xgrid))
plot(months, rstudent(tele2), pch=20, col=4)
abline(h=0, lty=2)
## dev.off()

## Not in slides, show stupidity of adding more polynomials
X <- rep(1, length.out=length(months))
months.list <- min(months):max(months)
months.list <- seq(from=min(months), to=max(months), length.out=200)
xgrid <- data.frame(months=rep(1, length.out=length(months.list)))
varnames <- "months"
i <- 0
## while the user wishes to continue
while(TRUE) {
    i <- i+1
    X <- cbind(X, months^(i))
    xgrid <- cbind(xgrid, (months.list)^(i))
    varnames <- c(varnames,paste("months",i,sep=""))
    colnames(X) <- varnames
    colnames(xgrid) <- varnames 
    
    
    XY <- data.frame(calls=calls, X)
    fit <- lm(calls~.-1,data=XY)
    
    plot(months, calls, col=2, pch=19)
    lines(months.list, predict(fit, newdata=xgrid))
    legend("topleft", col=c(1), lty=1, legend=c(round(summary(lm(calls~.-months,data=XY))$r.squared,3)))
    #can't use the object "fit" here to get the R-squared, because fit uses a manual intercept, that is the first variable of X is the intercept. R does funny things when subtracting the intercept. See https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-does-summary_0028_0029-report-strange-results-for-the-R_005e2-estimate-when-I-fit-a-linear-model-with-no-intercept_003f
    
    if(i==length(calls)) break
    if(readline(paste("Press ENTER to fit the model with m = ",i+1,", q to stop:", sep="")) == "q") break
    
}


##################
## Sales data
##################
salesdata <- read.csv("sales.csv")
attach(salesdata)
salesMLR <- lm(Sales ~ P1 + P2)
salesMLR

## Get the errors from the summary function
summary(salesMLR)

# Or do it manually for fun!
X <- cbind(1, P1, P2) 
# covariance of b =  s^2 (X'X)^-1 :
cov.b <- summary(salesMLR)$sigma^2*solve(t(X)%*%X)
print(cov.b)
# the standard errors of the b's (match it to summary):
se.b <- sqrt(diag(cov.b))
se.b



##################################################
## Pickup data - dummy variables and interactions
##################################################

pickup <- read.csv("pickup.csv")
pickup$miles <- pickup$miles/10000
attach(pickup)

c(mean(price[make=="Dodge"]), mean(price[make=="Ford"]), mean(price[make=="GMC"]))

## pdf("pickup_boxplot.pdf", height=5, width=5)
par(mar=c(5,4,1,2))
boxplot(price ~ make, ylab="price", cex.axis=1.5, cex.lab=1.5)
## dev.off()

summary(trucklm1 <- lm(price ~ make, data=pickup))

trucklm2 <- lm(price ~ make + miles, data=pickup)
summary(trucklm2)

## pdf("pickup_dummies.pdf", width=5, height=4)
par(mar=c(4,4,1,1))
plot(miles, price, pch=20, col=make, xlab="miles (10k)", ylab="price ($)")
abline(a=coef(trucklm2)[1],b=coef(trucklm2)[4],col=1)
abline(a=(coef(trucklm2)[1]+coef(trucklm2)[2]),b=coef(trucklm2)[4],col=2)
abline(a=(coef(trucklm2)[1]+coef(trucklm2)[3]),b=coef(trucklm2)[4],col=3)
#another way to get the line for just GMC trucks
# points(miles[make=="GMC"][order(miles[make=="GMC"])],trucklm2.new$fitted[make=="GMC"][order(miles[make=="GMC"])], type="l")
legend("topright", levels(pickup$make), fill=1:3)
## dev.off()


## Now interactions for different slopes
trucklm3 <- lm(price ~ make*miles)

## pdf("pickup_interactions.pdf", width=5, height=4)
par(mar=c(4,4,1,1))
plot(miles, price, pch=20, col=make, xlab="miles (10k)", ylab="price ($)")
abline(a=coef(trucklm3)[1],b=coef(trucklm3)[4],col=1)
abline(a=(coef(trucklm3)[1]+coef(trucklm3)[2]), b=(coef(trucklm3)[4]+coef(trucklm3)[5]),col=2)
abline(a=(coef(trucklm3)[1]+coef(trucklm3)[3]), b=(coef(trucklm3)[4]+coef(trucklm3)[6]),col=3)
#another way to get the line for just GMC trucks
# points(miles[make=="GMC"][order(miles[make=="GMC"])],trucklm3.new$fitted[make=="GMC"][order(miles[make=="GMC"])], type="l")
legend("topright", levels(make), fill=1:3)
## dev.off()


summary(trucklm3)
print(summary(trucklm3), digits=2)

c(coef(trucklm3)[1], coef(trucklm3)[4]) ##(b_0,b_1) Dodge
c((coef(trucklm3)[1]+coef(trucklm3)[2]), ## b_0 Ford
  (coef(trucklm3)[4]+coef(trucklm3)[5])) ## b_1 Ford
c((coef(trucklm3)[1]+coef(trucklm3)[3]), ## b_0 GMC
  (coef(trucklm3)[4]+coef(trucklm3)[6])) ## b_1 GMC


price.Ford <- price[make=="Ford"]
miles.Ford <- miles[make=="Ford"]
summary(lm(price.Ford ~ miles.Ford))


## Once we add another variable to the model, without a make interaction, 
## it's different than running separate regressions.
## Not in slides
trucklm4 <- lm(price ~ make*miles + year)
summary(trucklm4)
c((coef(trucklm4)[1]+coef(trucklm4)[2]), ## b_0 Ford
  (coef(trucklm4)[4]+coef(trucklm4)[5])) ## b_{miles} Ford
year.Ford <- miles[make=="Ford"]
summary(lm(price.Ford ~ miles.Ford + year.Ford))




####################
## GPA/Score data
####################

grades <- read.csv("grades.csv")
attach(grades)
summary(grades) 
summary(lm(MBAGPA ~ BachGPA))

summary(lm(MBAGPA ~ BachGPA*Age - Age))
## alternatively
summary(lm(MBAGPA ~ BachGPA + BachGPA:Age))

## With the main effect for age
summary(lm(MBAGPA ~ BachGPA*Age))




#########################################
## census data
#########################################
## only include folks working more than 500 hours AND
## earning more than $5000 AND less than age 60

census <- read.csv("census2000.csv")
workers <- (census$hours > 500)&(census$income > 5000)&(census$age < 60) 
log.WR <- log(census$income/census$hours)[workers]
age <- census$age[workers]
sex <- census$sex[workers]

## pdf("incbox.pdf", width=8, height=4)
par(mfrow=c(1,2))
boxplot(log.WR[sex=="M"] ~ age[sex=="M"], col=5, main="Male Income Curve",
        xlab="age", ylab="log wage rate", ylim=c(0,6))
boxplot(log.WR[sex=="F"] ~ age[sex=="F"], col=6, main="Female Income Curve",
        xlab="age", ylab="log wage rate", ylim=c(0,6))
## dev.off()

## tapply gets the mean wage at each age level
men <- sex == "M"
malemean <- tapply(log.WR[men], age[men], mean) 
femalemean <- tapply(log.WR[!men], age[!men], mean)
## pdf("inccurve.pdf", width=7, height=4.5)
plot(18:59, malemean, type="l", lwd=2, col=4, xlab="age",
     ylab="mean log wage rate", main="", xlim=c(19,60), ylim=c(1.8,3))
lines(18:59, femalemean, lwd=2, col=6)
text(x = rep(60,2), y = c(malemean[42],femalemean[42]),
     labels=c("M","F"), col=c(4,6))
## dev.off()

## 
wagereg1 <- lm(log.WR ~ age)
summary(wagereg1)
grid <- 18:59
## pdf("inc1.pdf", width=7, height=4.5)
plot(grid, wagereg1$coef[1] + wagereg1$coef[2]*grid, type="l", lwd=2,
     main="", xlab="age", ylab="predicted log wagerate") 
## dev.off()

##
wagereg2 <- lm(log.WR ~ age + sex)
summary(wagereg2)
## pdf("inc2.pdf", width=7, height=4.5)
plot(grid, wagereg2$coef[1] + wagereg2$coef[2]*grid +
         wagereg2$coef[3], type="l", lwd=2, col=4,
     main="", xlab="age", ylab="predicted log wagerate", ylim=c(2,3.1)) 
lines(grid, wagereg2$coef[1] + wagereg2$coef[2]*grid, lwd=2, col=6)
legend("topleft", col=c(4,6), lwd=4, legend=c("M","F"), bty="n")
## dev.off()

##
wagereg3 <- lm(log.WR ~ age*sex)
summary(wagereg3)
## pdf("inc3.pdf", width=7, height=4.5)
plot(grid, wagereg3$coef[1] + (wagereg3$coef[2]+wagereg3$coef[4])*grid +
         wagereg3$coef[3], type="l", lwd=2, col=4, main="", xlab="age",
     ylab="predicted log wagerate", ylim=c(2.2,3.2)) 
lines(grid, wagereg3$coef[1] + wagereg3$coef[2]*grid, lwd=2, col=6)
legend("topleft", col=c(4,6), lwd=4, legend=c("M","F"), bty="n")
## dev.off()

##
age2 <- age*age
wagereg4 <- lm(log.WR ~ age*sex + age2)
summary(wagereg4)
## pdf("inc4.pdf", width=7, height=4.5)
plot(grid, wagereg4$coef[1] + (wagereg4$coef[2]+wagereg4$coef[5])*grid +
         wagereg4$coef[3] + wagereg4$coef[4]*grid^2 ,
     type="l", lwd=2, col=4, main="", xlab="age",
     ylab="predicted log wagerate", ylim=c(2,3)) 
lines(grid, wagereg4$coef[1] + wagereg4$coef[2]*grid +
          wagereg4$coef[4]*grid^2, lwd=2, col=6)
legend("topleft", col=c(4,6), lwd=4, legend=c("M","F"), bty="n")
## dev.off()

##
wagereg5 <- lm(log.WR ~ age*sex + age2*sex)
summary(wagereg5)
## pdf("inc5.pdf", width=7, height=4.5)
plot(grid, wagereg5$coef[1] + (wagereg5$coef[2]+wagereg5$coef[5])*grid +
         wagereg5$coef[3] + (wagereg5$coef[4]+wagereg5$coef[6])*grid^2 ,
     type="l", lwd=2, col=4, main="", xlab="age", ylab="log wagerate", ylim=c(2,3)) 
lines(grid, wagereg5$coef[1] + wagereg5$coef[2]*grid +
          wagereg5$coef[4]*grid^2, lwd=2, col=6)
legend("topleft", col=c(4,6), lwd=2, legend=c("M fitted","F fitted"), bty="n")
lines(grid, malemean, col=4, lty=2)
lines(grid, femalemean, col=6, lty=2)
legend("bottomright", col=c(4,6), lwd=2, lty=2,
       legend=c("M data mean","F data mean"), bty="n")
## dev.off()



## 
edu <- census$education[workers]
summary(edureg <- lm(log.WR ~ edu*age))
summary(edureg2 <- lm(log.WR ~ edu*age - edu))
summary(edureg3 <- lm(log.WR ~ edu*age - age))



#################
## NBA
#################

nba = read.csv("nba.csv")

EY <- aggregate(weight~height, nba, FUN='mean')
plot(weight~height,data=nba,pch=20,cex=0.5,bty='n',xlab='Height in inches',ylab='Weight in lbs')
points(EY$height,EY$weight,col='red',pch=20,cex=2)

plot(weight~height,data=nba,pch=20,cex=0.5,bty='n',xlab='Height in inches',ylab='Weight in lbs')
fit <- lm(weight~height,data=nba)
abline(fit,col='red',lwd=3)
## alternative way to get the line
## abline(a=fit$coefficients[1],b=fit$coefficients[2],col='red',lwd=3)

plot(fit$residuals, pch=20, cex=0.5, ylab="residuals")
abline(h=0, col="red")

plot(rstandard(fit), pch=20, cex=0.5, ylab="std. residuals")
abline(h=0, col="red")

outlier = abs(rstandard(fit)) > 3
nba[outlier, ]

plot(weight~height,data=nba,pch=20,cex=0.5,bty='n',xlab='Height in inches',ylab='Weight in lbs')
fit <- lm(weight~height,data=nba)
abline(fit,col='red',lwd=3)
points(nba$height[outlier], nba$weight[outlier], col="red", pch=25,cex=0.7)

summary(fit)
anova(fit)

new_height = data.frame(height=c(75, 81,85))
predict.lm(fit, newdata=new_height, interval="pred")
