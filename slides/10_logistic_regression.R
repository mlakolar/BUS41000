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
##  - tabloid.csv
##  - tabloid_test.csv

# download data to the current working folder
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/tabloid.csv", destfile="tabloid.csv")
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/tabloid_test.csv", destfile="tabloid_test.csv")


#################
##  Default data 
#################

library(ISLR)
df = Default
df$Y = as.numeric(df$default)-1
head(df)
summary(df)

## fit LS regression line  --- it does not look good
par(mar=c(3,3,3,1), mgp=c(2,1,0))
plot(df$balance, df$Y, col=c("red","green")[df$Y+1], 
     xlab="balance", ylab="Y", ylim = c(-0.2, 1.2), cex = 0.1)
lsfit = lm(Y~balance, data=df) 
abline(lsfit)
summary(lsfit)


# sigmoid function
par(mar=c(3,3,3,1), mgp=c(2,1,0))
z = seq(-10, 10, length.out = 1000)
pz = exp(z) / (1+exp(z))
plot(0, 0.5, type="n", xlim=c(-8, 8), ylim=c(-0.11,1.14), 
     xlab="z=x'beta", ylab="p(y|x)", main="Logistic sigmoid function")
lines(z, pz, lwd=2, col="blue")
abline(h=0, lty=2, col="gray")
abline(h=1, lty=2, col="gray")
abline(v=0, lty=1, lwd=2, col="black")



##  illustrate logistic regression on a small data 
par(mar=c(3,3,3,1), mgp=c(2,1,0))
df = data.frame(x=c(-2, -1, -0.75, -0.5, 0.5, 0.75, 1, 2, 5), y=c(0,0,1,0,1,0,1,1,1))
plot(df, col=c("red","green")[df$y+1], ylim=c(-0.2,1.2), xlab="z")

o = glm(y~x, df, family="binomial")
summary(o)

z = seq(-10, 10, length.out = 1000)
pz = 1 / (1+exp(-(o$coefficients[1] + z*o$coefficients[2])))

lines(z, pz, lwd=2, col="blue")
abline(h=0.5, col="orange", lty=2)
abline(v=0, col="orange", lty=2) 


## let us look again the default data
df = Default
df$Y = as.numeric(df$default)-1
par(mfrow = c(1, 2), mar=c(3,3,3,1), mgp=c(2,1,0))

plot(balance ~ default, data=df, col=c("red", "green"))

plot(df$balance, df$Y, col=c("red","green")[df$Y+1], 
     xlab="balance", ylab="Y", ylim = c(-0.2, 1.2), cex = 0.1)
fit = glm(default~balance, df, family="binomial")
summary(fit)
o = fit

abline(v = -o$coefficients[1]/o$coefficients[2], col="orange", lty=2)
z = seq(min(df$balance), max(df$balance), length.out = 1000)
pz = 1 / (1+exp(-(o$coefficients[1] + z*o$coefficients[2])))
lines(z, pz, lwd=2, col="blue")


# prediction using logistic regression
xf = data.frame(balance=c(1500, 2000, 2500))
phat = predict(fit, newdata=xf, type="response")
cbind(xf, phat)

# more than one variable
par(mar=c(3,3,3,1), mgp=c(2,1,0))
plot(df$balance, df$income, col=c("red","green")[df$Y+1], pch=c("o","x")[df$Y+1],
     xlab="balance", ylab="income", cex = 0.5 )
o = glm(default~balance+income, data=df, family="binomial")
summary(o)
abline(a=-o$coefficients[1]/o$coefficients[3], b=-o$coefficients[2]/o$coefficients[3], 
       lwd=2, col="black")
legend("topright", 
       legend=c("default=0", "default=1", "p(y=1 | x) = 1/2"), 
       col=c("red","green","black"), pch=c("o","x", "-"))



## confounding 
summary(glm(default~student, data=df, family="binomial"))
summary(glm(default~balance+student, data=df, family="binomial"))
summary(glm(default~balance+income+student, data=df, family="binomial"))





##############################
#  tabloid
##############################


td = read.csv("tabloid.csv")
td$purchase = as.factor(td$purchase)
summary(td)


par(mfrow=c(2,2), mar=c(3,3,3,1), mgp=c(2,1,0)) 
plot(nTab~purchase,td,col=c("red", "blue"))
plot(moCbook~purchase,td,col=c("red", "blue"))
plot(iRecMer1~purchase,td,col=c("red", "blue"))
plot(llDol~purchase,td,col=c("red", "blue"))

dev.off()

lgfit = glm(purchase~nTab+moCbook+iRecMer1+llDol,td,family=binomial)
summary(lgfit)

par(mar=c(3,3,3,1), mgp=c(2,1,0)) 
phat = predict(lgfit, newdata=td, type="response")
plot(phat~td$purchase, col=c("red","blue"), 
     xlab="purchase", ylab="phat", ylim=c(0,1.05), cex.text=0.7)


td$phat = phat
sorted_phat = order(-phat)
td[sorted_phat[1:40], c("purchase", "phat")]


## plot lift curve
liftf = function(yl,phatl,dopl=TRUE) {
  if(is.factor(yl)) yl = as.numeric(yl)-1
  oo = order(-phatl)
  sy = cumsum(yl[oo])/sum(yl==1)
  if(dopl) {
    ii = (1:length(sy))/length(sy)
    plot(ii,sy,type='l',lwd=2,col='blue',xlab='% of observations',ylab='% of successes',cex.lab=2)
    abline(0,1,lty=2)
    lines(x=c(0,sum(yl)/length(yl)), y=c(0,1), lty=2)
    lines(x=c(sum(yl)/length(yl),1), y=c(1,1), lty=2)
  }
  return(sy)
}

par(mar=c(5,5,3,1))
tmp = liftf(td$purchase,phat)
abline(0, 0.)

