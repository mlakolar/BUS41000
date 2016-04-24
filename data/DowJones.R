############################### 
#   Dow Jones Returns
###############################

# Monthly return data on country's broadband portfolios from Feb 1988 till Dec 1996.

# download data to the current working folder
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/DowJones.csv", 
              destfile="DowJones.csv")

dowJones_df = read.csv("DowJones.csv")
names(dowJones_df)
head(dowJones_df)
summary(dowJones_df)

plot(x=as.Date(dowJones_df$Date, format = "%m/%d/%Y"), 
     y=dowJones_df$Adj.Close, type="b", col="blue", cex=0.1,
     xlab="", ylab="Adjusted Close", main = "Dow Jones Industrial Average")
abline(v=as.Date("09/29/2008", format="%m/%d/%Y"), col="red", lty=2)
legend(x="topleft", legend=c("Sept. 29, 2008"), lty=2, col="red")

Xt = dowJones_df$Adj.Close
n = length(Xt)
Rt = log(Xt[-1]/Xt[-n])
Rt_mean = mean(Rt)
Rt_var = var(Rt)
hist(Rt, breaks = 30, 
     xlim = c(-0.12, 0.12), ylim=c(0,500),
     xlab ="Returns", ylab="Frequency", main="Dow Jones returns")
abline(v=mean(Rt), col="red", lty=2)

plot(x=as.Date(dowJones_df$Date[-1], format = "%m/%d/%Y"), 
     y=Rt, type="b", col="blue", cex=0.3,
     xlab="", ylab="Adjusted Close", main = "Dow Jones Industrial Average")

hist(Rt, breaks = 30, probability=T,
     xlim = c(-0.12, 0.12), ylim=c(0,50),
     xlab ="Returns", ylab="", main="Dow Jones returns")
xarr = seq(-0.12,0.12,length.out=1000)
pdfarr = dnorm(xarr, mean=Rt_mean, sd=sqrt(Rt_var))
lines(xarr, pdfarr, col="red", lwd=2)

Rt_mean
Rt_var
