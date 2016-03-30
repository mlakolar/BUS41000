################################ 
#   Country Monthly Returns -- 2
################################

# Monthly return data on country's broadband portfolios from Jan 1996 till Aug 2014.

# set working directory
# you should change the folder to a location
# on your own computer
setwd("/home/mkolar/Downloads/")

# download data to the current working folder
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/CountryMonthlyReturns2.csv", 
              destfile="CountryMonthlyReturns2.csv")

countryReturn_df = read.csv("CountryMonthlyReturns2.csv")

# plot histrograms
hist(countryReturn_df$Japan, 
     breaks = 10,                                                       # this parameter controls number of bins
     xlab = "returns", ylab = "Frequency", main="Japanese returns")

hist(countryReturn_df$Canada, 
     breaks = 10, 
     xlab = "returns", ylab = "Frequency", main="Canadian returns")

# to save image, click on export and "Save as Image"

countryReturn_df$Date

# plot time series plots
plot(x=as.Date(paste0(countryReturn_df$Date, "-01"), format = "%m/%d/%Y"), 
     y=countryReturn_df$Canada, type="l", col="blue", 
     xlab="", ylab="Canadian returns", main = "")
abline(h=0)

plot(x=as.Date(paste0(countryReturn_df$Date, "-01"), format = "%m/%d/%Y"), 
     y=countryReturn_df$Japan, type="l", col="blue", 
     xlab="", ylab="Japanese returns", main = "")
abline(h=0)

# Let us compare the Canadian and Japanese returns
mean(countryReturn_df$Canada); sd(countryReturn_df$Canada)
mean(countryReturn_df$Japan); sd(countryReturn_df$Japan)

# scatter plot
plot(x=countryReturn_df$Japan, y=countryReturn_df$Canada, 
     type="p", col="black",                                
     cex = 0.5,                                  # controls size of points
     xlab="Canadian returns", ylab="Japanese returns", main = "Japanese vs Candaian returns")

