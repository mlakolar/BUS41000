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
