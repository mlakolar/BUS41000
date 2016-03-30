############################### 
#  Bank Interarrival Times
###############################

# download data to the current working folder
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/bank.csv", destfile="bank.csv")

bank_df = read.csv("bank.csv")
# breaks = 25 tells R to create a histogram with 25 breaks
hist(bank_df[,1], breaks = 25, xlab='Minutes', main='Interarrival Times for 300 Bank Customers')

# annotate mean and median
abline(v = mean(bank_df[,1]), col="blue", lty=2)
abline(v = median(bank_df[,1]), col="red", lty=2)
legend(x = "topright", c("Mean", "Median"), col = c("blue", "red"), lty = c(2, 2))