###############################
#  41000: R scripts
###############################

# install and update packages needed for the course
# the first time you run this command, it will take a few minutes
source('https://raw.githubusercontent.com/mlakolar/BUS41000/master/BUS41000.packages.R')

############################### 
#  Week 4
###############################

# set working directory
# you should change the folder to a location
# on your own computer
setwd("/home/mkolar/Downloads/")

############################### 
# Fraud data
###############################


# download data to the current working folder
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/fraud.csv", 
              destfile="fraud.csv")


# there are two header lines
# in order to deal with this, we follow 
# http://stackoverflow.com/questions/17797840/reading-two-line-headers-in-r
header = scan("fraud.csv", nlines = 1, what = character(), sep=",")
header2 = scan("fraud.csv", skip = 1, nlines = 1, what = character(), sep=",")

fraud_df = read.csv("fraud.csv", skip = 2, header = FALSE, row.names = 1)
names(fraud_df) = c("Credit History", "Guarantor CoApplicant","Accommodation","Fraud")

head(fraud_df)

table(fraud_df$`Fraud`, dnn = names(fraud_df)[4], deparse.level = 1)
table(fraud_df$`Fraud`, fraud_df$`Credit History`, dnn = names(fraud_df)[c(1,4)], deparse.level = 1)
table(fraud_df$`Fraud`, fraud_df$`Guarantor CoApplicant`, dnn = names(fraud_df)[c(2,4)], deparse.level = 1)
table(fraud_df$`Fraud`, fraud_df$`Accommodation`, dnn = names(fraud_df)[c(3,4)], deparse.level = 1)


