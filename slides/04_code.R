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
# 
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
names(fraud_df) = paste0(header[-1], "/", header2[-1])

head(fraud_df)
