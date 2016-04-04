###############################
#  41000: R scripts
###############################

# install and update packages needed for the course
# the first time you run this command, it will take a few minutes
source('https://raw.githubusercontent.com/mlakolar/BUS41000/master/BUS41000.packages.R')

############################### 
#  Week 3
###############################

# set working directory
# you should change the folder to a location
# on your own computer
setwd("/home/mkolar/Downloads/")

############################### 
#   Tabloid -- Response phat
###############################


# download data to the current working folder
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/tabloid_response_phat.csv", 
              destfile="tabloid_response_phat.csv")

TMdat = read.csv("tabloid_response_phat.csv")
head(TMdat)
