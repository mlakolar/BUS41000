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
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/tabloid.csv", 
              destfile="tabloid.csv")
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/tabloid_test.csv", 
              destfile="tabloid_test.csv")

# this is the data the PA team is using to build a model 
# this is a historic data
tabloid_df = read.csv("tabloid.csv")
head(tabloid_df)

# we fit a logistic regression
# you will learn more about this tool later in the class
lrm = glm(purchase~., family=binomial, data=tabloid_df)
summary(lrm)

# this is a data we use for new customers 
# we will use this to evaluate the model
tabloid_test_df = read.csv("tabloid_test.csv")
ProbRespond = predict(lrm, newdata=tabloid_test_df, type="response")
head( cbind(ProbRespond, tabloid_test_df[,2:5]) )

# as.factor makes a categorical variable
response = as.factor(tabloid_test_df[,1])
dotarget = as.factor(ProbRespond>.02)

# name the two possible outcomes for target
levels(response) = c("NOBUY", "BUY")     
levels(dotarget) = c("NO","YES")           

head(cbind.data.frame(dotarget, response, ProbRespond))

table(response)
table(dotarget)
table(dotarget, response)

par(mfrow=c(1,2))
hist(tabloid_df[tabloid_df$purchase==0, "nTab"], breaks=40, col="red", 
     main="nonresponders", xlab="nTab", xlim=c(0,85))
hist(tabloid_df[tabloid_df$purchase==1, "nTab"], breaks=40, col="blue", 
     main="responders", xlab="nTab", xlim=c(0,85))
dev.off()


