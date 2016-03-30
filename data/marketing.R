############################### 
#  Marketing data
###############################

# download data to the current working folder
# recall: you can check what is the current working directory by typing getwd()
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/marketing.csv",
              destfile="marketing.csv")

# read the data from the file we just downloaded 
marketing_df = read.csv("marketing.csv")

# look at data
dim(marketing_df)
nrow(marketing_df)
ncol(marketing_df)

head(marketing_df)       
tail(marketing_df)
marketing_df 
head(marketing_df[,1:12])   # look at only first 12 columns

names(marketing_df)         # obtain names of columns

marketing_df$age            # look at the age column
marketing_df[, "age"]

# use histogram to plot age of customeres
hist(marketing_df$age, xlab="Age", main = "Histogram of Age")

# annotate mean and median
abline(v = mean(marketing_df$age), col="blue", lty=2)
abline(v = median(marketing_df$age), col="red", lty=2)
legend(x = "topright", c("Mean", "Median"), col = c("blue", "red"), lty = c(2, 2))

# we can also calculate the mean and standard deviation
mean(marketing_df$age); sd(marketing_df$age)

# obtain summary statistics for age
summary(marketing_df$age)
quantile(marketing_df$age)

# how many people in each socio economic status
soc_table = table(marketing_df$soc)
soc_table
plot(soc_table, xlab='Social Category', ylab='Number respondents', main='British Marketing Survey')


# create a boxplot of age vs soc
boxplot(age~soc, marketing_df, main="Age vs Socio Economic status", ylab="age", xlab="soc")

# create a two way table to investigate relationship between discrete variables
table(marketing_df[,c("colas","simpsons")])
table(marketing_df[,c("soc","cigs")])   # Lower social grades seem to smoke more cigarettes.