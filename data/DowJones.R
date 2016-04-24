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

