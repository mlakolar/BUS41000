############################### 
#   MBA Beer Consumption
###############################

# download data to the current working folder
download.file("https://raw.githubusercontent.com/mlakolar/BUS41000/master/data/BeerConsumptionMBA.csv", 
              destfile="BeerConsumptionMBA.csv")

beer_df = read.csv("BeerConsumptionMBA.csv")

head(beer_df)
summary(beer_df)

plot(beer_df$weight, beer_df$nbeer, main="MBA Beer Consumption Survey", xlab="weight", ylab="number of beers")

# find the outlier
iOutlier = which(beer_df$nbeer == max(beer_df$nbeer))
beer_df[iOutlier, ]
