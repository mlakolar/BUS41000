################################## 
#   European Protein Consumption
##################################

# European Protein Consumption, in grams/person-day 

library(maptools)

download.file("https://raw.githubusercontent.com/mlakolar/BUS41000/master/data/protein.csv", destfile="protein.csv")

food_df = read.csv("protein.csv", row.names=1) # 1st column is country name
# scale the data so that every column has sample mean equal to zero and variance equal to 1
food_scaled = scale(food_df)  

plot(food_scaled[,"RedMeat"], food_scaled[,"WhiteMeat"], xlim=c(-2,2.75), 
     type="n", xlab="Red Meat", ylab="White Meat")
pointLabel(food_scaled[,"RedMeat"], food_scaled[,"WhiteMeat"],  
           labels=rownames(food_scaled))

## first, consider just Red and White meat clusters
grpMeat <- kmeans(food_scaled[,c("WhiteMeat","RedMeat")], centers=3, nstart=100)
grpMeat

plot(food_scaled[,"RedMeat"], food_scaled[,"WhiteMeat"], xlim=c(-2,2.75), 
     type="n", xlab="Red Meat", ylab="White Meat",
     main="3-means clustering on Red vs White meat consumption")
pointLabel(food_scaled[,"RedMeat"], food_scaled[,"WhiteMeat"],
           labels=rownames(food_scaled), col=grpMeat$cluster+1)

# which contries belong to which cluster
rownames(food_df)[grpMeat$cluster==1]
rownames(food_df)[grpMeat$cluster==2]
rownames(food_df)[grpMeat$cluster==3]

## same plot, but now with clustering on all protein groups
grpProtein <- kmeans(food_scaled, centers=7, nstart=100) ## change the number of centers to see what happens.
grpProtein

plot(food_scaled[,"RedMeat"], food_scaled[,"WhiteMeat"], xlim=c(-2,2.75), 
     type="n", xlab="Red Meat", ylab="White Meat",
     main="7-means clustering on all nine protein types")
text(food_scaled[,"RedMeat"], food_scaled[,"WhiteMeat"], labels=rownames(food_scaled), 
     col=grpMeat$cluster+1) ## col is all that differs from first plot

