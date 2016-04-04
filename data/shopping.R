############################### 
#   Shopping attitudes 
###############################


# download data to the current working folder
download.file("https://raw.githubusercontent.com/mlakolar/BUS41000/master/data/shopping.csv", destfile="shopping.csv")

(shopping_df = read.csv("shopping.csv", row.names=1))

# plot data using first two columns
plot(x=shopping_df[,1], y=shopping_df[,2], main="Shopping attitudes",
     xlab="Shopping is fun", ylab="Shopping is bad for your budget")

# setting the seed will allow us to reproduce results
# otherwise the result will change every time
set.seed(1)                
# cluster into three groups
grpShopper = kmeans(shopping_df, centers=3, nstart=100)

plot(x=shopping_df[,1], y=shopping_df[,2], main="Three clusters",
     col=grpShopper$cluster+1, 
     xlab="Shopping is fun", ylab="Shopping is bad for your budget")
points(x=grpShopper$centers[,1], y=grpShopper$centers[,2], col=c(2,3,4), pch=3)

# investigate centers
t( grpShopper$centers )

# visualize kmeans
library(animation)

## set larger 'interval' if the speed is too fast
oopt = ani.options(interval = 2)
kmeans.ani(shopping_df, centers=3)
