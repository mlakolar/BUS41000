## Matrices =================================
matrix(0,3,4)
matrix(1,5,5)
a = 1:12
print(a)
matrix(a,3,4)
dim(a) = c(3,4)
print(a)

# matrix access
a[2,3]
a[2,3] = 1
a[2,]
a[,3]
a[,2:4]

# plotting matrix
elevation = matrix(1,10,10)
elevation[4,6] = 0
contour(elevation)
persp(elevation)
persp(elevation, expand = 0.5)
contour(volcano)
print(volcano)
persp(volcano, expand = 0.3)
image(volcano) # heat map


## "for" loop ================================
a = matrix(0,1,12)
for (i in 1:12){
  a[i] = i
}

## Factors ===================================
# creating factors
chests = c('gold', 'silver', 'gems', 'gold', 'gems')
types =factor(chests)
print(chests)
print(types)
as.integer(types)
levels(types)

# plots with factors
weights = c(300, 200, 100, 250, 150)
prices  = c(9000, 5000, 12000, 7500, 18000)
plot(weights, prices)
plot(weights, prices, pch=as.integer(types))
legend("topright", levels(types), pch = 1:length(levels(types)))


## Data frames ===============================
treasure = data.frame(weights, prices, types)
print(treasure)

# data frame access
treasure[[2]]
treasure[["prices"]]
treasure$prices
treasure$types

# loading data frame
list.files()
# read.csv("studenthw.csv")


## Factors ===================================
# creating factors
chests = c('gold', 'silver', 'gems', 'gold', 'gems')
types =factor(chests)
print(chests)
print(types)
as.integer(types)
levels(types)

# plots with factors
weights = c(300, 200, 100, 250, 150)
prices  = c(9000, 5000, 12000, 7500, 18000)
plot(weights, prices)
plot(weights, prices, pch=as.integer(types))
legend("topright", levels(types), pch = 1:length(levels(types)))


## Run script ===============================
getwd()
setwd("")
getwd()
list.files()
source("hw02_starter.R")


## ggplots ==================================
install.packages("ggplot2")
library(ggplot2)
qplot(weights, prices, color = types)
