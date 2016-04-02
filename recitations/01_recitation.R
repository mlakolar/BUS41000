## R Cyntax ========================================
# expressions
1 + 1
"Have a good Saturday!"
2*3

# logical values
1 < 2
8 - 4 == 3
T == TRUE
F == FALSE
true
TRUE

# variables
x = 22
x/2
x = "Saturday"
x = TRUE

# functions
sum(1,3,5)
help(sum)
example(sum)
rep(2016,times=3)
sqrt(16)

## Vectors =========================================
c(1,2,3)
c("a","b","c")
c(1,TRUE,"one") # not legitimate, but see what happens

# sequence vectors
1:5
seq(1,5)
seq(1,5,0.5)
5:1

# vector access
sentence = c("walk","on","the","moon")
sentence[4]
sentence[1] # the index starting at 1, not 0
sentence[5] = "with my dog"
sentence[4] = "Mars"
sentence[4:5]
sentence[c(1,5)]
sentence[4:5] = c("with a", "pooping dog")

# vector names
rank = 1:3
names(rank) = c("first","second","third")
rank
rank["first"]
rank["third"] = 4
rank

# plotting one vector
barplot(rank)
barplot(1:100)

# vector math
rank + 1
rank/2
rank*2
rank + c(1,2,3)
rank - c(1,2,3)
rank == c(1,2,4)
sin(rank)
sqrt(rank)

# scatter plots
x = seq(1,20,0.1)
y = sin(x)
plot(x,y)
values = -10:10
absolutes = abs(values)
plot(values,absolutes)

# NA values
a = c(1,2,NA,4,5)
sum(a)
help(sum)
sum(a, na.rm = TRUE)

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
persp(elevation, expand = 0.2)
contour(volcano)
print(volcano)
persp(volcano, expand = 0.2)
image(volcano) # heat map

# files
getwd()
setwd("D:/Ph.D. UChicago/TAs/BUS 41000_16S_MK/Recitation 1")
getwd()
list.files()
source("hw01_starter.R")
