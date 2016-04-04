################################## 
#   Scotch
##################################

library(proxy)

download.file("https://raw.githubusercontent.com/mlakolar/BUS41000/master/data/scotch.csv", destfile="scotch.csv")

# there are two header lines
# in order to deal with this, we follow 
# http://stackoverflow.com/questions/17797840/reading-two-line-headers-in-r
header = scan("scotch.csv", nlines = 1, what = character(), sep=",")
header2 = scan("scotch.csv", skip = 1, nlines = 1, what = character(), sep=",")

scotch_df = read.csv("scotch.csv", skip = 2, header = FALSE, row.names = 1)
names(scotch_df) = paste0(header[-1], "/", header2[-1])

scotch_df = scotch_df[,1:68]                               # drop columns that do not have characteristics
head(scotch_df)

indA = which(rownames(scotch_df) == "Ardberg")             # find a row corresponding to ardbeg
# compute distance between "Ardbeg" and all other scotches using Jaccard distance
# see: https://en.wikipedia.org/wiki/Jaccard_index
distM = proxy::dist(scotch_df[indA, ], y=scotch_df, method="jaccard")     

# names of 5 most similar scotches (the distance of ardbeg to itself is 0)
rownames(scotch_df)[ order(distM)[1:6] ]         
# show distance
distM[ order(distM)[1:6] ]
