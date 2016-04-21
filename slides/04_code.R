###############################
#  41000: R scripts
###############################

# install and update packages needed for the course
# the first time you run this command, it will take a few minutes
source('https://raw.githubusercontent.com/mlakolar/BUS41000/master/BUS41000.packages.R')

############################### 
#  Week 4
###############################

# set working directory
# you should change the folder to a location
# on your own computer
setwd("/home/mkolar/Downloads/")

############################### 
# Fraud data
###############################


# download data to the current working folder
download.file("https://github.com/mlakolar/BUS41000/raw/master/data/fraud.csv", 
              destfile="fraud.csv")


# there are two header lines
# in order to deal with this, we follow 
# http://stackoverflow.com/questions/17797840/reading-two-line-headers-in-r
header = scan("fraud.csv", nlines = 1, what = character(), sep=",")
header2 = scan("fraud.csv", skip = 1, nlines = 1, what = character(), sep=",")

fraud_df = read.csv("fraud.csv", skip = 2, header = FALSE, row.names = 1)
names(fraud_df) = c("Credit History", "Guarantor CoApplicant","Accommodation","Fraud")

head(fraud_df)

table(fraud_df$`Fraud`, dnn = names(fraud_df)[4], deparse.level = 1)
table(fraud_df$`Fraud`, fraud_df$`Credit History`, dnn = names(fraud_df)[c(1,4)], deparse.level = 1)
table(fraud_df$`Fraud`, fraud_df$`Guarantor CoApplicant`, dnn = names(fraud_df)[c(2,4)], deparse.level = 1)
table(fraud_df$`Fraud`, fraud_df$`Accommodation`, dnn = names(fraud_df)[c(3,4)], deparse.level = 1)



#########################################
# Sentiment Analysis
#########################################

# PART 1:
#
# In this part, I will preprocess data for you. Essentially, this is how we create 
# files sentiment_train.csv and sentiment_test.csv that you simply download in the second 
# part.
#
# This is more advanced part. You can simply skip over this and 
# go directly to PART 2 below


# download reviews 
# this is a big file and may take some time to retrieve
download.file("http://ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz", 
              destfile="aclImdb_v1.tar.gz")
# uncompress the file
untar("aclImdb_v1.tar.gz", compressed = T)


# these paths point to folders that have positive and negative reviews
path_to_neg_folder = "aclImdb/train/neg"
path_to_pos_folder = "aclImdb/train/pos"

# packages for text analysis
# install.packages("tm")
# install.packages("SnowballC")
library(tm)
library(SnowballC)


# In linguistics Corpus is a collection of documents
# In the tm package, corpus is a collection of 
# strings representing individual sources of text
nb_pos = Corpus(DirSource(path_to_pos_folder), readerControl = list(language="en"))
nb_neg = Corpus(DirSource(path_to_neg_folder), readerControl = list(language="en"))
# recursive parameter in the c() function used to merge the two corpora is needed
# to maintain the metadata information stored in the corpus objects
nb_all = c(nb_pos, nb_neg, recursive=T)

# see the metadata for the first review in our corpus
# observe id 
#       id           : 0_9.txt
# this is the name of the file
# each filename is of the form <counter>_<score>.txt
# scores in the range 7-10 are positive
# scores in the range 0-4 are negative
# we only have such polar reviews 
meta(nb_all[[1]])

# create vector of filenames
ids = sapply(1:length(nb_all), function(x) meta(nb_all[[x]], "id"))
head(ids)

# extract scores from the filenames using sub function
scores = as.numeric(sapply(ids, function(x) sub("[0-9]+_([0-9]+)\\.txt", "\\1", x)))
scores = factor(ifelse(scores>=7,"positive","negative"))
summary(scores)

# preprocessing steps 
nb_all = tm_map(nb_all, content_transformer(removeNumbers))
nb_all = tm_map(nb_all, content_transformer(removePunctuation))
nb_all = tm_map(nb_all, content_transformer(tolower))
nb_all = tm_map(nb_all, content_transformer(removeWords), stopwords("english"))
nb_all = tm_map(nb_all, content_transformer(stripWhitespace))

# create document term matrix
nb_dtm = DocumentTermMatrix(nb_all)
dim(nb_dtm)
nb_dtm

# remove infrequent items
# try repeating this with different sparsity
nb_dtm = removeSparseTerms(x=nb_dtm, sparse = 0.99)
dim(nb_dtm)
nb_dtm

# inspect first review
inspect(nb_dtm[1,]) 
terms = which( inspect(nb_dtm[1,]) != 0 ) # find terms in review 1
inspect( nb_dtm[1,terms] )

# convert all elements to binary
# The occurrence of the word fantastic tells us a lot 
# The fact that it occurs 5 times may not tell us much more
nb_dtm = weightBin(nb_dtm)

inspect( nb_dtm[1,terms] )

# split into train and test
nb_df = as.data.frame(as.matrix(nb_dtm))
set.seed(1)
nb_sampling_vector = sample(25000, 20000)
nb_df_train = nb_df[nb_sampling_vector,]
nb_df_test = nb_df[-nb_sampling_vector,]
scores_train = scores[nb_sampling_vector]
scores_test = scores[-nb_sampling_vector]

## Part 2
download.file("https://github.com/mlakolar/BUS41000/raw/master/files/NaiveBayes/naiveBayes.Rdata", 
              destfile="naiveBayes.Rdata")
load("naiveBayes.Rdata")

library(e1071)
nb_model = naiveBayes(nb_df_train, scores_train)

# compute training error
nb_train_predictions = predict(nb_model, nb_df_train) 
mean(nb_train_predictions == scores_train)
table(actual = scores_train, predictions = nb_train_predictions)

# compute test error
nb_test_predictions = predict(nb_model, nb_df_test)
mean(nb_test_predictions == scores_test)
table(actual = scores_test, predictions = nb_test_predictions)



