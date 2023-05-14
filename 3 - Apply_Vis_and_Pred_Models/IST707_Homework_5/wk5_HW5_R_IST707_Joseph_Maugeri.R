### FILE INFO
###############################################################################
# Author: Joseph R Maugeri
# Class: IST 707
# Week: 4
# Activity: Homework 5 
# Purpose: 
'This file uses train and test datasets partitioned from the famous /n
        titanic dataset. It includes attributes about passengers, and /n
        their survival as 0 (deceased) or 1 (survived)'
#
### Library Packages
###############################################################################
# ONCE install.packages('quanteda')
library(quanteda)
## ONCE: install.packages("tm")
library(tm) # Needed 
# ONCE install.packages("textstem")
library(textstem)  ## Needed for lemmatize_strings
# ONCE install.packages("stringr")
library(stringr)
# ONCE install.packages("stringi")
library(stringi)
## ONCE: install.packages("slam")
library(slam)
##ONCE: install.packages('proxy')
library(proxy)
##ONCE: install.packages('Matrix')
library(Matrix) # Used to create document - term matrices
##ONCE: install.packages('tidytext')
library(tidytext) # convert DTM to DF
# ONCE install.packages("plyr")
library(plyr) ## for adply
# ONCE install.packages("ggplot2")
library(ggplot2)
# ONCE install.packages("wordcloud")
library(wordcloud)
# ONCE install.packages("rpart")
library(rpart)
# ONCE install.packages("rattle")
library(rattle)
# ONCE install.packages("rpart.plot")
library(rpart.plot)
# ONCE install.packages("RColorBrewer")
library(RColorBrewer)
# ONCE install.packages("Cairo")
library(Cairo)
# ONCE install.packages("network")
library(network)
## ONCE: install.packages("arules")
library(arules)
##ONCE: install.packages('proxy')
library(proxy)
## ONCE:  install.packages("stringr")
library(stringr)
## ONCE: install.packages("textmineR")
library(textmineR)
## ONCE: install.packages("igraph")
library(igraph)
## ONCE: install.packages("lsa")
library(lsa)

#
### Loading Corpus
###############################################################################
#
feds_corpus <- Corpus(DirSource('C:/Users/Maugeri/Desktop/IST707/fedPapers_txt'))
# The (getTransformations()) enclosed in parentheses will visualize the 
# available transformations through the tm package
(getTransformations()) 
# Let's also use () to visualize the corpus ( )
# This corpus includes many authors, some we won't be able to decipher given
# the sample size. 
(ndocs<-length(feds_corpus))

### Pre-processing of corpus
###############################################################################
# Looking at What was read in
(summary(feds_corpus)) 
## Transforming the content of the corpus
feds_corpus <- tm_map(feds_corpus, content_transformer(tolower))
feds_corpus <- tm_map(feds_corpus, removePunctuation)
# lemmatization is critical for conforming words to the base element using a dictionary
feds_corpus <- tm_map(feds_corpus, lemmatize_strings)

## Remove all Stop Words
feds_corpus <- tm_map(feds_corpus, removeWords, stopwords("english"))


feds_DTM <- DocumentTermMatrix(feds_corpus,
                               control = list(
                                 stopwords = TRUE, ## remove normal stopwords
                                 wordLengths=c(4, 10), ## get rid of words of len 3 or smaller or larger than 15
                                 removePunctuation = TRUE,
                                 removeNumbers = TRUE,
                                 tolower=TRUE,
                                 #stemming = TRUE,
                                 remove_separators = TRUE
                                 #stopwords = MyStopwords,
                                
                                 #removeWords(MyStopwords),
                                 #bounds = list(global = c(minTermFreq, maxTermFreq))
                               ))

### Converting DTM into Matrix, and Copying
###############################################################################
# Creating a matrix from the Document term matrix
feds_mat <- as.matrix(feds_DTM)
# Viewing the entire matrix
feds_mat[0:77,50:59]
# Creating a matrix of disputed papers
feds_mat_D <- feds_mat[0:11,]
# Creating a matrix of know authored papers ( Hamilton and Madison)
feds_mat_HM <- feds_mat[-0:-11,]

# Creating dataframes from the matrices
feds_DF <- as.data.frame(feds_mat)
feds_D_DF <- feds_DF[0:11,]
feds_HM_DF <- feds_DF[-0:-11,]

feds_DF[0:11,]
### Labeling and Creating Factors
###############################################################################
# start by making the columns factors
# Moving the index into the first column of DF
feds_HM_DF <- cbind(FILENAME = rownames(feds_HM_DF), feds_HM_DF)
# Renaming the rows to the number representative of position
rownames(feds_HM_DF) <- 1:nrow(feds_HM_DF)
# Storing the first column as a list 
feds_HM_DFFILENAME <- c(feds_HM_DF$FILENAME)
# subsetting list of first column into label of Hami / Madi
feds_HM_DFAuthors <- c(substr(feds_HM_DF$FILENAME, start = 0, stop = 4))
# Using the list of filenames created 
feds_HM_DF$FILENAME <- c(feds_HM_DFFILENAME)
feds_HM_DF$Authors <- c(feds_HM_DFAuthors)
# converting our two label columns into factors
feds_HM_DF$FILENAME <- as.factor(feds_HM_DF$FILENAME)
feds_HM_DF$Authors <- as.factor(feds_HM_DF$Authors)


# Doing the Same thing with the disputed set of papers (for later)
feds_D_DF <- cbind(FILENAME = rownames(feds_D_DF), feds_D_DF)
rownames(feds_D_DF) <- 1:nrow(feds_D_DF)
feds_D_DFFILENAME <- c(feds_D_DF$FILENAME)
feds_D_DFAuthors <- c(substr(feds_D_DF$FILENAME, start = 0, stop = 4))
feds_D_DF$FILENAME <- c(feds_D_DFFILENAME)
feds_D_DF$Authors <- c(feds_D_DFAuthors)
feds_D_DF$FILENAME <- as.factor(feds_D_DF$FILENAME)
feds_D_DF$Authors <- as.factor(feds_D_DF$Authors)

# Looking at the columns 
feds_D_DF[1:10,4598]
feds_HM_DF[1:10,4598]

feds_D_DF[1:10,1]
feds_HM_DF[1:10,1]

# Using sequencing to build the Testing and Training data to build the 
## initial model with.
###############################################################################

(every3_indexes <- seq(1,nrow(feds_HM_DF),3))
feds_HM_DF_Test <- feds_HM_DF[every3_indexes,]
feds_HM_DF_Train <- feds_HM_DF[-every3_indexes,]

feds_HM_DF_Train[1:5, (4598-5):4598]
feds_HM_DF_Test[1:5, (4598-5):4598]

feds_HM_DF_Train[,4598]
feds_HM_DF_Test[,4598]
# Removing labels from training data which, I don't need to drop the filename,
# We have authors of training stored, but we need to drop filename

#feds_HM_DF_Trainunlabel<-subset( feds_HM_DF_Train, select = -c(Authors))
#feds_HM_DF_Train <- feds_HM_DF_Train[,-1]
#head(feds_HM_DF_Trainunlabel)

# Storing the labels to apply back when predicting
feds_HM_ATestLabels <- c(feds_HM_DF_Test$Authors)
feds_HM_FTestLabels <- c(feds_HM_DF_Test$FILENAME)

# Removing labels from test data which I am definitely supposed to do
feds_HM_DF_Testunlabel<-subset( feds_HM_DF_Test, select = -c(Authors))
feds_HM_DF_Testunlabel<-feds_HM_DF_Testunlabel[,-1]
feds_HM_DF_Testunlabel[1:5,4596]
feds_HM_DF_Testunlabel[1:5, (4598-5):4596]

## OK - now we have labeled  and unlabeled record data
###############################################################################
# FINAL PREDICTION DATASETS
# Making the datasets to predict the disputed paper's authorship with 
Fknown <- feds_HM_DF
Fdisputed <- feds_D_DF

Fknown <- cbind(FILENAME = rownames(Fknown), Fknown)
rownames(Fknown) <- 1:nrow(Fknown)
feds_D_DFFILENAME <- c(Fknown$FILENAME)
feds_D_DFAuthors <- c(substr(Fknown$FILENAME, start = 0, stop = 4))
Fknown$FILENAME <- c(feds_D_DFFILENAME)
Fknown$Authors <- c(feds_D_DFAuthors)
Fknown$FILENAME <- as.factor(Fknown$FILENAME)
Fknown$Authors <- as.factor(Fknown$Authors)

# Removing labels from test
Fdisputed<-subset( Fdisputed, select = -c(Authors))
Fdisputed<-Fdisputed[,-1]
Fdisputed[1:5,4596]
Fdisputed[1:5, (4596-5):4596]


Fknown[1:5, (4598-5):4598]
Fdisputed[1:5, (4596-5):4596]

Fknown[,4598]
Fdisputed[,4598]

# Storing the labels to apply back when predicting
FdisputedALabels <- c(Fdisputed$Authors)
FdisputedFLabels <- c(Fdisputed$FILENAME)

# Removing labels from test data which I am definitely supposed to do
Fdisputedunlabel<-subset( Fdisputed, select = -c(Authors))
Fdisputedunlabel<-Fdisputedunlabel[,-1]
Fdisputedunlabel[1:5,4596]
Fdisputedunlabel[1:5, (4596-5):4596]
Fdisputedunlabel[,4596]


Fknown <- Fknown[,-1]

### WORDCLOUDS
###############################################################################
word.freq <- sort(rowSums(t(feds_mat)), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq*2, min.freq = 2,
          random.order = F, max.words=30)
word.freq <- sort(rowSums(t(feds_mat_D)), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq*2, min.freq = 2,
          random.order = F, max.words=30)
word.freq <- sort(rowSums(t(feds_mat_HM)), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq*2, min.freq = 2,
          random.order = F, max.words=30)

### Decision Trees model Build
###############################################################################
# Initial model building
fitR <- rpart(feds_HM_DF_Train$Authors ~ ., data = feds_HM_DF_Train, method="class")
summary(fitR)
## Predict the Test sets
predictedR <- predict(fitR,feds_HM_DF_Testunlabel, type="class")
## Confusion Matrix
table(predictedR,feds_HM_ATestLabels)
# visualize the results
fancyRpartPlot(fitR)

# 
### Final Model building Decision Trees 
###############################################################################
# we use the same steps as building the model when applying it
# ( believe this is where an error was encountered )
fitF <- rpart(Fknown$Authors ~ . , data = Fknown, method = 'class')
summary(fitF)

predictedF <- predict(fitF,Fdisputedunlabel, type="class")
table(predictedF,FdisputedALabels)


fancyRpartPlot(fitF)

