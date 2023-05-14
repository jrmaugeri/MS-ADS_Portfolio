### FILE INFO
###############################################################################
# Author: Group 4 ( Joseph Maugeri, Yu Sheng Lu , Jonathan Xi)
# Class: IST 707
# Week: 10
# Activity: mushrooms.csv R data mining and classification
# Purpose: Final Submission of R Code tandem to paper report
#
### PACKAGES
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

# ONCE install.packages("RColorBrewer")
library(RColorBrewer)
# ONCE install.packages("Cairo")
library(Cairo)
# ONCE install.packages("network")
library(network)
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

# Libraries for Association Rule Mining
## ONCE: install.packages("arules")
library(arules)
## ONCE: install.packages("arulesViz")
library(arulesViz)
## ONCE: install.packages("datasets")
library(datasets)

# Library for randomForest
## ONCE: install.packages("randomForest")
library(randomForest)
## ONCE: install.packages("rpart.plot")
library(rpart.plot)


### Reading in the data
###############################################################################

setwd('C:/Users/Maugeri/Desktop/IST707')
mushrooms <- read.csv('mushrooms.csv', stringsAsFactors = TRUE)

### Exploring the Data
###############################################################################
# Using summary to get an overview
(summary(mushrooms))
# Using str() to investigate the size
(str(mushrooms))
# using sapply() with levels() to print all levels of the dataframe
M_Levels <- sapply(mushrooms,levels)
(M_Levels)
# Visualizations of Each Feature
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = class), fill = "blue") + scale_x_discrete(labels = c('edible','poisonous'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = cap.shape), fill = "blue") + scale_x_discrete(labels = c('bell','conical', 'flat', 'knobbed', 'sunken', 'convex'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = cap.surface), fill = "blue") + scale_x_discrete(labels = c('fibrous','grooves', 'smooth', 'scaly'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = cap.color), fill = "blue") + scale_x_discrete(labels = c('buff','cinnamon', 'red', 'gray', 'brown', 'pink', 'green', 'purple', 'white', 'yellow'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = bruises), fill = "blue") + scale_x_discrete(labels = c('no','bruises'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = odor), fill = "blue") + scale_x_discrete(labels = c('almond','creosote', 'foul', 'anise', 'musty', 'none', 'pungent', 'spicy','fishy'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = gill.attachment), fill = "blue") + scale_x_discrete(labels = c('attached','free'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = gill.spacing), fill = "blue") + scale_x_discrete(labels = c('close','crowded'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = gill.size), fill = "blue") + scale_x_discrete(labels = c('broad','narrow'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = gill.color), fill = "blue") + scale_x_discrete(labels = c('buff','red', 'gray', 'chocolate', 'black', 'brown', 'orange', 'pink', 'green', 'purple', 'white', 'yellow'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = stalk.shape), fill = "blue") + scale_x_discrete(labels = c('enlarging','tapering'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = stalk.root), fill = "blue") + scale_x_discrete(labels = c('missing','bulbous', 'club', 'equal', 'rooted'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = stalk.surface.above.ring), fill = "blue") + scale_x_discrete(labels = c('fibrous', 'silky', 'smooth', 'scaly'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = stalk.surface.below.ring), fill = "blue") + scale_x_discrete(labels = c('fibrous', 'silky', 'smooth', 'scaly'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = stalk.color.above.ring), fill = "blue") + scale_x_discrete(labels = c('buff','cinnamon', 'red', 'gray', 'brown', 'orange', 'pink', 'white', 'yellow'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = stalk.color.below.ring), fill = "blue") + scale_x_discrete(labels = c('buff','cinnamon', 'red', 'gray', 'brown', 'orange', 'pink', 'white', 'yellow'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = veil.type), fill = "blue") + scale_x_discrete(labels = c('partial'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = veil.color), fill = "blue") + scale_x_discrete(labels = c('brown', 'orange', 'white', 'yellow'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = ring.number), fill = "blue") + scale_x_discrete(labels = c('none', 'one', 'two'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = ring.type), fill = "blue") + scale_x_discrete(labels = c('evanescent', 'flaring', 'large', 'none', 'pendant'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = spore.print.color), fill = "blue") + scale_x_discrete(labels = c('buff','chocolate', 'black', 'brown', 'orange', 'green', 'purple', 'white', 'yellow'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = population), fill = "blue") + scale_x_discrete(labels = c('abundant', 'clustered', 'numerous', 'scattered', 'several', 'solitary'))
ggplot(data = mushrooms) +
  geom_bar(mapping = aes(x = habitat), fill = "blue") + scale_x_discrete(labels = c('woods', 'grasses', 'leaves', 'meadows', 'paths', 'urban', 'waste'))

### Association Rule Mining
###############################################################################
# Lets set a seed to preserve the random numbers used in this instance
set.seed(1337)

# Adding index into the first column of DF
mushrooms <- cbind(ID = rownames(mushrooms), mushrooms)
mushrooms$ID <- factor(mushrooms$ID)
ShroomIDS <- c(mushrooms$ID)
# A copy of the data without an ID for association rule mining
mushrooms_A <- subset(mushrooms, select = -c(ID))
mushrooms_A <- subset(mushrooms_A, select = -c(veil.type))

### Testing the Generation of Association Rules globally
# Final tune for a "large set" of rules - should be 98
mrules <- apriori(mushrooms_A, parameter = list(supp = 0.50, conf = 0.9, maxlen = 5 ))
# Summary to see what is generated initially
summary(mrules)
# Sort the rules by confidence 
mrules <-sort(mrules, by="lift", decreasing=TRUE)
# Show the top rules, but only 2 digits
options(digits=2)

arules::inspect(mrules[1:10])
top10 <- (mrules)
print(top10)

### Association Rules for class Edible Mushrooms
# Target class=e on RHS ordered by decreasing Lift
m_edible_rules_rhs <- apriori(mushrooms_A, parameter = list(supp = 0.30, conf = 0.9, maxlen = 5 ), 
                              appearance = list(default="lhs",rhs="class=e"),
                              control = list(verbose=F))
summary(m_edible_rules_rhs)
m_edible_rules_rhs<-sort(m_edible_rules_rhs, decreasing=TRUE,by="lift")
arules::inspect(m_edible_rules_rhs[1:10])
# Target class=e on LHS ordered by decreasing Lift
m_edible_rules_lhs <- apriori(mushrooms_A, parameter = list(supp = 0.30, conf = 0.9, maxlen = 5 ), 
                              appearance = list(default="rhs",lhs="class=e"),
                              control = list(verbose=F))
summary(m_edible_rules_lhs)
m_edible_rules_lhs<-sort(m_edible_rules_lhs, decreasing=TRUE,by="lift")
arules::inspect(m_edible_rules_lhs)

### Association Rules for class Poisonous Mushrooms
# Target class=p on RHS ordered by decreasing lift
m_poisonous_rules_rhs <- apriori(mushrooms_A, parameter = list(supp = 0.30, conf = 0.9, maxlen = 5 ), 
                                 appearance = list(default="lhs",rhs="class=p"),
                                 control = list(verbose=F))
summary(m_poisonous_rules_rhs)
m_poisonous_rules_rhs<-sort(m_poisonous_rules_rhs, decreasing=TRUE,by="lift")
arules::inspect(m_poisonous_rules_rhs[1:10])

# Target class=p on LHS ordered by decreasing lift
m_poisonous_rules_lhs <- apriori(mushrooms_A, parameter = list(supp = 0.30, conf = 0.9, maxlen = 5 ), 
                                 appearance = list(default="rhs",lhs="class=p"),
                                 control = list(verbose=F))
summary(m_poisonous_rules_lhs)
m_poisonous_rules_lhs<-sort(m_poisonous_rules_lhs, decreasing=TRUE,by="lift")
arules::inspect(m_poisonous_rules_lhs)
### Association Rule Mining Visualization section
###############################################################################
# visualization using aruleviz package
# Plotting the top 10 rules globally
top10 <- plot(mrules,method="graph",limit=10,engine="interactive")
top10
# plotting top 10 rules targeting rhs class=e
MEdibleViz <- plot(m_edible_rules_rhs,method="graph",limit=10,engine="interactive")
MEdibleViz
# plotting top 10 rules targeting rhs class=p
MPoisonousViz <- plot(m_poisonous_rules_rhs,method="graph",limit=10,engine="interactive")
MPoisonousViz

### Creating Training / Testing Sets
###############################################################################
### Using Sequencing to build training and testing datasets
(every4_indexes <- seq(1,nrow(mushrooms_A),4))
m_test <- mushrooms_A[every4_indexes,]
m_train <- mushrooms_A[-every4_indexes,]

m_train[1:5,6]
m_test[1:5,6]

# Storing the label of the class data and creating an unlabeld test dataframe
mtestlabels <- m_test$class
m_test_NO_LABEL <- subset(m_test, select = -c(class))


### Decision Trees & Random Forest
###############################################################################
# Decision tree using sequenced dataframes m_test and m_train
fitM <- rpart(m_train$class ~ ., data = m_train , method="class")
summary(fitM)
rpart.plot::rpart.plot(fitM)
## Predict the Test sets
predictedM <- predict(fitM,m_test_NO_LABEL, type="class")
## Confusion Matrix
table(predictedM,mtestlabels)


### Random Forest on sequenced dataframe
rfm_m <- randomForest(class~., data=m_train, ntree=11)
print(rfm_m)
## Prediction of test sets
pred_rfm_m <- predict(rfm_m, m_test_NO_LABEL, type=c("class"))
# Confusion matrix for Random Forest
table(pred_rfm_m,m_test$class)
