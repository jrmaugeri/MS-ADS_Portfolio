### FILE INFO
###############################################################################
# Author: Group 4 ( Joseph Maugeri, Yu Sheng Lu , Jonathan Xi)
# Class: IST 707
# Week: 6
# Activity: Mushroom R data 
# Purpose: Perform some exploratory analysis of the mushroom dataset,
# Prepare the data for several data mining techniques
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
# ONCE install.packages("rpart.plot")
library(rpart.plot)
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



### Reading in the data
###############################################################################

mushrooms <- read.csv('C:/Users/samsa/OneDrive/Documents/mushrooms.csv', stringsAsFactors = TRUE)

### Exploring the Data
###############################################################################
# Using summary to get an overview
(summary(mushrooms))
# Using str() to investigate the size
(str(mushrooms))
# using sapply() with levels() to print all levels of the dataframe
M_Levels <- sapply(mushrooms,levels)
(M_Levels)

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

## Testing the Generation of Association Rules
# Mine rules with the apriori algorithm. 
# First look with low support, 80% confidence
mrules <- apriori(mushrooms_A, parameter = list(supp = 0.10, conf = 0.9, maxlen = 5 ))
# Summary to see what is generated initially
summary(mrules)
# Sort the rules by confidence 
mrules <-sort(mrules, by="confidence", decreasing=TRUE)
# Show the top rules, but only 2 digits
options(digits=2)

arules::inspect(mrules[1:10])
top10 <- (mrules)
print(top10)

## Association Rules for class Edible Mushrooms
# Target class=e on RHS ordered by decreasing support
m_edible_rules <- apriori(mushrooms_A, parameter = list(supp = 0.10, conf = 0.9, maxlen = 5 ), 
                          appearance = list(default="lhs",rhs="class=e"),
                          control = list(verbose=F))
summary(m_edible_rules)
m_edible_rules<-sort(m_edible_rules, decreasing=TRUE,by="support")
arules::inspect(m_edible_rules[1:10])
## Association Rules for class Poisonous Mushrooms
# Target class=p on RHS ordered by decreasing support
m_poisonous_rules <- apriori(mushrooms_A, parameter = list(supp = 0.10, conf = 0.9, maxlen = 5 ), 
                             appearance = list(default="lhs",rhs="class=p"),
                             control = list(verbose=F))
summary(m_poisonous_rules)
m_poisonous_rules<-sort(m_poisonous_rules, decreasing=TRUE,by="support")
arules::inspect(m_poisonous_rules[1:10])

### Association Rule Mining Visualization section
###############################################################################
# visualization using aruleviz package
# Plotting the top 19 rules 
top20 <- plot(mrules,method="graph",limit=20,interactive=TRUE)
top20
# plotting top 10 rules targeting rhs class=e
MEdibleViz <- plot(m_edible_rules,method="graph",limit=10,interactive=TRUE)
MEdibleViz
# plotting top 10 rules targeting rhs class=p
MPoisonousViz <- plot(m_poisonous_rules,method="graph",limit=10,interactive=TRUE)
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


### Decision Trees model Build
###############################################################################
# Decision tree using sequenced dataframes m_test and m_train
fitM <- rpart(m_train$class ~ ., data = m_train , method="class")
summary(fitM)
fancyRpartPlot(fitM)
## Predict the Test sets
predictedM <- predict(fitM,m_test_NO_LABEL, type="class")
## Confusion Matrix
table(predictedM,mtestlabels)


# Testing the model
fitT <- rpart(Ttest$Survived ~ ., data = Ttrain , method="class")
summary(fitT)
fancyRpartPlot(fitT)
## Predict the Test sets
predictedT <- predict(fitT,Ttest, type="class")
## Confusion Matrix
table(predictedT,testSurvival)

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






library(cluster)

library(dplyr)
# ensuring reproducibility for sampling
set.seed(8000)
# generating random variable set
# specifying ordered factors, strings will be converted to factors when using data.frame()
# customer ids come first, we will generate 200 customer ids from 1 to 200
ID <- c(1:8124) %>%
  factor()
class <- sample(c("edible", "poisonous"), 8124, replace = T, prob = c(0.5, 0.5)) 
capshape <- sample(c("bell", "conical", "convex", "flat", "knobbed", "sunken"), 8124, 
                     replace = T,
                  prob = c(0.1, 0.1, 0.35, 0.3, 0.05, 0.1))
capsurface <- sample(c("fibrous", "grooves", "scaly", "smooth"), 8124, replace = T, 
                    prob = c(0.1, 0.2, 0.4, 0.3))
capcolor <- sample(c("brown", "buff", "cinnamon", "gray", "green", "pink","purple","red","white", "yellow"), 
                   8124, replace = T,
                   prob = c(0.3, 0.2, 0.05, 0.05, 0.05, 0.05, 0.05, 0.1, 0.05, 0.1))
bruises <- sample(c("bruises", "no"), 8124, replace = T,
                prob = c(0.6, 0.4)) 
odor <- sample(c("almond", "anise", "creosote", "fishy", "foul", "musty", "none", "pungent", "spicy"), 8124, 
               replace = T,
               prob = c(0.05, 0.05, 0.05, 0.05, 0.25, 0.05, 0.4, 0.05, 0.05))
gillattachment <- sample(c("attached", "descending", "free", "notched"), 
                         8124, replace = T,
                         prob = c(0.05, 0, 0.95, 0))

gillspacing <- sample(c("close", "crowded", "distant"), 
                      8124, replace = T, 
                      prob = c(0.84, 0.16, 0))
gillsize <- sample(c("broad", "narrow"), 
                   8124, replace = T,
                   prob = c(0.69, 0.31))
gillcolor <- sample(c("black", "brown", "buff", "chocolate", "gray", "green", "orange","pink","purple", "red","white","yellow"), 
                    8124, replace = T,
                    prob = c(0.1, 0.05, 0.25, 0.05, 0.05, 0.05, 0.05, 0.05, 0.2, 0.05, 0.05, 0.05))
stalkshape <- sample(c("enlarging", "tapering"), 
                     8124, replace = T,
                     prob = c(0.57, 0.43))
stalkroot <- sample(c("bulbous", "club", "cup", "equal", "rhizomorphs", "rooted", "missing"), 
                    8124, replace = T,
                    prob = c(0.45, 0.05, 0.05, 0.05, 0.05, 0.05, 0.3))
stalksurfaceabove <- sample(c("fibrous", "scaly", "silky", "smooth"), 
                            8124, replace = T,
                            prob = c(0.03, 0.04, 0.29, 0.64))
stalksurfacebelow <- sample(c("fibrous", "scaly", "silky", "smooth"), 
                            8124, replace = T,
                            prob = c(0.06, 0.05, 0.28, 0.61))
stalkcolorabove <- sample(c("brown", "buff", "cinnamon", "gray", "orange", "pink", "red", "white", "yellow"), 
                          8124, replace = T,
                          prob = c(0.05, 0.05, 0.05, 0.05, 0.05, 0.2, 0.05, 0.45, 0.05))
stalkcolorbelow <- sample(c("brown", "buffe", "cinnamon", "gray", "orange", "pink", "red", "white", "yellow"), 
                          8124, replace = T,
                          prob = c(0.05, 0.05, 0.05, 0.05, 0.05, 0.2, 0.05, 0.45, 0.05))
veiltype <- sample(c("partial", "universal"), 
                   8124, replace = T,
                   prob = c(1, 0))
veilcolor <- sample(c("brown", "orange", "white", "yellow"), 
                    8124, replace = T,
                    prob = c(0.1, 0.05, 0.98, 0.05))
ringnumber <- sample(c("none", "one", "two"), 8124, replace = T) %>%
                       factor(levels=c("none", "one", "two"), 
                              ordered = TRUE)
ringtype <- sample(c("cobwebby", "evanescent", "flaring", "large", "none", "pendant", "sheathing", "zone"), 
                   8124, replace = T,
                   prob = c(0.05, 0.3, 0.05, 0.05, 0.05, 0.4, 0.05, 0.05))
sporecolor <- sample(c("black", "brown", "buff", "chocolate", "green", "orange", "purple", "white", "yellow"), 
                     8124, replace = T,
                     prob = c(0.1, 0.25, 0.05, 0.1, 0.05, 0.05, 0.05, 0.3, 0.05))
population <- sample(c("abundant", "clustered", "numerous", "scattered", "several", "solitary"), 
                     8124, replace = T,
                     prob = c(0.05, 0.05, 0.1, 0.1, 0.5, 0.2))
habitat <- sample(c("grasses", "leaves", "meadows", "paths", "urban", "waste", "woods"), 
                  8124, replace = T,
                  prob = c(0.25, 0.1, 0.05, 0.05, 0.1, 0.05, 0.4))




df <- data.frame(ID, class, capshape, capsurface, capcolor, bruises, odor, gillattachment, 
                 gillspacing, gillsize, gillcolor, stalkshape, stalkroot, stalksurfaceabove,
                 stalksurfacebelow, stalkcolorabove, stalkcolorbelow, veiltype, 
                 veilcolor, ringnumber, ringtype, sporecolor, population, habitat)

df[,2] <- as.factor(df[,2])
df[,3] <- as.factor(df[,3])
df[,4] <- as.factor(df[,4])
df[,5] <- as.factor(df[,5])
df[,6] <- as.factor(df[,6])
df[,7] <- as.factor(df[,7])
df[,8] <- as.factor(df[,8])
df[,9] <- as.factor(df[,9])
df[,10] <- as.factor(df[,10])
df[,11] <- as.factor(df[,11])
df[,12] <- as.factor(df[,12])
df[,13] <- as.factor(df[,13])
df[,14] <- as.factor(df[,14])
df[,15] <- as.factor(df[,15])
df[,16] <- as.factor(df[,16])
df[,17] <- as.factor(df[,17])
df[,18] <- as.factor(df[,18])
df[,19] <- as.factor(df[,19])
df[,20] <- as.factor(df[,20])
df[,21] <- as.factor(df[,21])
df[,22] <- as.factor(df[,22])
df[,23] <- as.factor(df[,23])
df[,24] <- as.factor(df[,24])


#----- Dissimilarity Matrix -----#
library(cluster) 
# to perform different types of hierarchical clustering
# package functions used: daisy(), diana(), clusplot()
gower.dist <- daisy(df[ ,2:24], metric = c("gower"))



divisive.clust <- diana(as.matrix(gower.dist), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")


aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages")


# hierarchical clustering
hc<-hclust(gower.dist, method = "complete")
# dendrogram 
plot(hc, labels=FALSE)
rect.hclust(hc, k=8, border="red")
# choose k, number of clusters 
cluster<-cutree(hc, k=8)
# add cluster to original data 
df<-cbind(df,as.factor(cluster))



install.packages("fpc")
library(fpc)
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am capping the maximum amout of clusters by 7
# I want to choose a reasonable number, based on which I will be able to see basic differences between customer groups as a result
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 7)
stats.df.divisive

stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, 7) #complete linkages looks like the most balanced approach
stats.df.aggl


library(ggplot2)
# Elbow
# Divisive clustering
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))