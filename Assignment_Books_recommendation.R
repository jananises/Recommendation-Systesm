#Installing and loading the libraries
install.packages("recommenderlab", dependencies=TRUE)
install.packages("Matrix")
library("recommenderlab")
library(caTools)
library(reshape2)
library(ggplot2)

#book rating data
book <- read.csv("C:\\Data Science\\Assignments\\Recommendation\\book.csv", header=TRUE)
## Remove 'X' column. We do not need it
book <- book[,-1]
dim(book)
head(book,10)
#Omit Null Values
book <- na.omit(book)
dim(book)


####Descriptive Statistics
#Printing Summary 
summary(book)

#metadata about the variable
str(book)
unique(book$User.ID)
No_of_Users <- sort(book$User.ID,decreasing = TRUE)
summary(No_of_Users)

b <- unique(book$Book.Title)
No_of_Books <- as.data.frame(sort(b,decreasing = TRUE))
str(No_of_Books)
View(No_of_Books)
summary(No_of_Books)

## Group_by Title, calculate mean of ratings
df <- as.data.frame(aggregate(book$Book.Rating, by=list(book$Book.Title), FUN=mean))
head(df)
sorted_output <- sort(df$x, decreasing = TRUE)
head(sorted_output)

book_rating <- aggregate(book$Book.Rating ~ book$Book.Title, book, mean, na.rm = TRUE, decreasing = TRUE)


#################################################################################

#Histogram rating distribution 
hist(book$Book.Rating, freq=TRUE, breaks=10, col = 'blue')
#Density Plot rating distribution 
plot(density(book$Book.Rating), col="red")

###############################################
#Represent the data as a real rating matrix    
RR.matrix <- as(book,"realRatingMatrix")
RR.matrix
####################################################
#Build the Model
#Now we build algorithms based on Popularity 

#Popularity based 
book_rec_modelPop <- Recommender(RR.matrix, method="POPULAR")

#To display top N items and item affinity
#We predict for Users, Recommended items:     

Recommended.itemsPoP <- predict(book_rec_modelPop, RR.matrix[2182,], n=10)
as(Recommended.itemsPoP, "list")

#To obtain top 5 and display them:    
Recommended.itemsPoP5 <- bestN(Recommended.itemsPoP, n=5)       
as(Recommended.itemsPoP5, "list")

#Return the ratings given by this User
which(book$User.ID=='278854')
book[2394:2399,]

###############################################
#User Based Collaborative Filtering

book_rec_modelUBCF <- Recommender(RR.matrix, method="UBCF")


#To display top N items and item affinity
#Recommended items:     

Recommended.itemsUBCF <- predict(book_rec_modelUBCF, RR.matrix[2182,], n=10)
as(Recommended.itemsUBCF, "list")

#To obtain top 5 and display them:    
Recommended.itemsUBCF5 <- bestN(Recommended.itemsUBCF, n=5)       
as(Recommended.itemsUBCF5, "list")

##Now, for the same user "2182???, let's have a look at the affinity value computed for all items we didn't have any value in the original data:
#to predict affinity to all non-rated items 
predicted.affinity.2182 <- predict(book_rec_modelUBCF, RR.matrix[2182,], type="ratings", n=10)
# to see the user "u15348"'s predicted affinity for items we didn't have any value for
as(predicted.affinity.2182, "list")

# .. and the real affinity for the items obtained from the affinity.matrix
as(RR.matrix[2182,], "list")


###################################################
#RERECOMMEND based 
book_rec_modelRE <- Recommender(RR.matrix, method="RERECOMMEND")

#To display top N items and item affinity
#Recommended items:     
#Re-recommends highly rated items (real ratings)

Recommended.itemsRE <- predict(book_rec_modelRE, RR.matrix[2182,], n=10)
as(Recommended.itemsRE, "list")


#To obtain top 5 and display them:    
Recommended.itemsRE5 <- bestN(Recommended.itemsRE, n=5)       
as(Recommended.itemsRE5, "list")


##########################
#IBCF based Collaborative Filtering - Item Based Collaborative Filtering (IBCF) recommends items on the basis of the similarity matrix. this algorithm is efficient and scalable.
rec=Recommender(RR.matrix,method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
names(getModel(rec))
getModel(rec)$nn

Recommended.items <- predict(rec, RR.matrix[114,], n=10)
as(Recommended.items, "list")

#To obtain top 5 and display them:    
Recommended.items5 <- bestN(Recommended.items, n=5)       
as(Recommended.items5, "list")

#########################################################################

book_recomm_model3 <- Recommender(RR.matrix, method="IBCF")


#To display top N items and item affinity
#Recommended items:     

Recommended3.items <- predict(book_recomm_model3, RR.matrix[114,], n=10)
as(Recommended3.items, "list")

#To obtain top 5 and display them:    
Recommended3.items5 <- bestN(Recommended3.items, n=5)       
as(Recommended3.items5, "list")


Recommended2.items <- predict(book_recomm_model2, RR.matrix[380,], n=10)
as(Recommended2.items, "list")

#To obtain top 5 and display them:    
Recommended2.items5 <- bestN(Recommended2.items, n=5)       
as(Recommended2.items5, "list")

##########################################################

rec=Recommender(book_rate_data_matrix,method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
print(rec)
names(getModel(rec))
getModel(rec)$nn

#####################################
#Build the Model - IBCF
#Now we build algorithms based on IBCF
df_train <- binarize(RR.matrix, minRatin = 1)
parameter <- list(method = 'Jaccard')
rec_IBCF=Recommender(df_train[2182],method="IBCF", parameter = parameter)

Recommended.itemsIBCF <- predict(rec_IBCF, RR.matrix[2182], n=10)
as(Recommended.itemsIBCF, "list")

#To obtain top 5 and display them:    
Recommended.itemsIBCF5 <- bestN(Recommended.itemsIBCF, n=5)       
as(Recommended.itemsIBCF5, "list")

#Return the ratings given by this User
which(book$User.ID=='278854')
book[2394:2399,]

######################################################
#Validation of the Models
#UBCF
#Split the data into train and test: UBCF
Vmatrix <- evaluationScheme(RR.matrix, method="split", train=0.9, given=1)
RM <- Recommender(getData(Vmatrix,"train"), "UBCF")

test <- predict( RM, getData(Vmatrix, "known"), type="ratings")
error1 <- calcPredictionAccuracy( test, getData(Vmatrix,"unknown"))


#Validation of the model
#Popular
#Split the data into train and test: popular
Vmatrix <- evaluationScheme(RR.matrix, method="split", train=0.9, given=1)
RM <- Recommender(getData(Vmatrix,"train"), "POPULAR")

test <- predict( RM, getData(Vmatrix, "known"), type="ratings")
error2 <- calcPredictionAccuracy( test, getData(Vmatrix,"unknown"))


#Generate Error Matrix
error <- rbind(error1,error2)
rownames(error) <- c("UBCF","POPULAR")
error


