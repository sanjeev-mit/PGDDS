
### Loading necessary packages################


required_packages <- c("dplyr","ggplot2","rstudioapi","recommenderlab")
lapply(required_packages, require, character.only = TRUE)

####setting source directory as working directory#####

sourcedir <- getActiveDocumentContext()$path 
setwd(dirname(sourcedir))

####reading the beer mart data

beer_mart <- read.csv('beer_data.csv')

####Overview of data#################

str(beer_mart) ##beerid and review_overall are numeric and profilename is factor

summary(beer_mart) ##reviews are in range 0-5

dim(beer_mart) ###475984 user rating and 3 attributes

####Data Cleansing######

###Removing na or blank users

 invalid_users <- nrow(beer_mart[(is.na(beer_mart$review_profilename) | beer_mart$review_profilename==""),])##There are 100 such invalid profiles

##100 invalid users data removed
beer_mart <- beer_mart[!(is.na(beer_mart$review_profilename) | beer_mart$review_profilename==""),]

######removing duplicate value-Total 474462 distinct entries

beer_mart<-distinct(beer_mart,beer_beerid,review_profilename,.keep_all = TRUE)

#######Total -40304 beers are reviewed

beer_reviewcount<- beer_mart %>% group_by(beer_mart$beer_beerid) %>% summarise(total_beer_reviews=n())%>% arrange(desc(total_beer_reviews))
colnames(beer_reviewcount) <- c("BeerID","ReviewCount")
summary(beer_reviewcount) ###The reviews for the beer varies from 1 to 977 with mean as 11.77 and median as 2-data is heavily right skewed


#######frequency of review count######

review_frequency <- beer_reviewcount %>% group_by(beer_reviewcount$ReviewCount) %>% summarise(Total_occurrence=n()) %>% arrange(desc(Total_occurrence))
View(review_frequency)
##As the count suggest there are huge number of beer which are reviewed only once or less than 10.As we are building recommendation engine
##which recommends the beer likely to be bought-hence ignoring such low reviewed beer.

####subset of beer having more than 10 reviews

beer_reviewcount <- beer_reviewcount %>% subset(ReviewCount >10)
View(beer_reviewcount)
summary(beer_reviewcount)###The Mean is 60.64 and Median is 28.00 hence still it is highly right skewed

###subset of beer data having more than 29 (which is more than 28 ie median)

beer_reviewcount <- beer_reviewcount %>% subset(ReviewCount >29)
View(beer_reviewcount)
summary(beer_reviewcount) ##Median-68 and Mean 143- hence taking N=30


#Lets also count number of distinct users 

beer_mart %>% group_by(review_profilename) %>% summarise(total_user_reviews=n()) %>% nrow() #22497 distinct users

#Lets now check which user have reviewed maximum beers

beer_mart %>% group_by(review_profilename) %>% summarise(user_review_count=n()) %>% top_n(1)
#user: northyorksammy have given max reviews:1842 


##################beer data having review more than equal to 30 reviews#########

beer_final <- merge(beer_mart,beer_reviewcount,by.x="beer_beerid",by.y="BeerID")[,-4]
beer_final <- beer_final[,c(2,1,3)]
View(beer_final)

class(beer_final)

##Converting it into realRatingMatrix

beer_rrMatrix <- as(beer_final,"realRatingMatrix")

class(beer_rrMatrix)

#Coerce the matrix to a dataframe

beers_df <- as(beer_rrMatrix, "data.frame")
colnames(beers_df) <- c("User","BeerID","Rating")
View(beers_df)
summary(beers_df)


#Checking realRatingMatrix
dimnames(beer_rrMatrix)
rowCounts(beer_rrMatrix) 
colCounts(beer_rrMatrix) 
rowMeans(beer_rrMatrix)  


#######################################################################################################
#II. Data Exploration:

#1. Determine how similar the first ten users are with each other and visualise it

#Similarity of first ten users with each other

similar_users <- similarity(beer_rrMatrix[1:10,],method = "cosine",which = "users") 
class(similar_users)

#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

#2. Compute and visualise the similarity between the first 10 beers

similar_beers <- similarity(beer_rrMatrix[,1:10],method = "cosine",which = "items")

#Similarity matrix
as.matrix(similar_beers)

#Visualise similarity matrix
image(as.matrix(similar_beers), main = "Beer similarity")

#3. What are the unique values of ratings? 

beers_df %>% group_by(Rating) %>% summarise(rating_frequency=n()) %>% nrow() ##Total number of unique ratings=9

## unique values of ratings
beers_df %>% group_by(Rating) %>% summarise(rating_frequency=n()) %>% View()

#So the ratings start with 1 and goes upto 5 at an interval of 0.5 

#4. Visualise the rating values and notice:
View(beers_df)

#(i) The average beer ratings

avg_beer_ratings<-beers_df %>% group_by(BeerID) %>% summarise(average_rating=mean(Rating))

ggplot(avg_beer_ratings,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="# of Beers")
summary(avg_beer_ratings$average_rating)

#So average beer ratings(Mean)=3.796 & Median=3.846, almost normal

#(ii) The average user ratings
avg_user_ratings<-beers_df %>% group_by(User) %>% summarise(average_rating=mean(Rating))
ggplot(avg_user_ratings,aes(x=average_rating)) + geom_histogram() + labs(x="Average Rating", y="# of Users")
summary(avg_user_ratings$average_rating)

#So average beer ratings(Mean)=3.938 & Median=4.00, almost normal


#(iii) The average number of ratings given to the beers

avg_beer_reviews<-beers_df %>% group_by(BeerID) %>% summarise(average_reviews=mean(n()))
View(avg_beer_reviews)
ggplot(avg_beer_reviews,aes(x=average_reviews)) + geom_histogram() + labs(x="Average Rating", y="# of Beers")
summary(avg_beer_reviews$average_reviews)

#So on average each beer gets ~108 reviews from chosen subset


#(iv) The average number of ratings given by the users

avg_user_reviews<-beers_df %>% group_by(User) %>% summarise(average_reviews=mean(n()))
ggplot(avg_user_reviews,aes(x=average_reviews)) + geom_histogram()
summary(avg_user_reviews$average_reviews)

View(avg_user_reviews)

#So on average each user gives ~17 reviews, but this distribution is very skewed


#######################################################################################################
#III. Recommendation Models:

#1. Divide your data into training and testing datasets, Experiment with 'split' and 'cross-validation' evaluation schemes

#i) Scheme1 with train/test(75/25) using split without cross validation & goodRating as 4  
scheme1 <- evaluationScheme(beer_rrMatrix, method = "split", train = .75,k = 1, given = -1, goodRating = 4)
scheme1

#ii) Scheme2 using cross-validation without cross validation(5 folds) & goodRating as 4 
scheme2 <- evaluationScheme(beer_rrMatrix, method = "cross-validation",k = 5, given = -1, goodRating = 4)
scheme2

#2. Building IBCF and UBCF models with below hyperparameters

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=50)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score")))

#Evaluating algorithms & predicting next n beers

results1 <- evaluate(scheme1, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results1)

results2 <- evaluate(scheme2, algorithms, n=c(1, 3, 5, 10, 15, 20))
class(results2)

#3. Comparing the performance of the two models and suggest the one that should be deployed
#Drawing ROC curve

plot(results1, annotate = 1:4, legend="topleft")
plot(results2, annotate = 1:4, legend="topleft")

#So UBCF seems to get better then IBCF especially with higher values of n

#4. Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"

#Making Recomendations using UBCF 

r <- Recommender(beer_rrMatrix, method = "UBCF") 
recom_cokes <- predict(r, beer_rrMatrix["cokes"],n=5)
as(recom_cokes, "list")

#recommendation for cokes: "1283"  "42533" "1867"  "72138"  "4083" 

recom_genog <- predict(r, beer_rrMatrix['genog'], n=5)
as(recom_genog, "list")

#recommendation for geong: "16074" "7971" "1013"  "19960"  "34420"  

recom_giblet <- predict(r, beer_rrMatrix['giblet'], n=5)
as(recom_giblet, "list")
#recommendation for giblet: "22352" "7971"  "1385"  "7597"  "571" 
