library(recommenderlab)
library(stringr)
library(data.table)
library(ggplot2)
library(reshape2)
library(dplyr)

MOVIES <- read.csv("D:/movies.csv", stringsAsFactor = FALSE)
RATINGS <- read.csv("D:/ratings.csv", stringsAsFactors = FALSE)
str(MOVIES)
str(RATINGS)

head(MOVIES)
head(RATINGS)

MOVIE_GENRE <- as.data.frame(MOVIES$genres, stringsAsFactors = FALSE)
MOVIE_GENRE_2 <- as.data.frame(tstrsplit(MOVIE_GENRE[,1], "[|]",type.convert = TRUE),stringsAsFactors = FALSE)

colnames(MOVIE_GENRE_2) <- c(1:10)

GENRE_LIST <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
G_MAT <- matrix(0,10330,18)
G_MAT[1,] <- GENRE_LIST
colnames(G_MAT) <- GENRE_LIST

for(index in 1 :nrow(MOVIE_GENRE_2))
{
  for (col in 1:ncol(MOVIE_GENRE_2)) {
    GEN_COL = which(G_MAT[1,] == MOVIE_GENRE_2[index,col])
    G_MAT[index+1,GEN_COL] <- 1
  }
}

G_MAT1 <- as.data.frame(G_MAT[-1,],stringsAsFactors = FALSE)

for (col in 1:ncol(G_MAT1)) {
  G_MAT1[,col] <- as.integer(G_MAT1[,col])
} 

SEARCH_MOVIE <- cbind(MOVIES[,1:2],G_MAT1[])
View(SEARCH_MOVIE)

RATING_MAT <- reshape2::dcast(RATINGS, userId~movieId, value.var = "rating", na.rm=FALSE) #SPARSE MATRIX
RATING_MAT <- as.matrix(RATING_MAT[,-1]) #REMOVE USER ID
RATING_MAT <- as(RATING_MAT, "realRatingMatrix")

RATING_VAL <- as.vector(RATING_MAT@data)
unique(RATING_VAL)

VIEWS_RATING <- table(RATING_VAL)
VIEWS_RATING

#MOST VIEWED MOVIES VISUALIZATION USING GRAPH - SIMPLE RECOMMENDER
MOVIE_VIEWS <- colCounts(RATING_MAT) # VIEWS FOR EACH MOVIE
VIEWS <- data.frame(movie = names(MOVIE_VIEWS),
                          views = MOVIE_VIEWS) # DATAFRAME FOR VIEWS
VIEWS <- VIEWS[order(VIEWS$views,
                                 decreasing = TRUE), ]
VIEWS$title <- NA
for (index in 1:10325){
  VIEWS[index,3] <- as.character(subset(MOVIES,
                                              MOVIES$movieId == VIEWS[index,1])$title)
}
VIEWS[1:6,]
View(VIEWS[1:6,])

#PLOTTING
ggplot(VIEWS[1:6, ], aes(x = title, y = views)) +
  geom_bar(stat="identity", fill = 'steelblue') +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("TOP FILMS BASED ON VIEWS")


#CONTENT-BASED FILTERING - GENRE
BINARY_RATINGS <- RATINGS
for (i in 1:nrow(BINARY_RATINGS)){
  if (BINARY_RATINGS[i,3] > 3){
    BINARY_RATINGS[i,3] <- 1
  }
  else{
    BINARY_RATINGS[i,3] <- -1
  }
}

BR_2 <- dcast(BINARY_RATINGS, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(BR_2)){
  BR_2[which(is.na(BR_2[,i]) == TRUE),i] <- 0
}
BR_2 = BR_2[,-1] #REMOVING UNNECESSARY COLOUMNS

RES = matrix(0,18,706)
for (c in 1:ncol(BR_2)){
  for (i in 1:ncol(G_MAT1)){
    RES[i,c] <- sum((G_MAT1[,i]) * (BR_2[,c]))
  }
}


for (i in 1:nrow(RES)){
  if (RES[i] < 0){
    RES[i] <- 0
  }
  else {
    RES[i] <- 1
  }
}

RES2 <- RES[1,] #USER PROFILE
SIMILARITY_MATRIX <- rbind.data.frame(RES2, G_MAT1)
SIMILARITY_MATRIX <- data.frame(lapply(SIMILARITY_MATRIX,function(x){as.integer(x)})) #CONVERTING TO INTEGER

#JACCARD DISTANCE BETWEEN USER PROFILE AND ALL MOVIES
library(proxy)
SIMILARITY_RES <- dist(SIMILARITY_MATRIX, method = "Jaccard")
SIMILARITY_RES <- as.data.frame(as.matrix(SIMILARITY_RES[1:8552]))
rows <- which(SIMILARITY_RES == min(SIMILARITY_RES))
#RECOMMENDED MOVIES FOR USER 1
View(MOVIES[rows,])


MOVIE_RAT <- RATING_MAT[rowCounts(RATING_MAT) > 50,colCounts(RATING_MAT) > 50]
MOVIE_RAT

#COLLABORATIVE FILTERING SYSTEM
SAMP<- sample(x = c(TRUE, FALSE),
                      size = nrow(MOVIE_RAT),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
DATA1 <- MOVIE_RAT[SAMP, ]
DATA2 <- MOVIE_RAT[!SAMP, ]

#RECOMMENDATION SYSTEM
RECOMM_SYS <- recommenderRegistry$get_entries(dataType ="realRATING_MAT")
RECOMM_SYS$IBCF_realRATING_MAT$parameters

RECOMM_MOD <- Recommender(data = DATA1,
                              method = "IBCF",
                              parameter = list(k = 30))

TOP_RECOMM <- 10
PREDICTED <- predict(object = RECOMM_MOD,
                                     newdata = DATA2,
                                     n = TOP_RECOMM)
PREDICTED


USER <- PREDICTED@items[[1]] # RECOMMENDATION FOR USER 1
M_USER <- PREDICTED@itemLabels[USER]
M_USER2 <- M_USER
for (index in 1:10){
  M_USER2[index] <- as.character(subset(MOVIES,
                                             MOVIES$movieId == M_USER[index])$title)
}
View(M_USER2)
#META DATA BASED
TAGS <- as.data.frame(read.csv("D:/tags.csv", stringsAsFactor = FALSE))
SEARCH = readline(prompt = "SEARCH MOVIES : ")

S <- filter(TAGS, TAGS$tag == SEARCH)
RES <- dplyr::select(S,movieId)
print(RES)
print(as.data.frame(RES))


for(i in 1:nrow(RES))
{
  D <- filter(MOVIES, MOVIES$movieId == as.numeric(RES$movieId[i]))
  X <- dplyr::select(D,title,movieId)
  print(X)
}


#TOP TRENDING - UPCOMING SHOWS
A <- filter(TAGS, TAGS$likes > 300 )
B <- dplyr::select(TAGS,movieId)
RES <- unique(B)
print(as.data.frame(RES))

for(i in 1:nrow(RES))
{
  D <- filter(MOVIES, MOVIES$movieId == RES$movieId[i])
  X <- dplyr::select(D,title,movieId)
  print(X)
}



