#Create test and validation sets
###################################
# Create edx set and validation set
###################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


##--------------------------------------------

#Exploratory Data Analysis
sapply(edx, function(x) sum(length(which(is.na(x))))) #There are no NAs found in the data
n_distinct(edx$movieId) #10,677 films have been rated
n_distinct(edx$userId)  #69,878 users provided ratings
n_distinct(edx$genres)  #797 genres
avg <- mean(edx$rating) #3.5

#Visualizations
  #Distribution appears normal at user level with average rating roughly around 3.5
edx %>% group_by(userId) %>% summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) + geom_density(fill="#B3E5FC") +
  geom_hline(yintercept = 0, size = 1) +xlab("Rating") +
  labs(title="Distribution of Movie Ratings")

  #Top movies with ratings larger than average and rating volume of 10k
  #Table shows a variety of genres in the higher rating volume
edx %>% group_by(title) %>%
  summarize(b_i = mean(rating - avg), n = n()) %>% filter(b_i > 0.5, n > 10000) %>%
  ggplot(aes(reorder(title, b_i), b_i, fill = n)) +
  geom_bar(stat = "identity") + coord_flip() + scale_fill_distiller(palette = "PuBuGn") +
  ggtitle("") + xlab("Movie Title") + ylab("Rating - Avg") +
  ggtitle("Movie rating - Avg,\nfor Number of ratings > 10000") +
  theme_classic()

#Modeling Approach
  #Here we compute the variability against the mean at the movie level 'me'
  #and user level 'ue'
me<- edx %>% group_by(movieId) %>% summarize(b_i=mean(rating-avg))
ue<- edx %>% left_join(me, by="movieId") %>% group_by(userId) %>%
  summarize(b_u=mean(rating-b_i-avg))

#This model predicts on the validation dataset by using the movie effect. 
#It is calculating the average rating for each movie and the bias for each

prediction1 <- validation %>% left_join(me, by = "movieId") %>%
  mutate(pred1 = avg + b_i) %>% .$pred1

#This model predicts on the validation set using movie and user effect

prediction2 <- validation %>% left_join(m, by="movieId") %>%
  left_join(ue, by="userId") %>% mutate(pred2= avg + b_i + b_u) %>%
  .$pred2

#results
  #Here's the root mean square error function

rmse <- function(actual, predicted){
  sqrt(mean((actual-predicted)^2))}

  #Now we compute the RMSE for both models

rmse(validation$rating, prediction1) #RMSE for movie effect = 0.94
rmse(validation$rating, prediction2) #RMSE for movie and user effect = 0.865

#regulatization to constrain total variability of the effect sizes
#after running several iterations, optimal lambda is set to be around .5

lambdas <- .5
rmses <- sapply(lambdas,function(l){
  
  avg<-mean(edx$rating)
  
  #calculate movie effect and penalize low number of ratings
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - avg)/(n()+l))
  
  #calculate user and movie effect and penalize low number of ratings
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - avg)/(n()+l))
  
  #predict ratings in the training set to derive optimal lambda
  predicted_ratings <- 
    edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = avg + b_i + b_u) %>%
    .$pred
  
  return(rmse(predicted_ratings, edx$rating))
})
plot(lambdas, rmses,
     col = "red")



