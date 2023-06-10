# set.seed(1234)

# library(recommenderlab)
library(recosystem)
library(data.table)
#
# data("MovieLense")
#
# MovieLense100 <- MovieLense[rowCounts(MovieLense) > 100, ]
# MovieLense100
#
# train <- MovieLense100[1:300]
# rec <- Recommender(train, method = "UBCF")
# rec
#
# pre <- predict(rec, MovieLense100[301:302], n = 5)
# pre
#
# as(pre, "list")

movies <-
  setDT(read.csv(
    "movielens_full/movies.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

# ratings <-
#   setDT(read.csv(
#     "movielens_full/ratings.csv",
#     header = TRUE,
#     stringsAsFactors = FALSE
#   ))
# ratings <- ratings[, .SD, .SDcols = -"timestamp"]
#
# movie_ratings <- ratings[, .N, by = movieId]
# top_movies <- movie_ratings[N >= 50, movieId]
#
# ratings <- ratings[movieId %in% top_movies]
#
# user_ratings <- ratings[, .N, by = userId]
# top_raters <- user_ratings[N >= 20, userId]
#
# ratings <- ratings[userId %in% top_raters]

ratings <- readRDS("movielens_full/ratings.RDS")

movie_indices <- ratings[, unique(movieId)]

set.seed(123)

voted_items <- c(33493L)

user_vote <- data.table(userId = 51L,
                        movieId = voted_items,
                        rating = c(5.0))

missing_votes <- setdiff(movie_indices, voted_items)

ratings <- rbindlist(list(user_vote, ratings))
setorder(ratings, userId)

train_set <- recosystem::data_memory(
  user_index = ratings$userId,
  item_index = ratings$movieId,
  rating = ratings$rating,
  index1 = TRUE
)


rec <- Reco()

rec$train(train_set, out_model = NULL, opts = list(nthread = 12))


test_set <-
  recosystem::data_memory(
    user_index = c(rep(51L, length(missing_votes))),
    item_index = missing_votes,
    rating = rep(NA, times = length(missing_votes)),
    index1 = TRUE
  )

prediction <- rec$predict(test_set, out_memory())

prediction <- data.table(movieId = missing_votes,
                         pred = prediction)

prediction <-
  merge(prediction, movies[, .(movieId, title)], by = "movieId")
setorderv(prediction, cols = "pred", order = -1)



ratings <- dcast(ratings,
                 userId ~ movieId,
                 value.var = "rating",
                 na.rm = FALSE)

ratings <- ratings[, .SD, .SDcols = -"userId"]

new_user <- ratings[1]
new_user <- new_user[, lapply(.SD, function(x)
  x <- NA_real_)]
new_user[, "2628" := 5.0]
new_user[, "5378" := 4.5]
new_user[, "33493" := 5.0]

ratings <- rbindlist(list(new_user, ratings), fill = TRUE)

ratingmat <- as(as.matrix(ratings), "realRatingMatrix")

# rec <- recommenderlab::Recommender(ratingmat[-1], method = "UBCF")
rec <- recommenderlab::Recommender(ratingmat[-1],
                                   method = "UBCF",
                                   param = list(method = "pearson", nn = 40))

pred <- recommenderlab::predict(rec, ratingmat[1], n = 10)

movies[movieId %in% as.integer(unlist(as(pred, "list")))]


historyback <- copy(history)
history <- copy(historyback)
history[, pairs := rep(c(1L, 2L), nrow(history) / 2)]
history[, index := 1L:nrow(history)]

history_pairs <- split(x = history, by = "index")

history_pairs <- lapply(X = history_pairs, function(row) {
  do.call(list, args = list(role = ifelse(row[, pairs] == 1L, "user", "assistant"), content = row[, content]))
})
