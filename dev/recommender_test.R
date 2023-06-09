set.seed(1234)

library("recommenderlab")
library("data.table")
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
    "movielens_small/movies.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

ratings <-
  setDT(read.csv(
    "movielens_small/ratings.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

ratings <- dcast(ratings,
                 userId ~ movieId,
                 value.var = "rating",
                 na.rm = FALSE)

ratings <- ratings[, .SD, .SDcols = -"userId"]

new_user <- ratings[1]
new_user <- new_user[, lapply(.SD, function(x) x <- NA_real_)]
new_user[, "2628" := 5.0]
new_user[, "5378" := 4.5]
new_user[, "33493" := 5.0]

ratings <- rbindlist(list(new_user, ratings), fill = TRUE)

ratingmat <- as(as.matrix(ratings), "realRatingMatrix")

# rec <- recommenderlab::Recommender(ratingmat[-1], method = "UBCF")
rec <- recommenderlab::Recommender(ratingmat[-1], method = "UBCF",
                       param = list(method = "pearson", nn = 40)
)

pred <- recommenderlab::predict(rec, ratingmat[1], n = 10)

movies[movieId %in% as.integer(unlist(as(pred, "list")))]
