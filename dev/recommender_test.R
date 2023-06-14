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

set.seed(1234)

voted_items <- c(33493L)

user_vote <- data.table(userId = 51L,
                        movieId = voted_items,
                        rating = c(5.0))

missing_votes <- setdiff(movie_indices, voted_items)

ratings <- rbindlist(list(user_vote, ratings))
setorder(ratings, userId)

ratings[, rating := rating - mean(rating), by = userId]

ratings <- ratings[sample(.N, 1000000)]

train_set <- recosystem::data_memory(
  user_index = ratings$userId,
  item_index = ratings$movieId,
  rating = ratings$rating,
  index1 = TRUE
)

rec <- Reco()

# res <- rec$tune(
#   train_set,
#   opts = list(
#     # dim      = c(20L, 30L, 40L),
#     dim      = c(30L, 40L),
#     costp_l1 = c(0, 0.033),
#     # costp_l2 = c(0.01, 0.033, 0.066, 0.1, 0.3),
#     costq_l1 = c(0, 0.033),
#     costq_l2 = c(0.01, 0.033),
#     lrate    = c(0.033, 0.3),
#     nthread = 10
#   )
# )
#
# res
# res$min
# res <- setDT(res$res)
# res[, median(loss_fun), by = lrate]

rec$train(
  train_set,
  out_model = NULL,
  opts = list(
    dim = 40L,
    costp_l1 = 0.033,
    costp_l2 = 0.1,
    costq_l1 = 0,
    costq_l2 = 0.033,
    lrate = 0.1,
    nthread = 10,
    niter = 25
  )
)


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
  do.call(list, args = list(
    role = ifelse(row[, pairs] == 1L, "user", "assistant"),
    content = row[, content]
  ))
})

write_function <- function(x) {
  data <- rawToChar(x)
  data_json <- jsonlite::fromJSON(data, simplifyVector = FALSE)
  print(data_json)
}

write_function <- function(x) {
  # browser()
  data <- rawToChar(x)
  data_json <- jsonlite::fromJSON(data, simplifyVector = FALSE)
  message_content <- data_json$choices[[1]]$message$content

  print(message_content)
}

POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(Authorization = paste("Bearer", apiKey2)),
  content_type("application/json"),
  encode = "json",
  body =    body2,
  stream = TRUE,
  write_stream(function(x) {
    print(length(x))
    length(x)
  })
)





h <- new_handle()
handle_setopt(
  h,
  url = "https://api.openai.com/v1/chat/completions",
  customrequest = "POST",
  httpheader = c(
    "Authorization" = paste("Bearer", apiKey2),
    "Content-Type" = "application/json"
  )
)

# Define the function that will handle each line of the response
callback <- function(data, ...) {
  print(data)
}

# Perform the request
curl_fetch_multi(
  h,
  writefunction = callback,
  done = function(res) {
    print(res$status_code)
  }
)
