library(proxy)
library(recommenderlab)
library(reshape2)

library(httr)

library(recosystem)
library(data.table)

movies <-
  setDT(read.csv(
    "movielens_full/movies.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  ))

ratings <- readRDS("movielens_full/ratings.RDS")

movie_recommendation <-
  function(input1 = NA,
           input2 = NA,
           input3 = NA,
           movies_data = movies,
           ratings_data = ratings) {
    ratings <- copy(ratings_data)
    input1_id <- movies_data[title %in% input1, movieId]
    input2_id <- movies_data[title %in% input2, movieId]
    input3_id <- movies_data[title %in% input3, movieId]

    if (length(input1_id) == 0)
      input1_id <- NA
    if (length(input2_id) == 0)
      input2_id <- NA
    if (length(input3_id) == 0)
      input3_id <- NA

    inputs <- c(input1_id, input2_id, input3_id)

    if (all(is.na(inputs))) {
      no_result <- data.frame(matrix(NA, 1))
      no_result[1, 1] <-
        "Sorry, there is not enough information in our database on the movies_data you've selected. Try to select different movies you like."
      colnames(no_result) <- "No results"
      return(no_result)
    }

    user_vote <- data.table(userId = 51L,
                            movieId = inputs[!is.na(inputs)],
                            rating = 5.0)

    movie_indices <- ratings[, unique(movieId)]

    missing_votes <- setdiff(movie_indices, inputs)

    ratings <- rbindlist(list(user_vote, ratings))
    setorder(ratings, userId)

    train_set <- recosystem::data_memory(
      user_index = ratings$userId,
      item_index = ratings$movieId,
      rating = ratings$rating,
      index1 = TRUE
    )

    rec <- Reco()
    set.seed(123)
    rec$train(train_set,
              out_model = NULL,
              opts = list(niter = 20, nthread = 8))


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
      merge(prediction, movies_data[, .(movieId, title)], by = "movieId")
    setorderv(prediction, cols = "pred", order = -1)

    prediction <- head(prediction, n = 20L)
    return(prediction)
  }

# movies <-
#   read.csv("movies.csv", header = TRUE, stringsAsFactors = FALSE)
# ratings <- read.csv("ratings.csv", header = TRUE)
# movies2 <-
#   movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]
#
# movie_recommendation <- function(movies2, ratings, input, input2, input3) {
#   input = "[REC] (2007)"
#   input2 = "Paranormal Activity (2009)"
#   input3 = "Ring, The (2002)"
#   row_num <- which(movies2[, 2] == input)
#   row_num2 <- which(movies2[, 2] == input2)
#   row_num3 <- which(movies2[, 2] == input3)
#   userSelect <- matrix(NA, 10325)
#   userSelect[row_num] <- 5 #hard code first selection to rating 5
#   userSelect[row_num2] <- 4 #hard code second selection to rating 4
#   userSelect[row_num3] <- 3 #hard code third selection to rating 3
#   userSelect <- t(userSelect)
#
#   ratingmat <-
#     dcast(ratings,
#           userId ~ movieId,
#           value.var = "rating",
#           na.rm = FALSE)
#   browser()
#   ratingmat <- ratingmat[, -1]
#   colnames(userSelect) <- colnames(ratingmat)
#   ratingmat2 <- rbind(userSelect, ratingmat)
#   ratingmat2 <- as.matrix(ratingmat2)
#   #Convert rating matrix into a sparse matrix
#   ratingmat2 <- as(ratingmat2, "realRatingMatrix")
#   # ratingmat2 <- new("dgCMatrix", ratingmat2)
#   # ratingmat2 <- new("realRatingMatrix", data = ratingmat2)
#
#   #Create Recommender Model. "UBCF" stands for user-based collaborative filtering
#   recommender_model <-
#     recommenderlab::Recommender(ratingmat2[-1],
#                 method = "UBCF",
#                 param = list(method = "Cosine", nn = 30)
#                 )
#   recom <- recommenderlab::predict(recommender_model, ratingmat2[1], n = 10)
#   # recom <- predict(recommender_model, ratingmat2[1], n = 25)
#   recom_list <- as(recom, "list")
#   no_result <- data.frame(matrix(NA, 1))
#   recom_result <- data.frame(matrix(NA, 10))
#   # recom_result <- data.frame(matrix(NA, 25))
#   if (as.character(recom_list[1]) == 'character(0)') {
#     no_result[1, 1] <-
#       "Sorry, there is not enough information in our database on the movies you've selected. Try to select different movies you like."
#     colnames(no_result) <- "No results"
#     return(no_result)
#   } else {
#     for (i in c(1:10)) {
#       # for (i in c(1:25)) {
#       recom_result[i, 1] <- as.character(subset(movies,
#                                                 movies$movieId == as.integer(recom_list[[1]][i]))$title)
#     }
#     colnames(recom_result) <-
#       "User-Based Collaborative Filtering Recommended Titles"
#     # browser()
#     return(recom_result)
#   }
# }


chatGPT_R <-
  function(apiKey,
           prompt,
           model = "gpt-3.5-turbo",
           preferences = NULL,
           recommendations = NULL,
           number = 0,
           history = NULL) {
    # if (number == 1) {
    #   prompt <- "You are "
    # }
    # browser()
    if (!is.null(recommendations) & (length(recommendations) > 1)) {
      recs_readable <-
        recommendations[, title]
      recs_readable <- paste(recs_readable, collapse = "; ")

      system_prompt <-
        "You are part of a movie recommender system as a sophisticated conversational interface that may provide explanations to the recommendations given and help in decision-making. In this context you are a helpful and highly proactive as well as inquisitive assistant to the user, who may interact with you once he is shown the top recommendations.
    You will be provided with the user's preferences as well as the recommendations to him. Always try to highlight parallels between the user's preferences and their recommendations based on all your knowledge (only spoil movie plots after explicit permission).
    You should always engage the user with further inquiry helpful to you in reference to their listed preferences, i.e. by you clarifying common genres / the most obvious and prominent movie elements (tropes, themes, plot elements) / attributes (older vs. newer movies, popular movies/classics vs. indie/niche) and whether they like those!
      Regarding specific movie picks, focus on movies recommended to the user by the algorithm. Only deviate from the list if the user voices dissatisfaction. If you do that, explicitly mention that it's not on the list and request the user's permission for recommending something else.
    "
      system_prompt <-
        paste(
          system_prompt,
          "The user liked
        <preferences>",
        paste0(preferences, collapse = ", "),
        "</preferences>
        ",
        "Based on these, the top recommendations to the user (in descending order) are
        <recommendations>",
        recs_readable,
        "</recommendations>
        "
        )
    } else {
      system_prompt <-
        "You are part of a movie recommender system as a sophisticated conversational interface that may provide explanations to the recommendations given. In this context you are a helpful assistant to the user, who may interact with you once he is shown the top recommendations.
      However, the user has not yet given a single preference, but chose to interact with you first. This is not intended. Apologize and Kindly guide him to rate at least one movie on this page. You are not supposed to give recommendations without the user doing this first. Respond briefly."
    }
    if (nrow(history) > 0) {
      # browser()

      # history[, pairs := rep(c(1L, 2L), nrow(history) / 2)]
      # history[, index := 1L:nrow(history)]

      history <- apply(history, 1, function(x){
        # browser()
        list(role = ifelse(x[1] == "Human", "user", "assistant"), content = unname(x[2]))
      })
      # history_rows <- split(x = history, by = "index")
      # c(list(role = "system", content = "system_prompt"), lapply(unname(history_rows), function(x) {
      # # browser()
      #   x <- list(role = x$users, content = x$content)
      # }))
      #
      # history <- lapply(X = history_rows, function(row) {
      #   do.call(list, args = list(
      #     role = ifelse(row[, pairs] == 1L, "user", "assistant"),
      #     content = row[, content]
      #   ))


      # browser()
      # system_prompt <- history

      # df_readable <-
      #   apply(history, 1, function(row)
      #     paste(row['users'], "message:", row['content']))
      # system_prompt <-
      #   paste(
      #     system_prompt,
      #     "Lastly, this is the chat history:
      #     <chat_history>",
      #     paste(df_readable, collapse = " \n "),
      #     "</chat_history>",
      #     collapse = " "
      #   )

      # join all of the lines together into one big string:
      # if (number == 1) {
      #   system_prompt <- paste(system_prompt, df_readable, collapse = " \n ")
      # } else {
      #   system_prompt <- paste(df_readable, collapse = " \n ")
      # }
    }

    # messages <-
    #   list(list(role = "system", content = system_prompt),
    #        list(role = "user", content = prompt))

# browser()
    if (length(history) > 0) {
      # messages <- list(
      #   list(role = "system", content = system_prompt),
      #   history,
      #   list(role = "user", content = prompt)
      # )

      messages <- c(
        list(list(role = "system", content = system_prompt)),
        history,
        list(list(role = "user", content = prompt))
      )
    } else {
      messages <-
        list(list(role = "system", content = system_prompt),
             list(role = "user", content = prompt))
    }

    body <- list(model = model,
                 messages = messages)

    # browser()

    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type("application/json"),
      encode = "json",
      body =    body
    )

    if (status_code(response) > 200) {
      result <- trimws(content(response)$error$message)
    } else {
      result <- trimws(content(response)$choices[[1]]$message$content)
    }

    print(messages)

    # result <- paste("User prompt:", prompt)

    return(result)

  }

execute_at_next_input <-
  function(expr, session = getDefaultReactiveDomain()) {
    observeEvent(once = TRUE,
                 reactiveValuesToList(session$input),
                 {
                   force(expr)
                 },
                 ignoreInit = TRUE)
  }

jscode <-
  'var container = document.getElementById("chat-container");
if (container) {
  var elements = container.getElementsByClassName("user-message");
  if (elements.length > 1) {
    var lastElement = elements[elements.length - 1];
    lastElement.scrollIntoView({
      behavior: "smooth"
    });
  }
}'
