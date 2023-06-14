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
           input4 = NA,
           input5 = NA,
           movies_data = movies,
           ratings_data = ratings) {
    movies <- copy(movies_data)
    movies[, new_movieId := (match(movieId, unique(movieId)) - 1)]
    ratings <- copy(ratings_data)
    setkey(movies, movieId)
    ratings[movies, on = .(movieId), movieId := i.new_movieId]
    setnames(
      movies,
      old = c("movieId", "new_movieId"),
      new = c("old_movieId", "movieId")
    )
    setkey(movies, movieId)
    # ratings <- ratings[sample(.N, 10000000)]

    input1_id <- movies[title %in% input1, movieId]
    input2_id <- movies[title %in% input2, movieId]
    input3_id <- movies[title %in% input3, movieId]
    input4_id <- movies[title %in% input4, movieId]
    input5_id <- movies[title %in% input5, movieId]

    if (length(input1_id) == 0)
      input1_id <- NA
    if (length(input2_id) == 0)
      input2_id <- NA
    if (length(input3_id) == 0)
      input3_id <- NA
    if (length(input4_id) == 0)
      input4_id <- NA
    if (length(input5_id) == 0)
      input5_id <- NA

    inputs <-
      c(input1_id, input2_id, input3_id, input4_id, input5_id)
    # if (all(!is.na(inputs))) {
    #   browser()
    # }
    if (all(is.na(inputs))) {
      no_result <- data.frame(matrix(NA, 1))
      no_result[1, 1] <-
        "Sorry, there is not enough information in our database on the movies you've selected. Try to select different movies you like."
      colnames(no_result) <- "No results"
      return(no_result)
    }

    user_vote <- data.table(userId = 0L,
                            movieId = inputs[!is.na(inputs)],
                            rating = 5 - 3.680596)
    # browser()
    movie_indices <- sort(ratings[, unique(movieId)])

    missing_votes <- setdiff(movie_indices, inputs)

    ratings[, rating := rating - mean(rating), by = userId]
    ratings[, userId := match(userId, unique(userId))]

    ratings <- rbindlist(list(user_vote, ratings))
    setorder(ratings, userId)

    train_set <- recosystem::data_memory(
      user_index = ratings$userId,
      item_index = ratings$movieId,
      rating = ratings$rating,
      index1 = FALSE
    )

    rec <- Reco()
    # set.seed(123)
    rec$train(
      train_set,
      out_model = NULL,
      opts = list(
        # dim = 40L,
        # costp_l1 = 0.033,
        # costp_l2 = 0.1,
        # costq_l1 = 0,
        # costq_l2 = 0.033,
        # lrate = 0.1,
        nthread = 8,
        niter = 10
      )
    )


    # test_set <-
    #   recosystem::data_memory(
    #     user_index = c(rep(0L, length(missing_votes))),
    #     item_index = missing_votes,
    #     rating = rep(NA, times = length(missing_votes)),
    #     index1 = FALSE
    #   )

    # browser()
    test_ratings_missing <-
      data.table(
            userId = c(rep(0L, length(missing_votes))),
            movieId = missing_votes,
            rating = rep(NA, times = length(missing_votes))
      )
    test_set_dt <- rbindlist(list(user_vote, test_ratings_missing))
    setorder(test_set_dt, movieId)



    test_set <-
      recosystem::data_memory(
        user_index = test_set_dt$userId,
        item_index = test_set_dt$movieId,
        rating = test_set_dt$rating,
        index1 = FALSE
      )

    prediction <- rec$predict(test_set, out_memory())

    # prediction <- data.table(movieId = missing_votes,
    #                          pred = prediction)
    prediction <- data.table(movieId = test_set_dt$movieId,
                             pred = prediction)

    prediction <-
      merge(prediction, movies[, .(movieId, title)], by = "movieId")
    prediction <- prediction[movieId %in% missing_votes]
    setorderv(prediction, cols = "pred", order = -1)

    prediction <- head(prediction, n = 20L)
    return(prediction)
  }


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

      #   system_prompt <-
      #     "You are part of a movie recommender system as a sophisticated conversational interface that may provide explanations to the recommendations given and help in decision-making. In this context you are a helpful and highly proactive as well as inquisitive assistant to the user, who may interact with you once he is shown the top recommendations.
      # You will be provided with the user's preferences as well as the recommendations to him. Always try to highlight parallels between the user's preferences and their recommendations based on all your knowledge (only spoil movie plots after explicit permission).
      # You should always engage the user with further inquiry helpful to you in reference to their listed preferences, i.e. by you clarifying common genres / the most obvious and prominent movie elements (tropes, themes, plot elements) / attributes (older vs. newer movies, popular movies/classics vs. indie/niche) and whether they like those!
      #   Regarding specific movie picks, focus on movies recommended to the user by the algorithm. Only deviate from the list if the user voices dissatisfaction. If you do that, explicitly mention that it's not on the list and request the user's permission for recommending something else.
      # "

      system_prompt <-
        "You're an integral part of our movie recommender system as an interactive interface, providing explanatory context for our recommendations. Your role is to assist the user in decision-making, leveraging their preferences against the system's suggestions. Be proactive, curious, and always seek to understand more about the user's likes and dislikes, whether that's genres, key movie elements, or film eras and types. Note that you should only disclose movie plot details with explicit user permission.

Important: You must only recommend those movies that are within the `your_recommendations` tag. You may only suggest alternatives if the user expresses discontent with those recommendations, always seek their approval before doing so, and explicitly mention if movies are not on the list. NEVER suggest movies the user has already watched and rated."
      system_prompt <-
        paste(
          system_prompt,
          "The following movies were rated 5/5 by the user:
        <user_loves>",
        paste0(preferences, collapse = ", "),
        "</user_loves>
        ",
        "Based on these ratings, the user should like these movies (in descending order)
        <your_recommendations>",
        recs_readable,
        "</your_recommendations>
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

      history <- apply(history, 1, function(x) {
        # browser()
        list(role = ifelse(x[1] == "Human", "user", "assistant"),
             content = unname(x[2]))
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

      messages <-
        c(list(list(role = "system", content = system_prompt)),
          history,
          list(list(role = "user", content = prompt)))
    } else {
      messages <-
        list(list(role = "system", content = system_prompt),
             list(role = "user", content = prompt))
    }

    body <- list(model = model,
                 messages = messages)

    browser()

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
      result <- content(response)$choices[[1]]$message$content
      result <- gsub("\\n", "<br/>", result)
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
