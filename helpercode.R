library(proxy)
library(recommenderlab)
library(reshape2)

library(httr)

movies <-
  read.csv("movies.csv", header = TRUE, stringsAsFactors = FALSE)
ratings <- read.csv("ratings.csv", header = TRUE)
movies2 <-
  movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]

movie_recommendation <- function(input, input2, input3) {
  #input = "Gladiator (2000)"
  #input2 = "Aeon Flux (2005)"
  #input3 = "Alexander (2004)"
  row_num <- which(movies2[, 2] == input)
  row_num2 <- which(movies2[, 2] == input2)
  row_num3 <- which(movies2[, 2] == input3)
  userSelect <- matrix(NA, 10325)
  userSelect[row_num] <- 5 #hard code first selection to rating 5
  userSelect[row_num2] <- 4 #hard code second selection to rating 4
  userSelect[row_num3] <- 3 #hard code third selection to rating 3
  userSelect <- t(userSelect)

  ratingmat <-
    dcast(ratings,
          userId ~ movieId,
          value.var = "rating",
          na.rm = FALSE)
  ratingmat <- ratingmat[, -1]
  colnames(userSelect) <- colnames(ratingmat)
  ratingmat2 <- rbind(userSelect, ratingmat)
  ratingmat2 <- as.matrix(ratingmat2)

  #Convert rating matrix into a sparse matrix
  ratingmat2 <- as(ratingmat2, "realRatingMatrix")

  #Create Recommender Model. "UBCF" stands for user-based collaborative filtering
  recommender_model <-
    Recommender(ratingmat2,
                method = "UBCF",
                param = list(method = "Cosine", nn = 30))
  recom <- predict(recommender_model, ratingmat2[1], n = 10)
  # recom <- predict(recommender_model, ratingmat2[1], n = 25)
  recom_list <- as(recom, "list")
  no_result <- data.frame(matrix(NA, 1))
  recom_result <- data.frame(matrix(NA, 10))
  # recom_result <- data.frame(matrix(NA, 25))
  if (as.character(recom_list[1]) == 'character(0)') {
    no_result[1, 1] <-
      "Sorry, there is not enough information in our database on the movies you've selected. Try to select different movies you like."
    colnames(no_result) <- "No results"
    return(no_result)
  } else {
    for (i in c(1:10)) {
      # for (i in c(1:25)) {
      recom_result[i, 1] <- as.character(subset(movies,
                                                movies$movieId == as.integer(recom_list[[1]][i]))$title)
    }
    colnames(recom_result) <-
      "User-Based Collaborative Filtering Recommended Titles"
    # browser()
    return(recom_result)
  }
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
    if (!is.null(recommendations)) {
      recs_readable <-
        apply(recommendations, 1, function(row)
          paste(row['User-Based Collaborative Filtering Recommended Titles']))
      recs_readable <- paste(recs_readable, collapse = "; ")
    }
    system_prompt <-
      "You are part of a movie recommender system interface that may provide explanations to the recommendations given. In this context you are a helpful assistant to the user, who may interact with you once he is shown the top k results. You will be provided with the user's preferences as well as the recommendations to him. Always try to highlight parallels between the user's preferences and their recommendations based on all your knowledge, but only spoil movie plots after explicit permission. Respond briefly if possible."
    system_prompt <-
      paste(
        system_prompt,
        "The user liked <preferences>",
        paste0(preferences, collapse = ", "),
        "</preferences>",
        "Based on these, the top k recommendations to the user (in descending order) are <recommendations>",
        recs_readable,
        "</recommendations>"
      )
    if (!is.null(history)) {
      # browser()
      # system_prompt <- history
      df_readable <-
        apply(history, 1, function(row)
          paste(row['users'], "message:", row['content']))
      system_prompt <-
        paste(
          system_prompt,
          "Lastly, this is the chat history: <chat_history>",
          paste(df_readable, collapse = " \n "),
          "</chat_history>",
          collapse = " "
        )
      # join all of the lines together into one big string:
      # if (number == 1) {
      #   system_prompt <- paste(system_prompt, df_readable, collapse = " \n ")
      # } else {
      #   system_prompt <- paste(df_readable, collapse = " \n ")
      # }
    }

    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type("application/json"),
      encode = "json",
      body = list(model = model,
                  messages = list(
                    list(role = "system", content = system_prompt),
                    list(role = "user", content = prompt)
                  ))
    )

    if (status_code(response) > 200) {
      result <- trimws(content(response)$error$message)
    } else {
      result <- trimws(content(response)$choices[[1]]$message$content)
    }

    print("System Prompt:")
    print(system_prompt)
    print("User Prompt:")
    print(prompt)

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
