library(shiny)
library(proxy)
library(recommenderlab)
library(reshape2)
source("helpercode.R")

library(shinyjs)
library(shinyWidgets)

# search2 <- read.csv("search.csv", stringsAsFactors = FALSE)
search <- movies[genres != "(no genres listed)"]
suppressWarnings({search <-
  search[, list(
    movieId = movieId,
    title = title,
    year = as.numeric(gsub(".*\\((\\d{4})\\)$", "\\1", title)),
    genre = unlist(strsplit(genres, "\\|"))
  ), by = 1:nrow(search)]})
search <- search[!is.na(year)]
search <-
  dcast(search, movieId + title + year ~ genre, fun.aggregate = length)
setnames(
  search,
  old = c("Film-Noir", "Sci-Fi"),
  new = c("Film_Noir", "Sci_Fi")
)

# ratings <- read.csv("ratings.csv", header = TRUE)
search <-
  search[-which((search$movieId %in% ratings$movieId) == FALSE), ]

formatInput <- function(v, a, d) {
  ## This function formats the user's input of Valence-Arousal-Dominance
  ## and outputs them as a vector

  c(v, a, d)
}

genre_list <- c(
  "Action",
  "Adventure",
  "Animation",
  "Children",
  "Comedy",
  "Crime",
  "Documentary",
  "Drama",
  "Fantasy",
  "Film_Noir",
  "Horror",
  "Musical",
  "Mystery",
  "Romance",
  "Sci_Fi",
  "Thriller",
  "War",
  "Western"
)

ui <- fluidPage(
  titlePanel("Movie Recommendation System"),
  # sidebarLayout(mainPanel(
  fluidRow(
    column(
      4,
      h3("Select Movie Genres You Prefer (order matters):"),
      wellPanel(
        pickerInput("input_genre", "Genre #1",
                    genre_list),
        pickerInput("input_genre2", "Genre #2",
                    genre_list),
        pickerInput("input_genre3", "Genre #3",
                    genre_list),
        pickerInput("input_genre4", "Genre #4",
                    genre_list),
        pickerInput("input_genre5", "Genre #5",
                    genre_list)
        #submitButton("Update List of Movies")
      )
    ),
    column(
      4,
      h3("Select Movies You Like of these Genres:"),
      wellPanel(
        # This outputs the dynamic UI component
        uiOutput("ui"),
        uiOutput("ui2"),
        uiOutput("ui3"),
        uiOutput("ui4"),
        uiOutput("ui5")
      )
    ),
    #submitButton("Get Recommendations")))))),
    column(
      4,
      h3("You Might Like The Following Movies Too!"),
      tableOutput("table")
      #verbatimTextOutput("dynamic_value")
    )
  ),
  fluidRow(
    column(
      2,
      textInput("apiKey", "API Key", "sk-xyz"),
      pickerInput(
        "model",
        "Model",
        choices = c("gpt-3.5-turbo", "gpt-4"),
        selected = "gpt-3.5-turbo"
      )
    ),
    column(
      6,
      uiOutput("chatThread"),
      textAreaInput(
        inputId = "prompt",
        label = "",
        placeholder = "Type your message here...",
        width = "100%"
      )
    )
  ),
  fluidRow(column(2, br()), column(
    6,
    actionButton(
      inputId = "submit",
      label = "Send",
      icon = icon("paper-plane")
    ),
    actionButton(
      inputId = "remove_chatThread",
      label = "Clear History",
      icon = icon("trash-can")
    )
    # ))
  ))
  # uiOutput("chatThread")
)


server <- function(input, output, session) {
  historyALL <- reactiveValues(df = data.frame() , val = character(0))

  output$ui <- renderUI({
    if (is.null(input$input_genre))
      return()

    switch(
      input$input_genre,
      "Action" = pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Action == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Adventure" = pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Adventure == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Animation" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Animation == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Children" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Children == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Comedy" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Comedy == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Crime" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Crime == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Documentary" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Documentary == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Drama" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Drama == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Fantasy" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Fantasy == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Film_Noir" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Film_Noir == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Horror" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Horror == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Musical" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Musical == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Mystery" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Mystery == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Romance" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Romance == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Sci_Fi" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Sci_Fi == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Thriller" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Thriller == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "War" =  pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, War == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Western" = pickerInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Western == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      )
    )
  })

  output$ui2 <- renderUI({
    if (is.null(input$input_genre2))
      return()

    switch(
      input$input_genre2,
      "Action" = pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Action == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Adventure" = pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Adventure == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Animation" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Animation == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Children" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Children == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Comedy" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Comedy == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Crime" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Crime == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Documentary" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Documentary == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Drama" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Drama == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Fantasy" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Fantasy == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Film_Noir" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Film_Noir == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Horror" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Horror == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Musical" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Musical == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Mystery" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Mystery == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Romance" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Romance == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Sci_Fi" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Sci_Fi == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Thriller" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Thriller == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "War" =  pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, War == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Western" = pickerInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Western == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      )
    )
  })

  output$ui3 <- renderUI({
    if (is.null(input$input_genre3))
      return()

    switch(
      input$input_genre3,
      "Action" = pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Action == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Adventure" = pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Adventure == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Animation" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Animation == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Children" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Children == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Comedy" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Comedy == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Crime" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Crime == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Documentary" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Documentary == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Drama" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Drama == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Fantasy" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Fantasy == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Film_Noir" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Film_Noir == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Horror" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Horror == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Musical" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Musical == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Mystery" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Mystery == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Romance" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Romance == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Sci_Fi" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Sci_Fi == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Thriller" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Thriller == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "War" =  pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, War == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Western" = pickerInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Western == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      )
    )
  })

  output$ui4 <- renderUI({
    if (is.null(input$input_genre4))
      return()

    switch(
      input$input_genre4,
      "Action" = pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Action == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Adventure" = pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Adventure == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Animation" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Animation == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Children" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Children == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Comedy" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Comedy == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Crime" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Crime == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Documentary" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Documentary == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Drama" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Drama == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Fantasy" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Fantasy == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Film_Noir" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Film_Noir == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Horror" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Horror == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Musical" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Musical == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Mystery" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Mystery == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Romance" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Romance == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Sci_Fi" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Sci_Fi == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Thriller" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Thriller == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "War" =  pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, War == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Western" = pickerInput(
        "select4",
        "Movie of Genre #4",
        choices = sort(subset(search, Western == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      )
    )
  })

  output$ui5 <- renderUI({
    if (is.null(input$input_genre5))
      return()

    switch(
      input$input_genre5,
      "Action" = pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Action == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Adventure" = pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Adventure == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Animation" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Animation == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Children" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Children == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Comedy" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Comedy == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Crime" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Crime == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Documentary" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Documentary == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Drama" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Drama == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Fantasy" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Fantasy == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Film_Noir" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Film_Noir == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Horror" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Horror == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Musical" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Musical == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Mystery" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Mystery == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Romance" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Romance == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Sci_Fi" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Sci_Fi == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Thriller" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Thriller == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "War" =  pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, War == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      ),
      "Western" = pickerInput(
        "select5",
        "Movie of Genre #5",
        choices = sort(subset(search, Western == 1)$title),
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                maxOptions = 1L)
      )
    )
  })

  recommendations <-
    reactive({
      movie_recommendation(input$select,
                           input$select2,
                           input$select3,
                           input$select4,
                           input$select5)
    })
  output$table <- renderTable({
    recommendations()
  })

  output$dynamic_value <- renderPrint({
    c(input$select,
      input$select2,
      input$select3,
      input$select4,
      input$select5)
  })

  x <- reactiveVal(0)
  observeEvent(input$submit, {
    if (nchar(trimws(input$prompt)) > 0) {
      x(x() + 1) # increment x by 1
    }
  })

  historyALL$df <- NULL
  observeEvent(input$submit, {
    if (nchar(trimws(input$prompt)) > 0) {
      print(x())
      chatGPT <-
        chatGPT_R(
          input$apiKey,
          input$prompt,
          input$model,
          preferences = c(
            input$select,
            input$select2,
            input$select3,
            input$select4,
            input$select5
          ),
          recommendations = recommendations(),
          number = x(),
          history = as.data.table(historyALL$df)
        )
      historyALL$val <- chatGPT
      history <- data.frame(
        users = c("Human", "AI"),
        # content = c(input$prompt, markdown::mark_html(text =
        #                                                 chatGPT)),
        content = c(input$prompt, chatGPT),
        stringsAsFactors = FALSE
      )
      historyALL$df <- rbind(historyALL$df, history)
      updateTextInput(session, "prompt", value = "")

      output$chatThread <- renderUI({
        conversations <- lapply(seq_len(nrow(historyALL$df)), function(x) {
          # tags$div(
          #   class = ifelse(
          #     historyALL$df[x, "users"] == "Human",
          #     "user-message",
          #     "bot-message"
          #   ),
          HTML(#               paste0(
            #               ifelse(
            #                 historyALL$df[x, "users"] == "Human",
            #                 "
            # <img src='girl.avif' class='img-wrapper2'>
            # ",
            # "
            # <img src='boy.avif' class='img-wrapper2'>
            # "
            #               ),
            paste0(
              ifelse(x %% 2 == 0, yes = "<b>Assistant:</b><br/>", no = "<b>You:</b><br/>"),
              historyALL$df[x, "content"],
              "<br/>",
              "<br/>"
            ))
          # )
          # )
        })
        # do.call(tagList, conversations)
        return(conversations)
      })
      # w$hide()
      execute_at_next_input(runjs(jscode))

    }

  })

  observeEvent(input$remove_chatThread, {
    output$chatThread <- renderUI({
      return(NULL)
    })
    historyALL$df <- NULL
    updateTextInput(session, "prompt", value = "")
  })



}

shinyApp(ui = ui, server = server)
