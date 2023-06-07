library(shiny)
library(proxy)
library(recommenderlab)
library(reshape2)
source("helpercode.R")

library(shinyjs)
library(shinyWidgets)

search <- read.csv("search.csv", stringsAsFactors = FALSE)
ratings <- read.csv("ratings.csv", header = TRUE)
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
  "Film.Noir",
  "Horror",
  "Musical",
  "Mystery",
  "Romance",
  "Sci.Fi",
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
        selectInput("input_genre", "Genre #1",
                    genre_list),
        selectInput("input_genre2", "Genre #2",
                    genre_list),
        selectInput("input_genre3", "Genre #3",
                    genre_list)
        #submitButton("Update List of Movies")
      )
    ),
    column(
      4,
      h3("Select Movies You Like of these Genres:"),
      wellPanel(# This outputs the dynamic UI component
        uiOutput("ui"),
        uiOutput("ui2"),
        uiOutput("ui3"))
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
      textInput("apiKey", "API Key", "sk-xxxxxxxxxxxxxxxxxxxx"),
      selectInput(
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
      "Adventure" = selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Adventure == 1)$title),
        selected = sort(subset(search, Adventure == 1)$title)[1]
      ),
      "Animation" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Animation == 1)$title),
        selected = sort(subset(search, Animation == 1)$title)[1]
      ),
      "Children" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Children == 1)$title),
        selected = sort(subset(search, Children == 1)$title)[1]
      ),
      "Comedy" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Comedy == 1)$title),
        selected = sort(subset(search, Comedy == 1)$title)[1]
      ),
      "Crime" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Crime == 1)$title),
        selected = sort(subset(search, Crime == 1)$title)[1]
      ),
      "Documentary" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Documentary == 1)$title),
        selected = sort(subset(search, Documentary == 1)$title)[1]
      ),
      "Drama" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Drama == 1)$title),
        selected = sort(subset(search, Drama == 1)$title)[1]
      ),
      "Fantasy" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Fantasy == 1)$title),
        selected = sort(subset(search, Fantasy == 1)$title)[1]
      ),
      "Film.Noir" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Film.Noir == 1)$title),
        selected = sort(subset(search, Film.Noir == 1)$title)[1]
      ),
      "Horror" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Horror == 1)$title),
        selected = sort(subset(search, Horror == 1)$title)[1]
      ),
      "Musical" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Musical == 1)$title),
        selected = sort(subset(search, Musical == 1)$title)[1]
      ),
      "Mystery" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Mystery == 1)$title),
        selected = sort(subset(search, Mystery == 1)$title)[1]
      ),
      "Romance" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Romance == 1)$title),
        selected = sort(subset(search, Romance == 1)$title)[1]
      ),
      "Sci.Fi" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Sci.Fi == 1)$title),
        selected = sort(subset(search, Sci.Fi == 1)$title)[1]
      ),
      "Thriller" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Thriller == 1)$title),
        selected = sort(subset(search, Thriller == 1)$title)[1]
      ),
      "War" =  selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, War == 1)$title),
        selected = sort(subset(search, War == 1)$title)[1]
      ),
      "Western" = selectInput(
        "select",
        "Movie of Genre #1",
        choices = sort(subset(search, Western == 1)$title),
        selected = sort(subset(search, Western == 1)$title)[1]
      )
    )
  })

  output$ui2 <- renderUI({
    if (is.null(input$input_genre2))
      return()

    switch(
      input$input_genre2,
      "Action" = selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Action == 1)$title),
        selected = sort(subset(search, Action == 1)$title)[1]
      ),
      "Adventure" = selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Adventure == 1)$title),
        selected = sort(subset(search, Adventure == 1)$title)[1]
      ),
      "Animation" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Animation == 1)$title),
        selected = sort(subset(search, Animation == 1)$title)[1]
      ),
      "Children" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Children == 1)$title),
        selected = sort(subset(search, Children == 1)$title)[1]
      ),
      "Comedy" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Comedy == 1)$title),
        selected = sort(subset(search, Comedy == 1)$title)[1]
      ),
      "Crime" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Crime == 1)$title),
        selected = sort(subset(search, Crime == 1)$title)[1]
      ),
      "Documentary" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Documentary == 1)$title),
        selected = sort(subset(search, Documentary == 1)$title)[1]
      ),
      "Drama" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Drama == 1)$title),
        selected = sort(subset(search, Drama == 1)$title)[1]
      ),
      "Fantasy" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Fantasy == 1)$title),
        selected = sort(subset(search, Fantasy == 1)$title)[1]
      ),
      "Film.Noir" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Film.Noir == 1)$title),
        selected = sort(subset(search, Film.Noir == 1)$title)[1]
      ),
      "Horror" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Horror == 1)$title),
        selected = sort(subset(search, Horror == 1)$title)[1]
      ),
      "Musical" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Musical == 1)$title),
        selected = sort(subset(search, Musical == 1)$title)[1]
      ),
      "Mystery" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Mystery == 1)$title),
        selected = sort(subset(search, Mystery == 1)$title)[1]
      ),
      "Romance" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Romance == 1)$title),
        selected = sort(subset(search, Romance == 1)$title)[1]
      ),
      "Sci.Fi" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Sci.Fi == 1)$title),
        selected = sort(subset(search, Sci.Fi == 1)$title)[1]
      ),
      "Thriller" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Thriller == 1)$title),
        selected = sort(subset(search, Thriller == 1)$title)[1]
      ),
      "War" =  selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, War == 1)$title),
        selected = sort(subset(search, War == 1)$title)[1]
      ),
      "Western" = selectInput(
        "select2",
        "Movie of Genre #2",
        choices = sort(subset(search, Western == 1)$title),
        selected = sort(subset(search, Western == 1)$title)[1]
      )
    )
  })

  output$ui3 <- renderUI({
    if (is.null(input$input_genre3))
      return()

    switch(
      input$input_genre3,
      "Action" = selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Action == 1)$title),
        selected = sort(subset(search, Action == 1)$title)[1]
      ),
      "Adventure" = selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Adventure == 1)$title),
        selected = sort(subset(search, Adventure == 1)$title)[1]
      ),
      "Animation" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Animation == 1)$title),
        selected = sort(subset(search, Animation == 1)$title)[1]
      ),
      "Children" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Children == 1)$title),
        selected = sort(subset(search, Children == 1)$title)[1]
      ),
      "Comedy" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Comedy == 1)$title),
        selected = sort(subset(search, Comedy == 1)$title)[1]
      ),
      "Crime" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Crime == 1)$title),
        selected = sort(subset(search, Crime == 1)$title)[1]
      ),
      "Documentary" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Documentary == 1)$title),
        selected = sort(subset(search, Documentary == 1)$title)[1]
      ),
      "Drama" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Drama == 1)$title),
        selected = sort(subset(search, Drama == 1)$title)[1]
      ),
      "Fantasy" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Fantasy == 1)$title),
        selected = sort(subset(search, Fantasy == 1)$title)[1]
      ),
      "Film.Noir" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Film.Noir == 1)$title),
        selected = sort(subset(search, Film.Noir == 1)$title)[1]
      ),
      "Horror" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Horror == 1)$title),
        selected = sort(subset(search, Horror == 1)$title)[1]
      ),
      "Musical" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Musical == 1)$title),
        selected = sort(subset(search, Musical == 1)$title)[1]
      ),
      "Mystery" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Mystery == 1)$title),
        selected = sort(subset(search, Mystery == 1)$title)[1]
      ),
      "Romance" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Romance == 1)$title),
        selected = sort(subset(search, Romance == 1)$title)[1]
      ),
      "Sci.Fi" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Sci.Fi == 1)$title),
        selected = sort(subset(search, Sci.Fi == 1)$title)[1]
      ),
      "Thriller" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Thriller == 1)$title),
        selected = sort(subset(search, Thriller == 1)$title)[1]
      ),
      "War" =  selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, War == 1)$title),
        selected = sort(subset(search, War == 1)$title)[1]
      ),
      "Western" = selectInput(
        "select3",
        "Movie of Genre #3",
        choices = sort(subset(search, Western == 1)$title),
        selected = sort(subset(search, Western == 1)$title)[1]
      )
    )
  })

  output$table <- renderTable({
    movie_recommendation(input$select, input$select2, input$select3)
  })

  output$dynamic_value <- renderPrint({
    c(input$select, input$select2, input$select3)
  })

  x = reactiveVal(0)
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
          preferences = c(input$select, input$select2, input$select3),
          recommendations = movie_recommendation(input$select, input$select2, input$select3),
          number = x(),
          history = historyALL$df
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
            paste(historyALL$df[x, "content"], "<br/>", "<br/>"))
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
