#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)

#temp_data <- write_csv(tibble(words = "blank"), "bowls_words.csv")

ui <- dashboardPage(
  dashboardHeader(title = "BOWLS!"),
  dashboardSidebar(sidebarMenu(
    menuItem("Submit Words",  tabName =  "submission", 
             icon = icon("paper-plane")), 
    menuItem( "Play Game", tabName = "play_game",
              icon = icon("play"), 
              selected = 1)
  )), #close dashboardsidebar
  dashboardBody(
                tabItems(
                  tabItem(tabName = "submission",
                fluidRow(box(width = 6, title = "Enter bowls list", 
                             solidHeader = T,# height = 500, 
                             status = "primary",  collapsible = T,    
                             
                textInput("name", "Your name:"), 
                tags$br(),
                tags$hr(),
                tags$br(), 
                textInput("word1", "Enter first word: "), 
                textInput("word2", "Enter second word: "), 
                textInput("word3", "Enter third word: "), 
                textInput("word4", "Enter fourth word: "), 
                textInput("word5", "Enter fifth word: "), 
                textInput("word6", "Enter sixth word: "), 
                textInput("word7", "Enter seventh word: "),
    actionButton("submit", "Submit Words!", icon = icon("paper-plane"), 
                 style = "color: white; 
                                       background-color: #1F65CC; 
                                       position: relative;
                                       height: 35px;
                                       width: 200px;")))),
    tabItem(tabName = "play_game",
            h3("Let's play bowls!!"),
         uiOutput("bowls_game"),
          uiOutput("score")))
  ) # dashboard body close
) #dashboard page close 

server <- function(input, output, session) {
  
  observeEvent(input$submit, {    
    showModal(modalDialog(paste0(input$name, ", your words for bowls have been entered!!!"),
                          footer=NULL))
    all_inputs <- reactiveValuesToList(input)
    
    input_words <- str_detect(names(all_inputs), 
                                     "word")
    keep_words <- unlist(all_inputs[input_words])
    

    write_csv(tibble(words = keep_words), paste0(input$name, "_bowls.csv"))
    
    # save names: 
    input_name <- str_detect(names(all_inputs), 
                              "name")
    
    write_csv(tibble(player = unlist(all_inputs[input_name])), 
              paste0(input$name, "names.csv"))
    Sys.sleep(2)
    #delay(100, removeModal())
    removeModal()
    updateTextInput(session, "name", value = "")
    updateTextInput(session,"word1", value = "")
    updateTextInput(session,"word2", value = "")
    updateTextInput(session,"word3", value = "") 
    updateTextInput(session,"word4", value = "") 
    updateTextInput(session,"word5", value = "") 
    updateTextInput(session,"word6", value = "") 
    updateTextInput(session,"word7", value = "")

  }) 

    
 #reactive data values, updated and used throughout  
  remaining_words <- reactiveValues(data = NULL)
  word_showing <- reactiveValues(data = NULL)
  score <- reactiveValues(team1 = 0, team2 = 0)
  turn_count <- reactiveValues(begin_count = NULL)
  turns <- reactiveValues(begin = 0, end = 0)
  teams <- reactiveValues(one = "Create", two = "Teams")
  
  observeEvent({input$restart_list}, {
    if(length(list.files()[str_detect(list.files(), "bowls.csv")]) == 0) {
          NULL
        } else {
          remaining_words$data  <- (list.files()[str_detect(list.files(), "bowls.csv")]  %>%
                                      map_df(read_csv) %>% pull(words))
          word_showing$data <- NULL
         # turns$begin <- 0
         # turns$end <- 0
            #write_csv(temp, "remaining_words.csv")
        #temp %>% pull(words)
        }
    
    
  })
  
  ### start New game #####
  observeEvent({input$start_new_game}, 
               {
               list.files()[str_detect(list.files(), "csv")]  %>%
                 map(file.remove)
                remaining_words$data <- NULL
                word_showing$data <- NULL
                score$team1 <- 0
                score$team2 <- 0
                teams$one <- "Create"
                teams$two <- "Teams"
               }
)
  
  observeEvent(input$create_teams, {
    # reset if teams already selected
     teams$one <- NULL
     teams$two <- NULL
    if(length(list.files()[str_detect(list.files(), "names.csv")]) == 0) {
      NULL
    } else {
      all.names  <- (list.files()[str_detect(list.files(), "names.csv")]  %>%
                                  map_df(read_csv) %>% pull(player))

      ids <- sample(length(all.names), size = ceiling(length(all.names)/2))
    
      teams$one <- all.names[ids]
      teams$two <- all.names[-ids]
      # turns$end <- 0
      #write_csv(temp, "remaining_words.csv")
      #temp %>% pull(words)
    }
    
  })
  output$bowls_game <- renderUI({

    
    box(width = 9, title = "Enter bowls list", 
        solidHeader = T,# height = 500, 
        status = "primary",  collapsible = T, 
        actionButton("restart_list", "Start/Restart list (ONLY PRESS at the BEGINNING of each round)",
                     style = "color: white; 
                                       background-color: black; 
                                       position: relative;
                                       height: 35px;
                                       width: 400px;"),
        tags$h5(paste0("There are ", length(remaining_words$data), " words remaining in the bowl.")), 

        tags$br(),
       # if(file.exists("remaining_words.csv")) {
        actionButton("remaining", "Reload remaining words & update score (press before you begin your turn)",
                     style = "color: white; 
                                       background-color: green; 
                                       position: relative;
                                       height: 35px;
                                       width: 600px;"),#},
        tags$hr(), 
        tags$b(h3(textOutput("guess_this"))), 
        tags$hr(), 
        tags$br(),
      if(turns$begin > turns$end) {
        tagList(
          actionButton("correct", "Next (correct answer)",
                       style = "color: white; 
                                       background-color: green; 
                                       position: relative;
                                       height: 35px;
                                       width: 200px;"), 
          actionButton("pass", "Pass",   style = "color: white;
                                    background-color: red;
                                    position: relative;
                                    height: 35px;
                                    width: 200px;"))} else {
        radioButtons("team_select", "Who's turn?",
                     choices = c("Team 1", "Team 2"), selected = "Team 1")}, 
      tags$br(), 
      tags$hr(), 
      if(turns$begin == turns$end) { 
      actionButton("begin_turn", "Begin Turn",   style = "color: white; 
                                       background-color: green; 
                                       position: relative;
                                       height: 35px;
                                       width: 200px;") },
      if(turns$begin > turns$end) {
      actionButton("end_turn", "End Turn",   style = "color: white; 
                                       background-color: blue; 
                                       position: relative;
                                       height: 35px;
                                       width: 200px;")})
    
  })
  
  observeEvent(input$remaining, {
    if(file.exists("remaining_words.csv")) {
    updateRadioButtons(session, "team_select", selected = paste("Team", (turns$end %% 2) + 1))
    remaining_words$data <- read.csv("remaining_words.csv") %>% pull(words)
    if(file.exists("score.csv")) {
      scores.dta <- read_csv("score.csv")
      score$team1 <- scores.dta %>% pull(`Team 1`)
      score$team2 <- scores.dta %>% pull(`Team 2`)
    } else {
      score$team1 <- 0
      score$team2 <- 0
    }
    } 
  })
  
  observeEvent(input$correct, 
                   { 
   
              
                if(!is.null(word_showing$data)) {
                  remaining_words$data <- remaining_words$data[remaining_words$data != word_showing$data]
                }
               
               
               if(length(remaining_words$data) == 0) {
                 word_showing$data = "There are no remaining words."
               } else {
                 word_showing$data <- sample(remaining_words$data, size = 1)
               }
                 
               })
  
  
  output$guess_this <- reactive({

      if(is.null(word_showing$data)) {
        if(length(remaining_words$data) == 0) {
          "There are no remaining words in the queue, press restart or submit new words" } else {
            "Press Begin Turn"
          }
      } else {
        word_showing$data
      }
    
  })
  
  
  observeEvent({input$pass}, {
               if(!is.null(input$begin_turn)) { 
                 if(length(remaining_words$data) == 0) {
                   word_showing$data = "There are no remaining words."
                 } else {
                   word_showing$data <- sample(remaining_words$data, size = 1)
                 }
                 
               }
    })
  
  observeEvent(input$begin_turn, {
    if(length(remaining_words$data) == 0) {
      word_showing$data = "There are no remaining words."
    } else {
      word_showing$data <- sample(remaining_words$data, size = 1)
    }
    turn_count$begin_count <- length(remaining_words$data)
    turns$begin <- 1 + turns$begin
  })
  observeEvent(input$end_turn, {
    if(file.exists("score.csv")) {
      scores.dta <- read_csv("score.csv")
      score$team1 <- scores.dta %>% pull(`Team 1`)
      score$team2 <- scores.dta %>% pull(`Team 2`)
    }
    
    
    if(input$team_select == "Team 1") {
      score$team1 <- score$team1 + turn_count$begin_count - length(remaining_words$data)
    } else {
      score$team2 <- score$team2 + turn_count$begin_count - length(remaining_words$data)
    }
    
    if(!is.null(remaining_words$data)) {
    write.csv(tibble(words = remaining_words$data), "remaining_words.csv")
    write.csv(tibble("Team 1" = score$team1, "Team 2" = score$team2),
              "score.csv", 
              row.names = FALSE)
    }
    word_showing$data <- "Press Begin Turn (with correct team selected)"
    turn_count$begin_count <- 0
    turns$end <- 1 + turns$end
  })
  

# Scoring the game 
  output$score <- renderUI({
    
    box(width = 2, title = "Score", 
        solidHeader = T,# height = 500, 
        status = "primary", 
        actionButton("create_teams", "Create Teams",
                     style = "color: white; 
                                       background-color: blue; 
                                       position: relative;
                                       height: 35px;"), 
                                      # width: 200px;"),
        tags$h3(tags$b("Score")),
        tags$br(), 
        DT::dataTableOutput("score_table"), 
       # DT::dataTableOutput("name_table"),
        # tags$div(
        #   tags$p(tags$b("Team 1: "), score$team1), 
        #   tags$p(tags$b("Team 2: "), score$team2)
        # ),
        tags$br(), 
        tags$br(),
   
     actionButton("start_new_game", "New Game",
                  style = "color: white; 
                                       background-color: red; 
                                       position: relative;
                                       height: 35px;
                                       width: 100px;"))
  })
  
  output$score_table <- DT::renderDataTable({
    print(input$start_new_game)
    if(length(teams$one) == length(teams$two)) {
    tibble("Team 1" = c(score$team1, teams$one), 
           "Team 2" = c(score$team2, teams$two))
    } else {
      tibble("Team 1" = c(score$team1, teams$one), 
             "Team 2" = c(score$team2, teams$two, NA))
    }
  },
  #tibble("Team 1" = score$team1, "Team 2" = score$team2),
  options = list(dom = "t", autoWidth = TRUE), rownames = FALSE, escape = FALSE)

  
  # output$name_table <- DT::renderDataTable({
  #   print(input$start_new_game)
  #   print(input$create_teams)
  #   tibble("Team 1" = teams$one, "Team 2" = teams$two)
  # },
  # #tibble("Team 1" = score$team1, "Team 2" = score$team2),
  # options = list(dom = "t", autoWidth = TRUE), rownames = FALSE, escape = FALSE)
  # 

}




shinyApp(ui, server)
