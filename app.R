library(shiny)
library(tidyverse)
library(reshape)

tennis = read_csv(file = "data/tennis.csv")

ui <- navbarPage(
  title = "Tennis",
  
  tabPanel(
    title = "Visualization",
    titlePanel(title = "Tennis Matches: 1991 - 2016"),
    sidebarLayout(
      sidebarPanel(
        numericInput("year", "Year:",
                     value = 1991,
                     min = 1991,
                     max = 2016,
                     step = 1),
        selectizeInput("player", "Player:",
                       choices = NULL),
        selectizeInput("tourney", "Tournament:",
                       choices = NULL),
        checkboxInput("all_tourneys", "All Tournaments",
                      value = FALSE),
        checkboxInput("all_years", "All Years",
                      value = FALSE)
      ),
      mainPanel(
        plotOutput("plot", hover = hoverOpts(id ="plot_hover",
                                             delayType = "throttle")),
        verbatimTextOutput("hover_info")
      )
    )
  ),
  
  tabPanel(
    title = "Table",
    fluidRow(
      titlePanel("Player Facts"),
      tableOutput("facts"),
      br(),
      titlePanel("Matches"),
      dataTableOutput("table")
    )
  ),
  
  tabPanel(
    title = "About",
    includeMarkdown("about.Rmd")
  )
)

server <- function(input, output, session) {
  # Server-side selectize
  updateSelectizeInput(session, "player",
                       choices = clean(tennis$player_name),
                       server = TRUE)
  updateSelectizeInput(session, "tourney",
                       choices = clean(tennis$tourney_name),
                       server = TRUE)
  
  # Reactives
  year_in = reactive({
    tennis |>
      filter(tourney_year == input$year)
  })
  player_in = reactive({
    year_in() |>
      filter(player_name == input$player)
  })
  
  # Filter Input Choices
  observeEvent(
    eventExpr = input$year,
    handlerExpr = {
      updateSelectizeInput(inputId = "player",
                           choices = clean(year_in()$player_name),
                           selected = choose(clean(year_in()$player_name),
                                             input$player))
      updateSelectizeInput(inputId = "tourney",
                           choices = clean(player_in()$tourney_name),
                           selected = choose(clean(year_in()$tourney_name),
                                             input$tourney))
    }
  )
  observeEvent(
    eventExpr = input$player,
    handlerExpr = {
      updateSelectizeInput(inputId = "tourney",
                           choices = clean(player_in()$tourney_name),
                           selected = choose(clean(player_in()$tourney_name),
                                             input$tourney))
    }
  )
  
  # Plot: closeness of player stats to average
  output$plot = renderPlot({
    data = tennis
    if (!input$all_tourneys) {
      data = filter(data, tourney_name == input$tourney)
    }
    if (!input$all_years) {
      data = filter(data, tourney_year == input$year)
    }
    
    data |> 
      compute_points(input$player) |> 
      ggplot() +
      aes(x=Stat, y=Count, fill=Type) +
      geom_bar(stat="identity", position="identity", color="black", alpha=0.6) +
      coord_flip() +
      theme(text = element_text(size = 20)) +
      scale_fill_manual(values=c("#33CCFF", "#FFFF33"))
  })
  output$hover_info <- renderPrint({
    cat("Hover count:\n")
    str(input$plot_hover$x)
  })
  
  # Table: facts about the player
  output$facts = renderTable({
    tennis |>
      filter(player_name == input$player) |>
      slice_head(n = 1) |> 
      select(player_name, player_id,
             flag_code, residence, birthplace, birth_year,
             turned_pro, weight_lbs, height_inches)
  })
  
  # Table: all matches played for given year and tournament
  output$table = renderDataTable({
    data = tennis
    if (!input$all_tourneys) {
      data = filter(data, tourney_name == input$tourney)
    }
    if (!input$all_years) {
      data = filter(data, tourney_year == input$year)
    }
    
    data |>
      filter(player_name == input$player) |>
      select(match_id, tourney_year, tourney_name, tourney_round_name,
             opponent_name, opponent_player_id,
             result, match_score_tiebreaks, match_duration) |> 
      arrange(tourney_year, tourney_name)
  })
  
}

shinyApp(ui, server)
