library(shiny)
library(shinydashboard)

playerdata <- read.csv("NBA201617.csv", header = TRUE)

#  "ui file must return UI object", perform ui construction:
dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Hypothesis Testing with NBA data", titleWidth = 300),
  
  # Sidebar
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Sampling Explained", tabName = "sampling_explained", icon = icon("chart-area")),
      menuItem("Hypothesis Testing", tabName = "hypothesis_testing", icon = icon("cogs"))
    )
  ),
  
  # Content within the tabs
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
    ),
    tabItems(
      tabItem(
        tabName = "overview",
        "Welcome to the app"
      ),
      tabItem(
        tabName = "sampling_explained",
        "Player A shoots 60%, simulate some free throws",
        fluidRow(
          sidebarPanel(
            sliderInput(
              inputId = 'n_freethrows',
              label = 'Number of Free Throws Shot',
              value = 10,
              min = 1,
              max = 50, 
              step = 10),
            actionButton(inputId = 'sim_ft', label = 'Simulate Free Throws!')
          ), 
          column(width = 8, 
                 plotOutput('free_throws')
          )
        ),
        "Player comparison. A shoots 60% and B 50% truly.",
        fluidRow(
          sidebarPanel(
            sliderInput(
              inputId = 'a_freethrows',
              label = 'Player A Free Throw Attempts:',
              value = 10,
              min = 10,
              max = 100, 
              step = 10),
            sliderInput(
              inputId = 'b_freethrows',
              label = 'Player B Free Throw Attempts:',
              value = 10,
              min = 10,
              max = 100, 
              step = 10),
            actionButton(inputId = 'sim_both', label = 'Simulate Both Players!')
          ), 
          column(width = 8, 
                 plotOutput('shooting_comp')
          )
        )
      ),

      tabItem(
        tabName = "hypothesis_testing",
      )
    )
  )
)
