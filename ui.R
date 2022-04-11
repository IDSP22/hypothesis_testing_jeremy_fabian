library(shiny)
library(shinydashboard)

#playerdata <- read.csv("NBA201617.csv", header = TRUE)

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
        strong("Learning objectives:"),
        p("By the end of the module you will be able to answer the following questions:"),
        h4("What is sampling variation?"),
        h4("Why do we need hypothesis testing?"),
        h4("How can we control for sampling variation?"),
        br(),
        p("In this app the goal is to learn about the reasoning of a hypothesis test about proportions. 
          This app uses 2016-2017 data for the NBA regular season."),
        
        br(),
        
        plotOutput('bar_plot'),
        
        br(),
        h4("At first sight it looks Player A is better than Player B. 
           Letâs see if we have all necessary information to make that claim."),
        
        strong("The scenario:"),
        p("You want to compare the performance of two basketball players. 
          We use Free Throws as a measure of performance. The shots can either hit or miss. 
          Free Throws happen in are counted throughout the complete season. Based on the graphic below,"),
        h4("can you tell who is the better player?"),
        
        actionButton('jumpToP2', 'Of course I can!')
        
### SECOND TAB        
      ),
      tabItem(
        tabName = "sampling_explained", value = "sampling_explained",
        
        
        strong("The binomial distribution:"),
        p("A Free Throw can hit or miss. Hit the button to let Player A throw some Free Throws."),
        br(),
        
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
                 plotOutput('free_throws'),
                 
                 br(),
                 p("The resulting distribution is called a binominal distribution, because the Free Throw can only have two values, 
            miss (the red cross) or hit (the green dot). When we want to conduct a hypothesis test we have to start thinking 
            in distributions rather than single values, like a proportion. Can we still compare the players even though 
            Player A threw for example 10 times and Player B 60 times? A comparison of proportion of hits would be misleading 
            in this case because they did not take the same number of shots."),
          )
        ),
        
        br(),
        br(),
        
        strong("Sampling Variation:"),
        p("It would be good to know how high the proportion of hits for the players is if they would throw an infinite number of Free Throws. 
          We could compute their true proportion of hits, regardless of how many shots they throw. 
          We do not know this true proportion, but we can account for that by understanding sampling variation."),
        br(),
        p("If you run the simulation from above again, you will see that every time you will get a slightly different result 
          for the proportion of hits. This is called sampling variation. If we run that simulation a 1000times for every game 
          we will get a slightly different result for the proportion of hits, but on average we get an estimate of the true value. 
          So it is worth looking at the mean. The standard deviation tells us how much variation we have in one season."),
        br(),
        p("Play around with the two sliders of the simulation below and try to answer the following questions."),
        br(),
        h4("Whoâs the better play when both players throw 100 times?"),
        h4("Whoâs the better plater when A throws 10 times and B 50 times?"),
        h4("What happens to the standard deviations when Player A throws 10 times and B 100 times?"),
        
        br(),
        
        strong("Player comparison."),
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
                 plotOutput('shooting_comp'),
                 
                 br(),
                 p("Did you notice that the standard deviation of the sample proportion decreases as sample size increases? 
          The larger the standard deviation the more uncertainty does your observation include. 
          The first bar graph does not account for uncertainties at all. Hypothesis testing is one way to account for 
          uncertainty and sample variation."),
                 
          actionButton('jumpToP3', 'Continue.')
                 
          )
        )
      ),
      
### THIRD TAB  
      
      tabItem(
        tabName = "hypothesis_testing",
        
        strong("Hypothesis Testing"),
        p("In reality we can not let players throw 1000 times, we only have small samples in most cases. 
          Hypothesis testing allows us to make claims about the true proportion of hits for a player based on a low number of samples."),
        br(),
        p("By now you probably have a feeling who is the better player, A or B. 
          Guess the true proportion for Player B and enter it as the Null value in the simulation.
          Then interpret the p-value."),
        br(),
        p("The Hypothesis test simulation now compares your guess with the true value of the player.
          Null hypothesis: Your guess = true value
          Alternative hypothesis: your guess ≠true value."),
        br(),
        p("The p value indicates how likely it is that the sample mean of the proportion of shots aligns with your guess."),



        
        
        fluidRow(
          sidebarPanel(
            selectInput(
              inputId = 'which_player',
              label = 'Select a player:',
              choices = c('Player A', 'Player B')
            ),
            sliderInput(
              inputId = 'nullValue',
              label = 'Select a null value',
              value = 0.5,
              min = 0,
              max = 1, 
              step = 0.1),
            sliderInput(
              inputId = 'n_shots',
              label = 'Number of shots to simulate',
              value = 10,
              min = 10,
              max = 100, 
              step = 10),
            actionButton(inputId = 'sim_ht', label = 'Simulate Free Throws!')
          ), 
          column(width = 8, 
                 DT::dataTableOutput(
                   outputId = "testResults",
                   width = "75%"
                 ),
                 plotOutput("ciPlot", height = "250px")
              )
          )
        )
    )
  )
)
