library(ggplot2)
library(reshape2)
library(dplyr)
library(shiny)

APP_TITLE <- "Hypothesis Testing with NBA data"

place_holder <- function(){
  ggplot(x = seq(0, 1, .1), y = seq(0, 1, .1)) + 
    scale_x_continuous(breaks=seq(0,1,.05), limits = c(-.01, 1.01), expand = expansion()) + 
    scale_y_continuous(labels = NULL, breaks = NULL) + 
    labs(title = 'Simulate to See Results!') + 
    theme_classic() + 
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  
}

#  "server file must return a server function", create server:
function(input, output, session) {
  store <- reactiveValues()
  
  observeEvent(input$sim_ft, {
    store$ft_sims <- rbinom(input$n_freethrows, 1, 0.6)
  })
  
  observeEvent(input$sim_both, {
    store$player_a <- rbinom(1, input$a_freethrows, 0.6)
    store$player_b <- rbinom(1, input$b_freethrows, 0.4)
  })
  
  observeEvent(input$sim_ht, {
    if (input$which_player == 'Player A'){
      store$simmed_player <- rbinom(1, input$n_shots, 0.6)
    } else{
      store$simmed_player <- rbinom(1, input$n_shots, 0.4)
    }
  })
  
  output$free_throws <- renderPlot({
    if (length(store$ft_sims) > 1){
      y <- c(1:length(store$ft_sims))
      x <- store$ft_sims
      dat <- data.frame(x = x, y=y)
      dat <- dat %>%
        mutate(label = ifelse(x == 1, "make", "miss"))
      
      makes <- sum(dat$x)
      misses <- length(dat$x) - makes
      sd_sp <- sqrt( makes/length(dat$x) * (1 - (makes/ length(dat$x))) / (length(dat$x))) #standard deviation of sample proportion
      subtitle <- paste(makes, "Makes, ", misses, "Misses,", round(sd_sp,2), "Standard deviation of sample proportion")
      
      p <- ggplot(data = dat, aes(x=y,y=x)) + 
        geom_point(aes(colour = factor(label), shape = factor(label)), size = 5) +
        scale_y_continuous(limits = c(-5000, 5000)) + 
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank() 
        ) + scale_color_manual(values=c("#39FF14", "Red")) +
        scale_shape_manual(values = c(20, 4)) +
        labs(title = "Free Throws Simulated",
             subtitle = subtitle)
    } else{
      p <- place_holder()
    }
    return(p)
  })
  
  output$shooting_comp <- renderPlot({
    if (length(store$player_a) == 1){
      a_se <- sqrt((.6 * (1-.6))/ input$a_freethrows) #standard deviation of the sample proportion
      b_se <- sqrt((.4 * (1-.4))/ input$b_freethrows)
      a_ft <- round(store$player_a/input$a_freethrows, 3)
      b_ft <- round(store$player_b/input$b_freethrows, 3)
      dat <-data.frame(Shooting_percentage = c(a_ft, b_ft),
                       se = c(a_se, b_se),
                       Player = c("Player A", "Player B"))
      subtitle <- paste("Player A:", a_ft, "Player B: ", b_ft, "SD A:", round(a_se,2) , "SD B:", round(b_se,2) )
      p <- ggplot(dat, aes(x=Player, y=Shooting_percentage)) + ###make y axis proportion between 0 and 1
        geom_point(size = 5)+
        geom_errorbar(aes(ymin=Shooting_percentage-se, ymax=Shooting_percentage+se), width=.1,
                      position=position_dodge(0.05)) +
        labs(title=('Player Free Throw Comparison'),
             subtitle = subtitle)
    } else{
      p <- place_holder()
    }
    return(p)
  })
  
  output$bar_plot <- renderPlot({
    # Create data for barplot
    data_plot <- data.frame(
      name=c("Player A","Player B"),  
      value=c(.3,.6)
    )
    
    # Barplot
    p_bar <- ggplot(data_plot, aes(x=name, y=value, fill=name)) + 
      geom_bar(stat = "identity")+
      scale_fill_brewer(palette = "Set1") +
      ylab("proportion of hits")+
      xlab("Player")+
      ggtitle("Free-Throws throughout the season")
    
    return(p_bar)
  })
 
  
  
  
  output$testResults <- DT::renderDataTable(
    expr = {
      validate(
        need(!is.na(store$simmed_player),
             message = "Select a player, then set paramters, and finally, press
             the Simulate button."
        )
      )
      pHat <- store$simmed_player / input$n_shots
      sePhat <- sqrt(pHat * (1 - pHat) / input$n_shots)
      z <- (pHat - input$nullValue) / (sePhat)
      temp3 <- binom.test(
        x = store$simmed_player,
        n = input$n_shots,
        p = input$nullValue,
        alternative = "two.sided",
        conf.level = 0.95
      )
      data.frame(
        row.names = c("Normal Approximation", "Exact Binominal"),
        Statistic = round(c(z, temp3$estimate), digits = 3),
        `p-value` = ifelse(
          c(pnorm(z, mean = 0, sd =1, lower.tail = FALSE), as.integer(temp3$p.value)) < 0.0001,
          "<0.0001",
          round(
            c(pnorm(z, mean = 0, sd =1, lower.tail = FALSE), as.integer(temp3$p.value)),
            digits = 4
          )
        )
      )
    },
    caption = "Null Hypothesis Test Results",
    style = "bootstrap4",
    rownames = TRUE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      ordering = FALSE,
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    )
  )
  
  output$ciPlot <- renderPlot({
    validate(
      need(!is.na(store$simmed_player),
           message = "Select a player, then set paramters, and finally, press
             the Simulate button."
      )
    )
    pHat <- store$simmed_player / input$n_shots
    sePhat <- sqrt(pHat * (1 - pHat) / input$n_shots)
    ggplot(
      data = data.frame(
        point = pHat,
        lower = max(pHat - 1.96 * sePhat, 0),
        upper = min(pHat + 1.96 * sePhat, 1)
      )
    ) +
      geom_pointrange(
        mapping = aes(
          y = 0,
          x = point,
          xmin = lower,
          xmax = upper,
          color = "estimate"
        ),
        size = 2
      ) +
      geom_point(
        mapping = aes(x = input$nullValue, y = 0, color = "null"),
        size = 12,
        shape = 18
      ) +
      scale_x_continuous(limits = c(0,1)) +
      scale_y_continuous(limits = c(-0.1, 0.1)) +
      scale_color_manual(
        values = c(
          "estimate" = "red",
          "null" = "blue"
        ),
        labels = c(
          "estimate" = parse(text = TeX("$\\widehat{p}$ and CI ")),
          "null" = parse(text = TeX("$p_0$"))
        )
      ) +
      labs(
        title = "Confidence Interval for Success Proportion",
        x = "Proportion of Successful Free Throws",
        y = NULL,
        color = NULL
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom"
      )
  })
}
