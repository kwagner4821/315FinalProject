# server

library(tidyverse)
library(forcats)
library(reshape2)
library(gapminder)
library(shiny)
library(plotly)
library(leaflet)
library(dygraphs)
library(DT)
library(collapsibleTree)
library(scales)
library(wordcloud2)
library(wordcloud)
library(stopwords)
library(dplyr)
library(schrute)
library(crosstalk)
library(igraph)
library(ggnetwork)
library(sjmisc)
library(intergraph)
library(ggthemes)




function(input, output) {
  
  ##############################################################################
  #  Table output
  ##############################################################################
  
  output$table <- DT::renderDataTable(
    schrute::theoffice %>%
      select(-c("index"))
  )
  
  ##############################################################################
  # Andrew 1
  ##############################################################################
  
  output$plotshiny <- renderPlot({
    cbp <- c("#999999", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
             "#0072B2", "#D55E00", "#CC79A7")
    
    if (input$`Second Variable`){
      ggplot2::ggplot(andrew_schrute1, mapping = ggplot2::aes(x = reorder(character, character, function(x) -length(x)), 
                                                      y = season,
                                                      fill = season)) +
        ggplot2::geom_bar(position = "stack", stat = "identity") + 
        ggplot2::labs(title = "Histogram of Character by Line Count", 
                      x = "Character Lines By Season", y = "Count",fill="Season") + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                           hjust = 1, 
                                                           vjust = 1))
    } else {
      ggplot2::ggplot(andrew_schrute1, mapping = ggplot2::aes(x = reorder(character, character, function(x) -length(x)),
                                                      y = season)) +
        ggplot2::geom_bar(position = "stack", stat = "identity", 
                          fill = "lightblue") + 
        ggplot2::labs(title = "Histogram of Character by Line Count",
                      x = "Character Lines", y = "Count") + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                           hjust = 1, 
                                                           vjust = 1))
    }
    
  })
  
  ############################################################################
  # Andrew 2
  ############################################################################
  
  output$plotshiny1 <- renderPlot({
    andrew_schrute <- subset(schrute, !duplicated(subset(schrute, select=c(season, episode))))
    andrew_schrute <- andrew_schrute[unsplit(table(andrew_schrute$director), andrew_schrute$director) > 2, ]
    if (input$FBD){
      ggplot2::ggplot(andrew_schrute, mapping = ggplot2::aes(x = imdb_rating, fill = director)) +
        ggplot2::geom_histogram(binwidth = .3) + 
        ggplot2::labs(title = "Episode Ratings By Director", 
                      x = "IMDB Rating", y = "Count",fill="Director") + 
        theme_stata() + 
        theme(
          legend.position = "right"
        )
    } else {
      ggplot2::ggplot(andrew_schrute, mapping = ggplot2::aes(x = imdb_rating)) +
        ggplot2::geom_histogram(binwidth = .3, fill = "lightblue") + 
        ggplot2::labs(title = "Episode Ratings", 
                      x = "IMDB Rating", y = "Count") + 
        theme_stata() + 
        theme(
          legend.position = "right"
        )
    }
    
  })
    
  ############################################################################
  # Sabrina 1
  ############################################################################    
  
  output$sab1 <- renderCollapsibleTree({
    pal <- scales::gradient_n_pal(colours = c("yellow","orange","red"), values = c(min(office_ratings$imdb_rating),median(office_ratings$imdb_rating),max(office_ratings$imdb_rating)))
    collapsibleTree(
      office_ratings,
      hierarchy = c("season", "episode"),
      inputId="tree",
      root="Tree of Season/Episodes of The Office",
      fill=c("seashell",
             rep("maroon", length(unique(office_ratings$season))),
             pal(office_ratings$imdb_rating))
    )
    #output$nodeStr <- renderPrint(str(input$node))
  })
  


  
  ############################################################################
  # Sabrina 2
  ############################################################################
  output$sab2 <- renderWordcloud2({
    wordcloud2(schrute::theoffice %>%
                 tidytext::unnest_tokens(word, text) %>%
                 dplyr::anti_join(tidytext::stop_words, by = "word") %>%
                 dplyr::count(word, sort = TRUE),
               size=input$size)
  })
  
  ############################################################################
  # Viraj 1
  ############################################################################
  
  output$scatter <- renderPlot({
    
    kowagner_315_theme <- theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "grey"),
      text = element_text(family = "Times",size = 12,color = "black"),
      plot.background = element_rect(fill = "snow2")
    )
    
    p <- ggplot(office_ratings) + 
      geom_point(aes(y=imdb_rating,x=total_votes,color=as.factor(season))) + 
      labs(
        title = "Scatterplot of IMDb Rating Versus Total Votes",
        x = "Total Votes",
        y = "IMDb Rating",
        color = "Season"
      ) + 
      kowagner_315_theme
    
    if (input$`lin-fit`){
      p <- p + geom_smooth(aes(x=total_votes,y=imdb_rating),method="lm",se=F)
    }
    
    if (input$`poly-ft`){
      p <- p + geom_smooth(aes(x=total_votes,y=imdb_rating),
                           method="lm",formula = y ~ poly(x,input$poly_degree), se=F)
    }
    
    if (input$log_x){
      p <- p + scale_x_log10()
    }
    
    return(p)
  })
  
  ############################################################################
  # Viraj 2
  ############################################################################
  
  output$plotly_plot_playable <- renderPlotly({
    p <- ggplot(viraj_office_ratings,
                aes(x=reorder(as.factor(episode),
                              imdb_rating,na.rm = TRUE),imdb_rating)) + 
      geom_boxplot(aes(colour=as.factor(season)),fill="White", colour="blue") + 
      labs(title = "Imdb Rating of The Office as the Season Progresses",
           subtitle = "Ordered by Median Imdb Rating",
           x = "Episode Number", y = "Imdb Rating") + 
      theme_economist()
    p_plotly <- ggplotly(p) %>% layout(title = list(text = paste0("Imdb Rating of The Office as the Season Progresses",
                                                                  '<br>',
                                                                  '<sup>',
                                                                  "Ordered by Average Imdb Rating",
                                                                  "</sup>")))
    return(p_plotly)
  })
  
  ############################################################################
  # Kyle 1
  ############################################################################
  
  output$time_series <- renderPlotly({
    office_react <- reactive({
      office_ratings %>% dplyr::filter(season %in% input$season)
    })
    
    p <- ggplot(office_react(),aes(x=air_date,y=imdb_rating)) + 
      geom_line(color=cols[as.numeric(input$season)]) + 
      geom_point(aes(x=air_date,y=imdb_rating,type=as.factor(episode)),color=cols[as.numeric(input$season)]) + 
      labs(
        title = "Time Series Plot of IMDb Rating by Release Date",
        x = "Release date",
        y = "IMDb Rating") + 
      theme_hc() + 
      theme(
        legend.position='none'
      )
    
    p_plotly <- ggplotly(p,tooltip="text")
    
    return(p)
  })
  
  
  ############################################################################
  # Kyle 2
  ############################################################################
  
  output$network_graph <- renderPlotly({
    p <- ggplot(office_data, aes(x, y, xend = xend, yend = yend)) +
      geom_edges(arrow = arrow(length = unit(0.3, "lines")), 
                 color = "blue", alpha = 0.5) +
      geom_nodes(shape=21,size=10,aes(fill=character)) + 
      geom_nodetext(aes(label = character),size=2.5, 
                    color = "black", fontface = "bold") + 
      labs(
        x = "",
        y = "",
        title = "Network Graph of Top 30 Office Characters by Number of Lines",
        size = "Degree",
        fill = "Character"
      ) + 
      theme_blank()+
      theme(
        panel.background = element_rect(fill = "white"),
        text = element_text(family = "Times",size = 12,color = "black"),
        plot.background = element_rect(fill = "snow2")
      ) 
    p <- ggplotly(p)
    return(p)
  })
  
  
  
  

}
  