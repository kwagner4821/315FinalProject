# ui.R

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
library(shiny)
library(shinydashboard)
library(ggthemes)

## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "A", icon = icon("home")),
    menuItem("Schrute Dataset", tabName = "B", icon = icon("th-large")),
    menuItem("World Map for Dialogue", tabName = "F", icon = icon("globe-americas")),
    menuItem("Collapsible Tree Ratings", tabName = "E", icon = icon("tree")),
    menuItem("Network Graph of Dialogue", tabName = "H", icon = icon("project-diagram")),
    menuItem("Episode Ratings", tabName = "D", icon = icon("tasks")),
    menuItem("Character Lines", tabName = "C", icon = icon("wrench")),
    menuItem("Ratings Over the Season", tabName = "G", icon = icon("cloud-sun-rain")),
    menuItem("Time Series of Episodes", tabName = "I", icon = icon("clock")),
    menuItem("Rating vs. Votes", tabName = "J", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "A",
            h2("Why We Chose to Look at the Office Dataset"),
            fluidRow(
              box(width=6,HTML("For our project we decided to take a look at the statistics
            of the television show The Office.<br> The Office is a TV mockumentary about a paper company Dunder Mifflin,
            and it chronicles<br>the lives of the workers and their day to day office life.
            We were particularly interested in investigating the statistics of this show because it was such an important
                             show for many of growing up and continues to become more relevant as we get older."))
            ),
            fluidRow(
              box(width=6,"We chose the dataset theoffice which can be loaded in the Schrute library in R. The dataset contains 
                  the quantitative variables season, episode, imdb_rating, total votes, and air-date. It contains the categorical
                  variables episode_name, director, writer, character, text, text with direction. Overall, it contains the imdb_ratings
                  for every season, and every line spoken in the show. In total the Office has 9 seasons and 201 episodes.")
            )
    ),
    tabItem(tabName = "B",
            fluidRow(DT::dataTableOutput('table',width=300))
    ),
    
    tabItem(tabName = "C",
            h2("How Many Lines Did Each Character Have?"),
            fluidRow(
              column(12,
                checkboxInput("Second Variable", label = "Fill By Season", value = TRUE),
                plotOutput(outputId = "plotshiny", height = 500, width = 800),align="center"
              )
            ),
            fluidRow(
                box(width=12,"Displayed above is a histogram of the line counts for each character.
                    Additionlly the interactive option to create a stacked histogram has been added. 
                    It is interesting to see how high a proportion of the total amount of lines
                    the most popular characters Michael, Dwight, Jim, Pam and Andy
                    take up compared to the rest of the characters.",align="center")
            )
    ),
    
    tabItem(tabName = "D",
            fluidRow(
              column(12,
                     checkboxInput("FBD", label = "Fill By Director", value = TRUE),
                     plotOutput(outputId = "plotshiny1",
                       height = 500, width = 800),
                       box(width=12,"Displayed above is a histogram of IMdb ratings
                       Additionlly the interactive option to create a stacked histogram filled by director has been added. 
                           It is interesting to see how certain directors
                           tended to have higher IMDb ratings while others had lower IMdb ratings."),align="center")
            )
    ),
    
    tabItem(tabName = "E",
            fluidRow(
              column(12,
                     collapsibleTreeOutput("sab1", height = "500px"),
                     box(width=12,"Displayed above is a interactive tree that allows one to visualize the 
                         number of episodes per season. Additionally the fill of the leaf nodes for the episodes
                         is the IMDb. It is interesting to see how certain seasons have significantly more episodes,
                         how some have higher ratings, and how the general trend of ratings occurs over
                         the course of the season. Red indicates a higher rating and yellow indicates a lower rating."),
                     align="center"))
    ),
    
    tabItem(tabName = "F",
            fluidRow(
              column(12,
                numericInput('size', 'Minimum Occurences Of Each Word', value = 1),
                wordcloud2Output('sab2'),
                box(width=12,"The word cloud above shows the most commonly used words in The Office.
                    Additionally, the interactive features allow you to set the limit
                    for minimum number of occurrences. When you hover over the word, it also
                    shows the counts. It is interesting to see how the names of the prominent characters
                    are used very often. "),align="center")
              )
    ),
    
    tabItem(tabName = "G",
            fluidRow(
              column(12,
              plotlyOutput(outputId = "plotly_plot_playable", 
                          height = 600,
                          width = 900),
              box(width=12,"The boxplot shows the distribution of the IMDb rating by episode number for all of the seasons.
                  It is interesting to see how certain numbered episodes tend to have higher ratings,
                  while other episodes tend to have lower ratings."),
              align="center"
              )
              )
    ),
    
    tabItem(tabName = "H",
            fluidRow(
              column(12,
                plotlyOutput("network_graph", height = 600,width=800),
                box(width=12,"The network graph shows the connection betweens the characters
                    in the office based on the names they mention in their lines. It is
                    interesting to see the grouping of characters and to see which character
                    would interact with each other more often versus less often."),
                align="center"
              )
            )
    ),
    
    tabItem(tabName = "I",
            fluidRow(
              column(12,
                plotlyOutput("time_series", height = 400,width=600),
                selectizeInput(
                  inputId = "season",
                  label = "Select Season",
                  choices = seq(1,9,by=1),
                  selected = 1
                ),
                box(width=12,"The Time Series plot shows the IMDb rating by season. It has an interactive feature that allows you
                    to select which season you would like to display.
                    It is interesting to see how the IDMb rating trends for each different season."),
                align="center"
              )
            )
    ),
    tabItem(tabName = "J",
            fluidRow(
              column(12,
              plotOutput("scatter",height=400,width=600),
              checkboxInput(inputId="lin-fit",label="Linear Fit"),
              checkboxInput(inputId="poly-ft",label="Polynomial Fit"),
              sliderInput(inputId="poly_degree",label="Polynomial Degree",min=2,max=10,step=1,value=2),
              checkboxInput(inputId="log_x",label="Scale X-axis as Log10"),
              align="center"
              )
            )
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Interactive Project"),
  sidebar,
  body,
  skin="green"
)