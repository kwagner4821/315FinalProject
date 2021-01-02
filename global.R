# global.R

# Notes:
# ------
# Where you can prepare all data that isn't reactive, clean up things and more.
# 
# Note: you could also do this preparation elsewhere, especially if it takes 
# some time to run and just save it as a .Rdata file and load it in this file


# Libraries

library(tidyverse)

### colors and theme

schrute <- schrute::theoffice

charlines <- table(schrute$character)
charlines <- charlines[charlines > 40]
charnames <- names(charlines)
andrew_schrute1 <- schrute[which(schrute$character %in% charnames),]
andrew_schrute1$season <- as.factor(andrew_schrute1$season)
andrew_schrute1$character <- as.factor(andrew_schrute1$character)

cb_pal = c("#000000", "#E69F00", 
           "#56B4E9", "#009E73",
           "#F0E442", "#0072B2",
           "#D55E00", "#CC79A7")
cb_pal_cont = c("Africa" = "#000000",
                "Americas" = "#E69F00",
                "Asia" = "#56B4E9", 
                "Europe" = "#009E73",
                "Oceania" = "#F0E442")

my_theme = theme_minimal() +
  theme(axis.title  = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16), 
        plot.title = element_text(size = 20)
        )

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
viraj_office_ratings <- subset(office_ratings,select = -c(air_date))
viraj_office_ratings <- viraj_office_ratings[,c(3,1,2,4,5)]

cols = c("red","blue","yellow","orange","green","brown","black","tan","grey")
office_data <- readr::read_csv("https://raw.githubusercontent.com/kwagner4821/315Final/main/office.csv")
