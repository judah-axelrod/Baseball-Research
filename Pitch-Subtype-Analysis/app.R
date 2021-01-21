library(shiny)
library(tidyverse)
library(purrr)

gumbo_df <- read_csv('~/Desktop/ST445/Project/2020finalproject-judah-axelrod/Pitch-Subtype-Analysis/RShiny_data.csv') %>%
subtypes <- sort(unique(gumbo_df$cluster))

url_chooser <- function(subtype, seed=NA){
  if (!is.na(seed)){
    set.seed(seed)
  }
  play_id <- gumbo_df %>%
    filter(cluster == subtype) %>%
    select(play_id) %>%
    sample_n(1) %>%
    pull()
  url <- paste0("https://baseballsavant.mlb.com/sporty-videos?playId=",play_id)
  url
}



#Create Data Frame
type <- subtypes
url <- map_chr(subtypes, url_chooser)

data <- data.frame(type, url)
 


options <- map(data$url, function(x) x)
options <- set_names(options, as.vector(data$type))
  

runApp(
  list(ui = fluidPage(
    selectInput('subtype', 'Choose a Pitch Subtype'
                , options
    )
    , htmlOutput("mySite")
  )
  ,server = function(input, output, session){
    output$mySite <- renderUI({
      tags$a(href = input$subtype, input$subtype)
    })
  })
)
