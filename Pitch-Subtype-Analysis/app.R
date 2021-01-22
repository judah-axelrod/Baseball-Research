library(shiny)
library(tidyverse)
library(purrr)

gumbo_df <- read_csv('data/RShiny_data.csv') 

gumbo_df <- gumbo_df %>%
  mutate(cluster = #Reformat cluster names
           str_replace_all(cluster,
               c('-' = ' - ',
                 'FF' = 'Four-Seam Fastball',
                 'SL' = 'Slider',
                 'CH' = 'Change-up',
                 'CU' = 'Curveball',
                 'SI' = 'Sinker',
                 'CU' = 'Cutter',
                 '1' = ' #1',
                 '2' = ' #2',
                 '3' = ' #3',
                 '4' = ' #4')))
gumbo_df <- gumbo_df[sample(nrow(gumbo_df)),] #Shuffle rows to facilitate randomization below

subtypes <- sort(unique(gumbo_df$cluster)) #List of subtypes

url_chooser <- function(subtype, seed){
  #This function takes in a random seed and a subtype and 
  #returns the URL corresponding to a random pitch of the given subtype
  gumbo_lim <- gumbo_df %>%
    filter(cluster == subtype)
  if (seed > nrow(gumbo_lim)){ #If seed too high, loop back around to beginning using modulo function
    seed = seed %% nrow(gumbo_lim)
  }
  play_id <- gumbo_lim %>%
    select(play_id) %>%
    slice(seed) %>%
    pull()
  url <- paste0("https://baseballsavant.mlb.com/sporty-videos?playId=",play_id)
  url
}

list_maker <- function(seed){
  #This function takes in a list of all the subtypes and calls the url_chooser function over all subtypes
  #The result is returned as a list to serve as the Select Inputs below.
  type <- subtypes
  url <- map_chr(subtypes, url_chooser, seed)
  
  data <- data.frame(type, url)
  options <- map(data$url, function(x) x)
  options <- set_names(options, as.vector(data$type))
  options
}



ui = fluidPage(
  numericInput('seed',HTML('Set Random Seed <br/> (Can be any Positive Whole Number)'),value=1,min=1), #Renders box to input random seed
  selectInput(input='subtype', HTML('Choose Pitch Subtype to See Example
                                    <br/> (Changing Seed Generates New Videos)'), #Renders drop-down menu to select pitch subtype
              choices="", selected=""),
  htmlOutput("mySite")
)


server <- function(input, output, session) {

    observeEvent(input$seed, {
      if(is.na(input$seed) | input$seed <=0 | !is.integer(input$seed)){
        updateSelectInput(session = session,
                          inputId = 'subtype',
                          choices ="")
      }
      else{
        updateSelectInput(session = session,
                        inputId = 'subtype',
                        choices = list_maker(input$seed)
        )
      }
    })
    output$mySite <- renderUI({
        tags$a(href = input$subtype, "View Pitch Here")
    })
  }



shinyApp(ui, server)











