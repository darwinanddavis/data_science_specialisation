# airbnb shiny app week 4
# install.packages('rsconnect')
require(shiny)
require(shinythemes)
require(dplyr)
require(readr)
require(leaflet)
require(colorspace)
require(leaflet.extras)
require(reshape2)
require(stringr)
require(scales)
require(rsconnect)
require(htmltools)
require(here)
require(RColorBrewer)
require(tidyr)
require(purrr)

city_names <- list(
  "Amsterdam, the Netherlands",
  "Berlin, Germany",
  "Chicago, USA",
  "Edinburgh, Scotland",
  "Hong Kong, China",
  "Lisbon, Portugal",
  "London, UK",
  "Melbourne, Australia",
  "Oslo, Norway",
  "Paris, France",
  "Rio de Janeiro, Brazil",
  "Stockholm, Sweden",
  "Vancouver, Canada"
)
# criteria to subset from df 
criteria_candidates <- c(
  "Bed type",
  "Room type",
  "Property type",
  "Bathrooms",           
  "Cancellation policy",
  "Reviews per month",
  "Review scores rating",
  "Security deposit",    
  "Cleaning fee", 
  "Accommodates"
)
colvec <- c("Sunset-Dark","Inferno","Brwn-Yl","Burg","Teal")

shinyUI(fluidPage(
  theme = shinytheme(theme = "cyborg"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%;background-color:black;}"),
  textOutput("crit"),
  # tabPanel("tab1"),
  titlePanel(tags$h1(span("Analysing Airbnb listings from"),span(style="color:#FF385C;","Inside Airbnb"),span("open online data"),.noWS="outside")), # title
  fluidRow( # row 1 col 1---------------------------------------------
            column( # row 1, col 1
              width = 4,
              selectizeInput(inputId = "select_city",
                             label = "Select city",
                             choices = city_names,
                             multiple = F,
                             options = list(maxItems = 1, placeholder = 'Select city or start typing',
                                            onInitialize = I('function() { this.setValue(""); }')),
                             width = "100%"),
              sliderInput("price", 
                          "Choose price range",
                          value = c(50,200),
                          min = 0, max = 1000, step = 50,
                          width = "100%")
            ),
            column( # row 1, col 2 
              width = 4, # [1,12]
              selectizeInput(inputId = "select_criteria",
                             label = "Select criteria to plot",
                             choices = criteria_candidates, # airbnb %>% names, # %>% levels,
                             multiple = F,
                             options = list(maxItems = 1, placeholder = 'Select criteria or start typing',
                                            onInitialize = I('function() { this.setValue(""); }')),
                             width = "100%"),
              
              selectInput("colpal", 
                          "Choose colour palette",
                          choices = colvec,
                          selected = "Sunset",
                          width = "100%"),
              p()
            ),
            column( # row 1, col 3 
              width = 4, # [1,12]
              numericInput("bedrooms",
                           "Number of bedrooms",
                           value = 1,
                           min = 1, max = 5, step = 1,
                           width = "50%")
            )
  ),
  fluidRow( # row 2 col 2---------------------------------------------
            column(width=4), # empty col
            column(width = 4,
                   actionButton("make_map", 
                                "Map the data",
                                icon("map"),
                                width = "100%"),
                   p()
            ),
            column(width = 4) # empty col
  ),
  sidebarLayout( # sidebar ---------------------------------------------
                 sidebarPanel(
                   width = "100%", # width of main frame
                   h2("Airbnb data for ", textOutput("select_city",inline = T), textOutput("date", inline = T)), # heading
                   splitLayout(#cellWidths = rep("33%",3), # top header
                     tags$h4(span(style="color:#FF385C;","Price")),  # match X with server 'output$X'
                     tags$h4(span(style="color:#FF385C;","Criteria")),
                     tags$h4(span(style="color:#FF385C;","Bedrooms")),
                     tags$h4(span(style="color:#FF385C;","Listings"))
                   ),
                   splitLayout(
                     h4(textOutput("price",inline = T)), # match X with server 'output$X'
                     h4(textOutput("select_criteria", inline = T)),
                     h4(textOutput("bedrooms", inline = T)),
                     h4(textOutput("entries",inline = T))
                   )
                 ),  
                 # main  ---------------------------------------------
                 mainPanel(width = "100%",
                           h4(textOutput("user_message")),
                           leafletOutput("map")
                 )
  ), 
  mainPanel( # footer 
    tags$p(span(style="text-align:left;",
                strong("Author: "),"Matt Malishev","|",
                strong("Github: "),
                span(style="color:#FF385C;",a(style="color:#FF385C;","@darwinanddavis",href="https://github.com/darwinanddavis")),"|",
                strong("Data: "),
                span(style="color:#FF385C;",a(style="color:#FF385C;","Inside Airbnb",href="http://insideairbnb.com/get-the-data.html"))
    ))
  )
)) # end ui
