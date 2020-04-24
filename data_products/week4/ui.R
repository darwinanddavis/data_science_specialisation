# pacman::p_load(dplyr,shiny,leaflet,leaflet.extras)
# runApp(here("data_products","week4"))

shinyUI(fluidPage( 
  titlePanel(h1("COVID19 global distribution from live webscraped")), # title
  fluidRow( # row 1 ---------------------------------------------
            column(# row 1, col 1 
              width = 9, # [1,12]
              selectizeInput(inputId = "Select country",
                             label = "Select countries to view",
                             choices = cv_countries %>% levels,
                             multiple = T,
                             options = list(maxItems = 10, placeholder = 'Enter country',
                                            onInitialize = I('function() { this.setValue(""); }'))
              )
            ),
            column(# row 1, col 2 
              width = 3,
              checkboxInput("Cases", # checkbox2
                            "Show/hide cases",
                            T),
              checkboxInput("Deaths",  # checkbox2 
                            "Show/hide deaths",
                            T),
              checkboxInput("Cases in last 15 days",  # checkbox3
                            "Show/hide recent cases",
                            T)
            )
  ),
  sidebarLayout( # sidebar ---------------------------------------------
                 sidebarPanel(
                   h3("Select inputs"),
                   width = 12, # width of sidebar frame
                   numericInput( # set rnorm 
                     "numeric1",
                     "Select random number",
                     value=1000,min=1,max=1000,step=1,
                     width = "15%"),
                 mainPanel( # main  ---------------------------------------------
                            h2("Plot random numbers"), # heading
                            width = "75%", # width of main frame
                            # textOutput("slidertext1"), # match X with server 'output$X'
                            code("Enter code here"),
                            plotlyOutput("plotly_1"), # match X with server output$X (plotly)
                            plotOutput("plot_1") # match X with server output$X (ggplot)
                 ) # end main 
  ) # end side panel 
)
) # end ui