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

server_dims <- "auto" #1000 
colvec <- c("Sunset-Dark","Inferno","Brwn-Yl","Burg","Teal")
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
city_urls <- list(
  "http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2020-05-08/data/listings.csv.gz",
  "http://data.insideairbnb.com/germany/be/berlin/2020-05-14/data/listings.csv.gz",
  "http://data.insideairbnb.com/united-states/il/chicago/2020-04-23/data/listings.csv.gz",
  "http://data.insideairbnb.com/united-kingdom/scotland/edinburgh/2020-04-27/data/listings.csv.gz",
  "http://data.insideairbnb.com/china/hk/hong-kong/2020-04-29/data/listings.csv.gz",
  "http://data.insideairbnb.com/portugal/lisbon/lisbon/2020-04-29/data/listings.csv.gz",
  "http://data.insideairbnb.com/united-kingdom/england/london/2020-04-14/data/listings.csv.gz",
  "http://data.insideairbnb.com/australia/vic/melbourne/2020-05-13/data/listings.csv.gz",
  "http://data.insideairbnb.com/norway/oslo/oslo/2020-04-30/data/listings.csv.gz",
  "http://data.insideairbnb.com/france/ile-de-france/paris/2020-04-15/data/listings.csv.gz",
  "http://data.insideairbnb.com/brazil/rj/rio-de-janeiro/2020-04-20/data/listings.csv.gz",
  "http://data.insideairbnb.com/sweden/stockholms-l%C3%A4n/stockholm/2020-04-28/data/listings.csv.gz",
  "http://data.insideairbnb.com/canada/bc/vancouver/2020-05-11/data/listings.csv.gz"
  # 
  # "http://data.insideairbnb.com/australia/vic/melbourne/2020-03-16/data/listings.csv.gz",
  # "http://data.insideairbnb.com/united-kingdom/england/london/2020-04-14/data/listings.csv.gz",
  # "http://data.insideairbnb.com/united-states/il/chicago/2020-03-18/data/listings.csv.gz",
  # "http://data.insideairbnb.com/china/hk/hong-kong/2020-03-17/data/listings.csv.gz",
  # "http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2020-04-16/data/listings.csv.gz",
  # "http://data.insideairbnb.com/germany/be/berlin/2020-03-17/data/listings.csv.gz",
  # "http://data.insideairbnb.com/brazil/rj/rio-de-janeiro/2020-03-18/data/listings.csv.gz",
  # "http://data.insideairbnb.com/france/ile-de-france/paris/2020-03-15/data/listings.csv.gz",
  # "http://data.insideairbnb.com/canada/bc/vancouver/2020-04-17/data/listings.csv.gz",
  # "http://data.insideairbnb.com/united-kingdom/scotland/edinburgh/2020-03-21/data/listings.csv.gz",
  # "http://data.insideairbnb.com/norway/oslo/oslo/2020-03-22/data/listings.csv.gz",
  # "http://data.insideairbnb.com/portugal/lisbon/lisbon/2020-03-21/data/listings.csv.gz",
  # "http://data.insideairbnb.com/sweden/stockholms-l%C3%A4n/stockholm/2020-03-22/data/listings.csv.gz"
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

shinyServer(function(input, output){
  # get user city string
  url_input <- reactive({ # get city url 
    city_input <- input$select_city
    city_input %>% str_split_fixed(", ",10) %>% str_to_lower() %>% str_replace_all(fixed(" "),"-") # split string into city and country and replace spaces with "-"
  })
  # get url for user city 
  url_read <- reactive({
    city_urls[str_detect(city_urls,url_input()[1])] %>% unlist # return url containing city string
  })
  
  # get base data --------------------------------------------------------------
  get_airbnb <- reactive({#eventReactive(input$make_map,{
    airbnb <- url_read() %>% read_csv
    
    # clean data 
    cols_clean <- airbnb %>% names %>% str_to_title() %>% str_replace_all("_", " ") # make cols title case 
    colnames(airbnb) <- cols_clean 
    # change char to num
    airbnb <- airbnb %>% mutate(`Price` = `Price` %>% str_sub(2) %>% str_trim("both") %>% as.numeric,
                                `Cleaning fee` = `Cleaning fee` %>% str_sub(2) %>% str_trim("both") %>% as.numeric,
                                `Security deposit` = `Security deposit` %>% str_sub(2) %>% str_trim("both") %>% as.numeric,
                                `Cancellation policy` = `Cancellation policy` %>% str_to_title() %>% str_replace_all("_", " ")
    ) %>% 
      select(`Host name`, # subset data 
             `Host url`,
             Longitude, Latitude,
             Price, Bedrooms, Neighbourhood, 
             `Review scores rating`,
             `Review scores location`,
             `Review scores cleanliness`,
             `Review scores communication`,
             `Review scores accuracy`,
             criteria_candidates) # add criteria candidates 
    airbnb <- airbnb %>% mutate_at("Neighbourhood",replace_na,"NA") # replace NAs in neighbourhood
    airbnb <- airbnb %>% # round off data 
      mutate_at("Security deposit",funs(plyr::round_any(.,100))) %>% 
      mutate_at("Cleaning fee",funs(plyr::round_any(.,100))) %>% 
      mutate_at("Accommodates",funs(plyr::round_any(.,1))) %>% 
      mutate_at("Reviews per month",funs(plyr::round_any(.,5))) %>% 
      mutate_at("Review scores rating",funs(plyr::round_any(.,10))) %>% 
      mutate_at("Bathrooms",funs(plyr::round_any(.,1))) 
    airbnb <- airbnb %>% mutate_all(replace_na, 0) # replace NAs
    airbnb
  })
  
  # map tile
  custom_tile <- "http://b.sm.mapstack.stamen.com/((mapbox-water,$f2f7ff[hsl-color]),(positron,$f2f7ff[hsl-color]),(buildings,$f2f7ff[hsl-color]),(parks,$2c403b[hsl-color]))/{z}/{x}/{y}.png"
  
  # set all user input changes to action button in ui
  # subset data based on user input 
  map_event <- eventReactive(input$make_map,{
    input_price <- input$price
    input_bedroom <- input$bedrooms
    # filter by criteria data
    get_airbnb() %>% filter(Price >= input$price[1] & Price <= input$price[2],
                            Bedrooms == input_bedroom)
  },ignoreNULL = T)
  
  # map latlon
  map_points <- eventReactive(input$make_map,{
    map_event() %>% # use filtered data
      select(Longitude, Latitude) # to get latlon
  },ignoreNULL = T)
  
  # user criteria 
  criteria_output <- eventReactive(input$make_map,{
    map_event() %>%
      pull(input$select_criteria)
  },ignoreNULL = T)

  # no of entries 
  entries <- eventReactive(input$make_map, {
    criteria_output() %>% length %>% format(big.mark=",",scientific = F,trim = T)
  })
  
  # # col pal reactive 
  col_pal <- eventReactive(input$make_map,{
    input_col <- criteria_output() %>% # pull user criteria 
      as.factor %>% as.integer() %>% # turn into num for col pal
      na.omit() # rm nas
    colv <- colorRampPalette(sequential_hcl(input_col %>% length, input$colpal))
    colv_vec <- colv(input_col %>% length)[as.numeric(cut(input_col, breaks = length(input_col)))]  # define breaks in col gradient
    colv_vec[is.na(colv_vec)] <- "black" # change NA to black
    colv_vec 
    # colorNumeric(colv, input_col) # user for leaflet proxy to convert to leaflet color func
  },ignoreNULL = T)
  
  # for col pal as layers
  col_pal_2 <- eventReactive(input$make_map,{
    input_col <- criteria_output() %>% # pull user criteria 
      as.factor %>% as.integer() %>% # turn into num for col pal
      na.omit() # rm nas
    colv <- colorRampPalette(sequential_hcl(input_col %>% length, "Viridis"))
    colv
  },ignoreNULL = T)
  
  # popup event. needs to be vector, use 'pull()'
  popup_event <- eventReactive(input$make_map,{
    host_name <- map_event() %>% 
      pull(`Host name`) # host name 
    host_url <- map_event() %>% 
      pull(`Host url`) # url
    neighbourhood <- map_event() %>% 
      pull(Neighbourhood) # hood
    rating_overall <- map_event() %>% 
      pull(`Review scores rating`) # overall rating
    rating_location<- map_event() %>% 
      pull(`Review scores location`) # location
    rating_cleanliness<- map_event() %>%
      pull(`Review scores cleanliness`) # cleanliness
    rating_communication <- map_event() %>%
      pull(`Review scores communication`) # comm
    rating_accuracy <- map_event() %>%
      pull(`Review scores accuracy`) # accuracy 
    href <- paste0("<strong><a href=",host_url,">",host_name,"</a></strong>") # web link
    site_names <- paste(
      "<div style=\"font-size:20px;\"> ","<br/>",href,"</div>","<br/>",
      "<strong> Neighbourhood </strong>","<br/>",neighbourhood,"<br/>",
      "<br/>", 
      "<strong> Ratings </strong>","<br/>",
      "Overall: ",rating_overall,"<br/>",
      "Location: ",rating_location,"<br/>",
      "Cleanliness: ",rating_cleanliness,"<br/>",
      "Communication: ",rating_communication,"<br/>",
      "Accuracy: ",rating_accuracy,"<br/>",
      "<br/>", 
      "<strong>",input$select_criteria,"</strong>", "<br/>", 
      criteria_output() # print user criteria 
    )
  },ignoreNULL = T)
  
  # price label
  price_label <- eventReactive(input$make_map,{
    map_event() %>%
      pull(Price) # get price from filtered data 
  })
  
  # criteria label
  criteria_label <- eventReactive(input$make_map,{
    label <- criteria_output() # use filtered user criteria data 
    # paste0(input$select_criteria,": ",label)
    paste0(
      "<div style=\"font-size:15px;\">", 
      "<strong>",input$select_criteria,"</strong>",": ",label,
      "<br/>",
      "<strong>Price</strong>",": $",price_label(), # use filtered price data 
      "<br/>",
      "</div>"
    ) %>% map(htmltools::HTML) # turn into html for styling 
  },ignoreNULL = T)
  
  # legend title 
  legend_title <- eventReactive(input$make_map,{
    map_event() %>%  
      select(input$select_criteria) %>% names
  },ignoreNULL = T)
  
  # style -------------------------------------------------------------------
  
  # current colpal
  opac <- 0.7
  font_size <- 40
  
  # style
  style <- list(
    "color" = "black",
    "font-size" = "10px",
    "font-weight" = "normal",
    # "font-family" = "Optima",
    "padding" = "5px 5px"
  ) 
  # label options
  text_label_opt <- labelOptions(noHide = F, direction = "top",
                                 textOnly = F, opacity = 1, offset = c(0,0),
                                 style = style, permanent = T
  )
  
  # easy buttons
  locate_me <- easyButton( # locate user
    icon="fa-crosshairs", title="Zoom to my position",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }"));
  
  reset_zoom <- easyButton( # reset zoom
    icon="fa-home", title="Reset zoom",
    onClick=JS("function(btn, map){ map.setZoom(10);}"));
  
  # pulse icons
  marker_pulse <- makePulseIcon(
    color = "orange",
    iconSize = 5,
    animate = T,
    heartbeat = 0.05
  )
  pulse_options <- markerOptions(opacity=0.5, riseOnHover = T, interactive = T)
  
  # layer options
  layer_options <- layersControlOptions(collapsed = F)
  min_zoom = 3
  max_zoom = 16

  # map ---------------------------------------------------------------------
  
  observeEvent(input$make_map, { # watch action button
    if (is.null(input$select_criteria)){ # if criteria empty
      output$user_message <- renderText({"Select criteria to map"})
    } else {
      output$user_message <- renderText({""})
      output$map <- renderLeaflet({
        leaflet(options = leafletOptions(worldCopyJump = T)) %>%
          # setView(zoom=12) %>%
          addTiles(#custom_tile,
            options = providerTileOptions(minZoom=min_zoom, maxZoom=max_zoom)) %>%
          addProviderTiles("CartoDB",
                           options = providerTileOptions(minZoom=min_zoom, maxZoom=max_zoom)) %>%
          addCircles(data = map_points(),
                     radius = 5,
                     color = col_pal(),
                     fill = col_pal(),
                     fillColor = col_pal(),
                     weight = 1,
                     opacity = opac,
                     fillOpacity = opac,
                     label = criteria_label(),
                     popup = popup_event(),
                     labelOptions = text_label_opt) %>%
          addEasyButton(reset_zoom) %>%
          addEasyButton(locate_me) %>%
          addLegend("bottomright",title = legend_title(),
                    colors = col_pal() %>% unique %>% sort,
                    labels = criteria_output() %>% unique %>% sort)
      })
    } # end else
  }) # end observe action button
  
  # print outputs 
  output$select_city <- renderText({paste0(input$select_city,",")})
  output$date <- renderText({"May 2020"})
  output$bedrooms <- renderText({input$bedrooms})
  output$price <- renderText({paste0("$",input$price[1],"—$",input$price[2])})
  output$select_criteria <- renderText({input$select_criteria})
  output$entries <- renderText({entries()})
  
}) # end server 

