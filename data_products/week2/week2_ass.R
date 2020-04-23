pacman::p_load(here,leaflet,dplyr,readr,colorspace,purrr,htmltools,htmlwidgets,httr)
set_here("/Users/malishev/Documents/Coursera/data_science_specialisation/assignments")
wd <- here("data_products","week2")

# url <- "http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2020-04-16/data/listings.csv.gz"
airbnb <- url %>% 
  read_csv
lon <- airbnb$longitude %>% as.numeric()
lat <- airbnb$latitude %>% as.numeric()
rating <- airbnb$review_scores_rating %>% as.numeric()

colv <- colorRampPalette(diverge_hcl(rating %>% length, "Berlin") %>% sort)
colv_vec <- colv(length(rating))[as.numeric(cut(rating, breaks = length(rating)))]  # define breaks in col gradient
opac <- 0.7

# labels
ttl <- "Airbnb data for Amsterdam, Holland, since April 16, 2020"


# title
tag.map.title <- tags$style(
  HTML(".leaflet-control.map-title { 
       transform: translate(-50%,20%);
       position: fixed !important;
       left: 50%;
       text-align: center;
       padding-left: 10px; 
       padding-right: 10px; 
       background: white; opacity: 0.7;
       font-weight: bold;
       font-size: 20px;
       }"
       ))

title <- tags$div(
  tag.map.title, HTML(ttl)
)  

# heading options 
text_label_opt <- labelOptions(noHide = T, direction = "top", 
                               textOnly = T, offset = c(0,0),
                               style = style
)

href <- paste("Data scraped from <a href=http://insideairbnb.com/get-the-data.html> Inside Airbnb open data</a>", 
              "<br/> Date:", Sys.Date(),
              "<br/> Github: <a href=https://darwinanddavis.github.io/data_science_specialisation/data_products/week2/week2.html> @darwinanddavis </a>") 


cleanliness <- airbnb[,"review_scores_cleanliness"] %>% unique
cleanliness[is.na(cleanliness)] <- 1
beds <- airbnb[,"beds"] %>% unique
beds[is.na(beds)] <- 1
beds <- beds + 1
site_names <- paste("<strong> Ratings </strong>","<br/>",
                    "<strong> Check-in: </strong>", airbnb$review_scores_checkin,"<br/>", 
                    "<strong> Cleanliness: </strong>", airbnb$review_scores_cleanliness,"<br/>",
                    "<strong> Location: </strong>", airbnb$review_scores_location
) 
rating_overall <- paste("Overall rating: ",airbnb$review_scores_rating)

# text label options 
style <- list(
  "color" = "black",
  "font-weight" = "normal",
  "font-family" = "Optima",
  "padding" = "3px 3px"
)

text_label_opt <- labelOptions(noHide = F, direction = "top", textsize = "10px",
                               textOnly = F, opacity = 1, offset = c(0,0),
                               style = style, permanent = T
)

map <- leaflet() %>% 
  setView(lon[1],lat[1],zoom=12) %>% 
  addTiles() %>% 
  addCircles(lon,lat,
             radius = airbnb$review_scores_cleanliness/20,
             stroke = T,
             weight = 3, 
             opacity = 0.5,
             color = colv_vec,
             fillColor = colv_vec,
             label=rating_overall,
             popup=site_names,
             labelOptions = text_label_opt) %>%
  addProviderTiles("CartoDB.DarkMatter") %>% 
  addControl(title, position = "topleft", className="map-title") %>% 
  addControl(href,"topright")
map
