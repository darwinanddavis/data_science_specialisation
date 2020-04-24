pacman::p_load(here,leaflet,dplyr,readr,colorspace,purrr,htmltools,htmlwidgets,httr)
set_here("/Users/malishev/Documents/Coursera/data_science_specialisation/assignments")
wd <- here("data_products","week2")

url <- "http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2020-04-16/data/listings.csv.gz"
airbnb <- url %>% 
  read_csv
lon <- airbnb$longitude %>% as.numeric()
lat <- airbnb$latitude %>% as.numeric()
rating <- airbnb$review_scores_rating %>% as.numeric()

# col
colv <- colorRampPalette(diverge_hcl(rating %>% length, "Berlin") %>% sort)
colv_vec <- colv(length(rating))[as.numeric(cut(rating, breaks = length(rating)))]  # define breaks in col gradient
opac <- 0.7
font_size <- 40

# labels
style <- list(
  "color" = "black",
  "font-weight" = "normal",
  "font-family" = "Optima",
  "padding" = "3px 3px"
)

ttl <- paste0("Airbnb data for Amsterdam, The Netherlands, since April 16, 2020",
              "<br/>","Data scraped from <a href=http://insideairbnb.com/get-the-data.html> Inside Airbnb open data</a>", 
              "<br/> Date: ", Sys.Date(),"<br/>",
              "<br/> Matt Malishev",
              "<br/> Github: <a href=https://github.com/darwinanddavis> @darwinanddavis </a>"
) 


# title
tag.map.title <- tags$style(
  HTML(".leaflet-control.map-title { 
       transform: translate(-50%,20%);
       position: fixed !important;
       left: 40%;
       text-align: center;
       padding-left: 10px; 
       padding-right: 10px; 
       background: white; opacity: 0.7;
       font-weight: bold;
       font-size: 15px;
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

# popup data 
cleanliness <- airbnb[,"review_scores_cleanliness"] %>% unique
cleanliness[is.na(cleanliness)] <- 1
beds <- airbnb[,"beds"] %>% unique
beds[is.na(beds)] <- 1
beds <- beds + 1
weblink <- airbnb$host_url # weblink
webname <- airbnb$host_name
href <- paste0("<strong><a href=",weblink,">",webname,"</a></strong>") # host url
host_img <-  paste0("<img src=",airbnb$host_picture_url,"height=\"150px\" width=\"150px\">") # host img

site_names <- paste(sep="<br/>",
                    "<div style=\"font-size:20px;\">",href, "</div>",
                    host_img,
                    "Neighbourhood: ",airbnb$neighbourhood_cleansed,
                    "No. of reviews: ", airbnb$number_of_reviews,"<br/>",
                    "<strong> Ratings </strong>",
                    "Location: ", airbnb$review_scores_location,
                    "Check-in: ", airbnb$review_scores_checkin,
                    "Cleanliness: ", airbnb$review_scores_cleanliness
) 

rating_overall <- paste("Overall rating: ",airbnb$review_scores_rating)

text_label_opt <- labelOptions(noHide = F, direction = "top", textsize = "10px",
                               textOnly = F, opacity = 1, offset = c(0,0),
                               style = style, permanent = T
)

map <- leaflet(width = 1200, height = 900) %>% 
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
  addControl(title, "topleft", className="map-title")
map

save_html(map,paste0(wd,"/week2.html"),background = "#090909")
