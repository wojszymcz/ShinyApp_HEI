#setwd("/Users/zuzanna/Desktop/Studia/AdvancedR")
setwd("C:/Users/ASUS/Dropbox/WSZ/AdvancedProgrammingInR")
rm(list = ls())
###############################################################################
#This is a framework for Shiny WebApp. 
###############################################################################
requiredPackages = c( "shiny",
                      "shiny.fluent",
                      "imola",
                      "readr", 
                      "tidyverse",
                      "stringr",
                      "colourpicker") # list of required packages
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) } 

# Read the data
uni <- read.csv("DataUniversities.csv")
glimpse(uni)

# University Adress
#  Some data missing concerning adress
j <- 0

for (i in 1: length(uni$Postcode)){
  if (uni[i, 4] == ''){
    print(i)
    print("No postcode definied")
    j = j + 1
  }
}
# 43 missings in data 
print(j)

# let's see how much data on lang-lat we can derive based on exsisting data
uni$adress <- paste(uni$Postcode, uni$City, ",Poland", sep = " ")
uni <- uni %>%
  geocode(adress, method = 'osm', lat = latitude , long = longitude)

uni$longitude[uni$Postcode == ''] <- ''
uni$latitude[uni$Postcode == ''] <- '0'


# Solution: Webscrapping from wikpedia
# In the end: we will need the lattitude and longitutde -> we will retreive these data
uni$Name2 <- gsub(" ", "_", uni$Name) # needed for wikipedia url 

for (i in 1:length(uni$Postcode)){
  if (uni[i, 4] == ''){
    url <- paste("https://pl.wikipedia.org/wiki/", uni[i,9], sep = "", collapse = NULL)
    x <- tryCatch({
      url %>% 
        read_html() %>% 
        html_node(xpath = '//*[@id="coordinates"]') %>% 
        html_text() %>% 
        as.character() %>% 
        str_split(pattern = "/")
    }, error = function(e) {
      # handle HTTP error 404
      if (grepl("HTTP error 404", e$message)) {
        message(paste("Error: ", e$message, " - Skipping URL ", url))
        return(NULL)
      }
      else {
        # re-throw any other errors
        stop(e)
      }
    })
    if (!is.null(x)) {
      uni[i, 7] <- as.character(str_extract_all(gsub(",", ".", x[[1]][2]), "[+-]?([0-9]*[.])?[0-9]+")[[1]])[1]
      uni[i, 8] <- as.character(str_extract_all(gsub(",", ".", x[[1]][2]), "[+-]?([0-9]*[.])?[0-9]+")[[1]])[2]
    }
  }
}

#prepared data for SHINY
data <- uni %>% 
  filter(longitude != "") %>% 
  select(Name, longitude, latitude)

# create map
subscr <- data.frame(lat=c(as.numeric(data$latitude)),
                     lon=c(as.numeric(data$longitude)))

leaflet() %>% addTiles() %>% 
  addCircleMarkers(data = subscr,
                   lat = ~lat, lng = ~lon,
                   color = "blue")

# generate data for universities 
for (i in 1: length(data$Name)){
  loc <- c(as.numeric(data[which(data$Name == as.character(data[i,1])), 2]), as.numeric(data[which(data$Name == as.character(data[i,1])), 3]))
  iso <- osrmIsochrone(loc, breaks = c(0, 30, 60, 90, 120), res = 50)
  iso$run_times <- factor(paste(iso$isomin, "to", iso$isomax, "min"),
                          levels = c("0 to 30 min", "30 to 60 min", "60 to 90 min", 
                                     "90 to 120 min"))
  data$iso[i] <- list(iso)
}
data <- data_chosenarea
save(data, file = "universities.RData")

