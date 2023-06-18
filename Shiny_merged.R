## Authors: Szymczak Wojciech, Kostecka Zuzanna
###############################################################################
#This is a framework for Shiny WebApp. 
###############################################################################
requiredPackages = c( "shiny",
                      "shiny.fluent",
                      "imola",
                      "readr", 
                      "tidyverse",
                      "stringr",
                      "colourpicker",
                      "foreign",
                      "DT") # list of required packages
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) } 

###############################################################################
#Spatial analysis packages
###############################################################################
requiredPackages = c( "spdep",
                      "Rcpp",
                      "terra",
                      "maptools", 
                      "sp",
                      "RColorBrewer",
                      "classInt",
                      "gtools",
                      "e1071",
                      "spatstat",
                      "rgdal", 
                      "rvest",
                      "tidygeocoder",
                      "leaflet",
                      "osrm", 
                      "secr",
                      "readr") # list of required packages
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) } 

###############################################################################
# Data and working directories   
###############################################################################
# setwd("/Users/zuzanna/Desktop/Studia/AdvancedR")
setwd("C:/Users/ASUS/Dropbox/WSZ/AdvancedProgrammingInR")
load("Projekt/ShinyApp_HEI/universities2.RData")
data_chosenarea <- data
load("Projekt/ShinyApp_HEI/universities.RData")
load("Projekt/ShinyApp_HEI/ela_earnings.Rdata")

#postcodes for defensive programming part
postcodes <- read_delim("Projekt/ShinyApp_HEI/kody.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)
postcodes_uniq <- unique(postcodes$`KOD POCZTOWY`)

###############################################################################
# Functions used in the project 
###############################################################################
postcode_defense <- function(postcode) {
  if (!(postcode %in% postcodes_uniq)) {
    return("Invalid zipcode format. Please enter a xx-xxx format")
  }
  return(NULL)  # Return NULL if the validation passes
}

# Define a function to sanitize the zipcode input
postcode_sanitize <- function(postcode) {
  postcode <- gsub("[^0-9][^0-9]-[^0-9][^0-9][^0-9]", "", postcode)
  return(postcode)
}
###############################################################################
# Sidebar and user's parameters
###############################################################################

#settings
display_uni <- function(id) {
  container_style <- "
    display: flex;
    padding: 4px;
    align-items: center;
    gap: 10px;
    margin-bottom: 0px;
    border-bottom: solid gray 1px;
  "
  h3_style <- "
    margin: 0;
    font-size: 13px;
    font-weight: 700;
  "
  p_style <- "
    margin: 0;
    font-size: 12px;
    font-weight: 300;
  "
  meta_style <- "
    display: flex;
    wrap: nowrap;
    gap: 5px;
  "
  
  div(
    style = "display: flex;",
    class = "uni-container",
    id = id,
    onclick = "sendUniId(this.id)",
    h3(mag, style = h3_style),
    div(
      h3(str_to_title(place), style = h3_style),
      div(
        class = "quake-metadata",
        p(time, style = p_style), 
        p(paste(mag, "km"),p = p_style)
      )
    ),
    style = container_style
  )
}

###############################################################################
# Header zoom and download
###############################################################################

# header_commandbar_list <- list(
#   list(
#     key = 'zoom_out', 
#     text = "Zoom out", 
#     iconProps = list(iconName = "FullScreen")
#   ),
#   list(
#     key = 'download', 
#     text = "Download data", 
#     iconProps = list(iconName = "Download"),
#     onClick = JS("function() { Shiny.setInputValue('downloadButton', true); }")
#   )
# )

###############################################################################
# Header
###############################################################################

app_header <- flexPanel(
  id = "header",
  align_items = "center",
  flex = c(0, 1, 0),
  img(src = "logo1.png", style = "width: 150px"),
  div(
    Text(variant = "xLarge", "| Final project for Advanced Programming in R", 
         style="color: gray"), 
    style = "margin-bottom: 10px;"),
  #CommandBar(items = header_commandbar_list),
  style = "box-shadow: 0 0 15px #000;  padding: 12px;"
)

###############################################################################
# Footer
###############################################################################

app_footer <- flexPanel(
  id = "footer",
  justify_content = 'space-between',
  gap = "80px",
  style = "margin-left: 0px; background-color: #f9f9f9; padding: 20px; color: #777; font-size: 14px;",
  Text(variant = "medium", "Built by Kostecka, Z. & Szymczak, W.", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "Source: HEI, ELA GOV and POLON register data"),
  Text(variant = "medium", nowrap = FALSE, "WNE UW 2023")
)
###############################################################################
#UI
###############################################################################

ui <- shinyUI(
  tabsetPanel(
    tabPanel("Draw Isochrones",
             fluidPage(
               tags$head(
                 tags$link(rel = "stylesheet", href = "uni_styles.css")
               ),
               app_header,
               titlePanel("Filter tool"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("HEI", "University:", choices = c(data$Name)),
                   sliderInput("Dist", "Selected Driving Time:",
                               min = 0, max = 120, value = c(0, 120),
                               step = 30),
                   colourInput("col1", "Select color of the first isochrone", "#F1BB7B", allowTransparent = TRUE),
                   colourInput("col2", "Select color of the second isochrone", "#FD6467", allowTransparent = TRUE),
                   colourInput("col3", "Select color of the third isochrone", "#5B1A18", allowTransparent = TRUE),
                   colourInput("col4", "Select color of the fourth isochrone", "#D67236", allowTransparent = TRUE)
                 ), 
                 mainPanel(
                   leafletOutput("leafletMap")
                 )
               ),
               div(id = "footer", app_footer)
             )
    ),
    tabPanel("Best paid majors in my surrounding",
             fluidPage(
               tags$head(
                 tags$link(rel = "stylesheet", href = "uni_styles.css")
               ),
               app_header,
               titlePanel("Table with universities"),
               sidebarLayout(
                 sidebarPanel(
                   textInput("zipp", "Enter Your Post-code"),
                   verbatimTextOutput("postcode_defense"),
                   sliderInput("dist_2", "Selected Driving Time:",
                               min = 0, max = 120, value = 30,
                               step = 30),
                   selectInput("field", "Field of Study:", choices = c(ela_earnings$p_dziedzina)),
                   selectInput("part_time", "Do you want to study part-time?", choices = c("Yes", "No")),
                   actionButton("acc", "Print Table")
                 ),
                 mainPanel(
                   htmlOutput('text1'),
                   DTOutput('table_earnings')
                 )
               ),
               div(id = "footer", app_footer)
             )
    )
  )
)

###############################################################################
#Server
###############################################################################
server <- function(input, output) {
  
  #input
  filteredData <- reactive({
    uni_name <- input$HEI
    if (!is.null(uni_name) && uni_name %in% data$Name) {
      loc <- c(
        as.numeric(data[data$Name == uni_name, 2]),
        as.numeric(data[data$Name == uni_name, 3])
      )
    }
    return(loc)
  })
  
  filteredISO <- reactive({
    uni_name <- input$HEI
    dist_min <- input$Dist[1]
    dist_max <- input$Dist[2]
    
    if (!is.null(uni_name) && uni_name %in% data$Name){
      iso <- st_as_sf(data[data$Name == uni_name,][[4]][[1]])
    }
    
    if (dist_min == 0 & dist_max == 90) {
      iso <- iso %>% 
        filter(id <= 3)
      return(iso)
    }
    
    if (dist_min == 0 & dist_max == 60) {
      iso <- iso %>% 
        filter(id <= 2)
      return(iso)
    }
    
    if (dist_min == 0 & dist_max == 30) {
      iso <- iso %>% 
        filter(id <= 1)
      return(iso)
    }
    
    if (dist_min == 30 & dist_max == 120) {
      iso <- iso %>% 
        filter(id > 1 & id < 5 )
      return(iso)
    }
    
    if (dist_min == 30 & dist_max == 90) {
      iso <- iso %>% 
        filter(id > 1 & id < 4 )
      return(iso)
    }
    
    if (dist_min == 30 & dist_max == 60) {
      iso <- iso %>% 
        filter(id > 1 & id < 3 )
      return(iso)
    }
    
    if (dist_min == 60 & dist_max == 120) {
      iso <- iso %>% 
        filter(id > 2  )
      return(iso)
    }
    
    if (dist_min == 60 & dist_max == 90) {
      iso <- iso %>% 
        filter(id > 2 & id < 4 )
      return(iso)
    }
    
    if (dist_min == 90 & dist_max == 120) {
      iso <- iso %>% 
        filter(id > 3  )
      return(iso)
    }
    
    if (dist_min == 0 & dist_max == 120) {
      return(iso)  
    }
    
  })
  
  palette <- reactive({
    pal <- c(input$col1, input$col2, input$col3, input$col4)
    return(pal)
  }
  )
  #output
  
  output$leafletMap <- renderLeaflet({
    
    req(input$HEI)
    loc <- filteredData()
    iso <- filteredISO()
    pal <- palette()
    
    iso$run_times <- factor(
      paste(iso$isomin, "to", iso$isomax, "min"),
      levels = c("0 to 30 min", "30 to 60 min", "60 to 90 min", "90 to 120 min")
    )
    
    
    factpal <- colorFactor(pal, iso$run_times)
    
    universities <- leaflet(data = iso) %>% 
      setView(lng = loc[1], lat = loc[2], zoom = 7) %>%
      addTiles() %>% 
      addMarkers(lng = loc[1], lat = loc[2], popup = "University") %>%
      addPolygons(
        fill = TRUE, stroke = TRUE, color = "black",
        fillColor = ~factpal(iso$run_times),
        weight = 0.5, fillOpacity = 0.3,
        data = iso, popup = iso$run_times,
        group = "Driving Time"
      ) %>% 
      addLegend(
        "bottomright", pal = factpal, 
        values = iso$run_times,
        title = "Driving Time"
      )
    
    return(universities)
  })
  
  
plot_zipp <- reactive({

    input$acc
    x <-isolate(input$zipp)
    y <- isolate(input$field)
    z <- isolate(input$part_time)
    to_return <- list(x,y,z)
    return(to_return)
  })



postcode <- reactive({
    # req(input$acc)
    # list_zipp <- plot_zipp()
    # zip_code <- unlist(list_zipp)[1]
    # dziedzina <- unlist(list_zipp)[2]
    # part_time <- unlist(list_zipp)[3]
    # 
    # x <- data.frame(Country = "Poland", postalcode = zip_code)
    # y <- x %>%
    #   geocode(country = Country, postalcode = postalcode)
    # area <- input$dist_2
    # route <- list()
    # routes <- list()
    # uni_names <- c()
    # j <- 1
    # for (i in 1:nrow(data)) {
    #   route[[i]] <- osrmRoute(src = c(as.numeric(data$longitude[i]), as.numeric(data$latitude[i])),
    #                           dst = c(y$long, y$lat),
    #                           overview = FALSE)
    #   route[[i]][3] <- data$longitude[i]
    #   route[[i]][4] <- data$latitude[i]
    #   route[[i]][5] <- data$Name[i]
    #   
    #   if(as.numeric(route[[i]][1]) < area){
    #     routes[[j]] <- route[[i]]
    #     uni_names[[j]] <- route[[i]][5]
    #     j = j+1 
    #   }
    #   
    # }
  req(input$acc)
  list_zipp <- plot_zipp()
  zip_code <- unlist(list_zipp)[1]
  dziedzina <- unlist(list_zipp)[2]
  part_time <- unlist(list_zipp)[3]
  
  x <- data.frame(Country = "Poland", postalcode = zip_code)
  y <- x %>%
    geocode(country = Country, postalcode = postalcode)
  area <- input$dist_2
  
  data$distance <- apply(data, 1, function(row) {
    osrmRoute(
      src = c(as.numeric(row$longitude), as.numeric(row$latitude)),
      dst = c(y$long, y$lat),
      overview = FALSE
    )[1]
  })
  
  routes <- data[data$distance < area, ]
  uni_names <- routes$Name
    unlisted_uni_names <- unlist(uni_names)
  
    zipcode <- postcode_sanitize(input$zipp)
    validationError <- postcode_defense(zipcode)
    if (!is.null(validationError)) {
      showNotification(validationError, type = "warning")
      return()
    }
    else{
      ela_earnings_choice <- ela_earnings %>% 
        filter(p_nazwa_uczelni %in% unlisted_uni_names) %>% 
        mutate(University = p_nazwa_uczelni,
               Major = p_kierunek_nazwa,
               Field = p_dziedzina,
               Part_Time = ifelse(p_forma == "S", "No", "Yes"),
               Level = ifelse(p_poziom == "1", "First-cycle, 6 semesters","First-cycle, 10 semesters" ),
               AverageEarnings = round(p_e_zar,2),
               RelativeEarningsInPoviat = round(p_wwz, 2)) %>% 
        select(University,
               Major, 
               Field,
               Part_Time, 
               Level,
               AverageEarnings,
               AverageEarnings) %>%
        distinct() %>% 
        arrange(desc(AverageEarnings))
      
      if(!is.null(dziedzina) & dziedzina != ""){
        ela_earnings_choice <- ela_earnings_choice %>%
          filter(Field == dziedzina)
      }
      
      if(!is.null(part_time)){
        ela_earnings_choice <- ela_earnings_choice %>%
          filter(Part_Time == part_time)
      }
      
      return(ela_earnings_choice)
    }

        })


table_earnings <- reactive({
  list_zipp <- plot_zipp()
  zip_code <- unlist(list_zipp)[1]
  zipcode <- postcode_sanitize(input$zipp)
  validationError <- postcode_defense(zipcode)
  
  if (is.null(validationError)) {
    table_earnings <- postcode()
    return(table_earnings)
  }
  else {
    return(data.frame())
  }
})

output$table_earnings <- renderDT(server=FALSE,{
  # Load data
  data <- table_earnings()
  # Show data
  datatable(data, extensions = 'Buttons', 
            options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                           paging = TRUE, searching = TRUE,
                           fixedColumns = TRUE, autoWidth = TRUE,
                           ordering = TRUE, dom = 'tB',
                           buttons = c('copy', 'csv', 'excel','pdf')))
})

}
###############################################################################
#Launcher
###############################################################################
shiny::shinyApp(ui, server)