### ENGLISH VERSION ###
### LOADING LIBRARY #############
library(readr)
library(shiny)
library(leaflet)
library(geojsonio)  # A package for geographic and spatial data, requires the latest version of dplyr
library(dplyr)      # Used for data manipulation and merging
library(htmltools)  # Used for constructing map labels using HTML

data <- read_csv("cleandata.csv")

### START BUILDING APP #########
selective = c("Inclusive, lower transfer-in" = 10,
              "Inclusive, higher transfer-in" = 11,
              "Selective, lower transfer-in" = 12,
              "Selective, higher transfer-in" = 13,
              "More selective, lower transfer-in" = 14,
              "More selective, higher transfer-in" = 15)
owner = c("Public" = 1, "Private Non-profit" = 2, "Prive For-profit" = 3)
region = c("New England" = 1, "Mid East" = 2, 
           "Great Lakes" = 3, "Plains" = 4, 
           "Southest" = 5, "Southwest" = 6, 
           "Rocky Moutains" = 7, "Far West" = 8, 
           "Outlying Areas" = 9, "U.S Services School" = 0)
location = c("Large City"=11, "Midsize City" = 12, "Small City" = 13, 
             "Large Suburb" = 21, "Midsize Suburb" = 22, "Small Suburb" = 23, 
             "Fringe Town" = 31, "Distant Town" = 32,"Remote Town" = 33, 
             "Fringe Rural" = 41, "Distant Rural" = 42, "Remote Rural" = 43)
choices = setNames(data$INSTNM,data$INSTNM)
# Define UI for application
ui <- fluidPage(
    # Application title
    tags$head(# Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js"),
        tags$style(HTML(".multicol {height: auto;
                                   -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 2;    /* Firefox */ 
                                   column-count: 2; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 }"))),
    leafletOutput("usmap", width = "100%", height = 800),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                  draggable = TRUE, top = 20, left = "auto", right = 20, bottom = "auto",
                  width = 440, height = "auto",
                  h2("U.S 4-Year Colleges Map"),
                  tags$div(align = 'left', 
                           class = 'multicol',
                  checkboxGroupInput("selective", h4("Admission"), selective, selective, inline = FALSE),
                  checkboxGroupInput("region", h4("Region"), region, region, inline = FALSE),
                  checkboxGroupInput("ownership", h4("School Type"), owner, owner, inline = FALSE),
                  checkboxGroupInput("location", h4("Location"), location, location, inline = FALSE)),
                  #selectInput("name", h4("Search a specific school"), choices, NULL),
                  selectInput("studentbody", h4("Student Body"), c("Co-ed", "Female Only", "Male Only"), "Co-ed")
    )
) 

# Define server 
server <- function(input, output) {
    
    newdata = reactive({
        #if_else(
            #is.null(input$name),
            a = subset(data, REGION %in% input$region & LOCALE %in% input$location & CONTROL %in% input$ownership & CCUGPROF %in% input$selective)
            if (input$studentbody == "Coed") {
                a = filter(a, MENONLY == 0 & WOMENONLY == 0)
            } else if (input$studentbody == "Female Only"){
                a = filter(a, WOMENONLY == 1)
            } else if(input$studentbody == "Male Only"){
                a = filter(a, MENONLY == 1)
            }
            return(a)
            #subset(data, INSTNM == input$name))
    })
    schoolPopup = reactive({paste("Institution:", "<strong>", newdata()$INSTNM, "</strong>",
                                  "<br/>", "Location: ", newdata()$CITY, ",", newdata()$STABBR,
                                  "<br/>", "Website: ", newdata()$INSTURL,
                                  "<br/>", "Admission Rate (%):", newdata()$ADM_RATE,
                                  "<br/>", "Average SAT Score: ", newdata()$SAT_AVG,
                                  "<br/>", "Average Age of Entry: ", newdata()$AGE_ENTRY,
                                  "<br/>", "Enrollment of degree-seeking undergraduate: ", newdata()$UGDS,
                                  "<br/>", "Male-Female students ratio", newdata()$UGDS_MEN, ":", newdata()$UGDS_WOMEN,
                                  "<br/>", "Proportion of full-time faculty (%): ", newdata()$PFTFAC,
                                  "<br/>", "Average Cost of Attendance ($):", newdata()$COSTT4_A,
                                  "<br/>", "In-sate tuition and fees ($):", newdata()$TUITIONFEE_IN,
                                  "<br/>", "Out-of-sate tuition and fees ($):", newdata()$TUITIONFEE_OUT,
                                  "<br/>", "Completion Rate (%):", newdata()$C100_4,
                                  "<br/>", "Rentention Rate (%):", newdata()$RET_FT4,
                                  "<br/>", "Unemployment Rate (%): ", newdata()$UNEMP_RATE,
                                  "<br/>", "Median Student Debt ($): ", newdata()$GRAD_DEBT_MDN,
                                  "<br/>", "Median Earnings after 10 years ($):", newdata()$MD_EARN_WNE_P10)})
    output$usmap = renderLeaflet(
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
            setView(lng = -80.85, lat = 33.45, zoom = 4.45) %>%
            addMarkers(lng = newdata()$LONGITUDE, lat = newdata()$LATITUDE,
                       label = lapply(paste("<strong>",newdata()$INSTNM, "</strong>"), HTML),
                       popup = lapply(schoolPopup(), HTML),
                       clusterOptions = markerClusterOptions())
    )
}
# Run the application 
shinyApp(ui = ui, server = server)
