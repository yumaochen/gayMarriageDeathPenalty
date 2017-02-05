#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(RColorBrewer)
library(maps)
library(rworldmap)
library(dplyr)
library(RCurl)

# Define UI for application that draws a histogram
ui <- fillPage (
  leafletOutput("mymap", width = "100%", height = "100%"),
  p(),
  padding = 10
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  gay_dp_country = read.csv("https://raw.githubusercontent.com/yumaochen/gayMarriageDeathPenalty/master/gay_dp_country.csv")
  
  gaydpSD = joinCountryData2Map(gay_dp_country, joinCode = "ISO3", nameJoinColumn = "ISO3Code")
  
  gaycountry = subset(gay_dp_country, label=="gay")
  dpcountry = subset(gay_dp_country, label=="death")
  bothcountry = subset(gay_dp_country, label=="both")
  
  
  gaySD = joinCountryData2Map(gaycountry, joinCode = "ISO3", nameJoinColumn = "ISO3Code")
  dpSD = joinCountryData2Map(dpcountry, joinCode = "ISO3", nameJoinColumn = "ISO3Code")
  usaSD = joinCountryData2Map(bothcountry, joinCode = "ISO3", nameJoinColumn = "ISO3Code")
  
  gaySD = gaySD[which(gaySD$ISO3Code!="NA"),]
  dpSD = dpSD[which(dpSD$ISO3Code!="NA"),]
  usaSD = usaSD[which(usaSD$ISO3Code!="NA"),]
  
  ################################# output ##
  output$mymap <- renderLeaflet({
    leaflet() %>%   setMaxBounds(-300, -150, 300, 150) %>%
      
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = F, minZoom=2)
      ) %>%
      
      addPolygons(data = gaySD, fill = TRUE, stroke = F, fillColor = "#f03b20", group = "Same-Sex Marriage",
                  popup = paste0("Country: ", as.character(gaySD$country)),  opacity = 0.2
      ) %>% 
      
      addPolygons(data = dpSD, fill = TRUE, stroke = F, fillColor = "#c51b8a", group = "Death Penalty",
                  popup = paste0("Country: ", as.character(dpSD$country)),  opacity = 0.2
      ) %>% 
      
      addPolygons(data = usaSD, fill = TRUE, stroke = F, fillColor = "#fec44f", group = "Both", 
                  popup = paste0("Country: ", as.character(usaSD$country)),  opacity = 0.2
      ) %>% 
      
      addLegend("bottomright", colors = c("#f03b20", "#c51b8a", "#fec44f"), 
                labels = c("Legal Same-Sex Marriage", "Legal Death Penalty", "Both")
      ) %>%   
      
      addLayersControl(
        overlayGroups = c("Same-Sex Marriage", "Death Penalty", "Both"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  ##  output #################################
}

# Run the application 
shinyApp(ui = ui, server = server)

