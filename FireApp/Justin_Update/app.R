#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Final App, dashboard


library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(sp)
library(raster)
library(tidyverse)

private <- st_read(dsn = ".", layer = "Private_Parcels")

private_t <- st_transform(private, "+init=epsg:4326")

private_tclass <- private_t %>% 
  select(Stand_Id)


veg_types <- st_read(dsn = ".", layer = "RegAgg")
veg_df <- st_transform(veg_types, "+init=epsg:4326")
veg_fire_class <- veg_df %>%
  select(Fire)
regdom_veg_name <- veg_df %>%
  select(Name)

condclass <- st_read(dsn = ".", layer = "conclassdiss")
cond_df <- st_transform(condclass, "+init=epsg:4326")
cond_class <- cond_df %>%
  select(Departure)

fireregime <- st_read(dsn = ".", layer = "fireregime")
regime_df <- st_transform(fireregime, "+init=epsg:4326")
regime_class <- regime_df %>%
  select(FireRegime)

Low_Severity <- regime_class %>%
  filter(FireRegime == "I - Low Severity Fire")
Mid_Severity <- regime_class %>%
  filter(FireRegime == "III - Mixed Severity FIre")
High_Severity <- regime_class %>%
  filter(FireRegime == "IV = High Severity Fire")

SAF_Cover <- st_read(dsn = ".", layer = "ForestCover")
SAF_Cover_df <- st_transform(SAF_Cover, "+init=epsg:4326")
SAF_class <- SAF_Cover_df %>%
  select(SAF_Name)

dinkey_boundary <- st_read(dsn = ".", layer = "DinkeyBoundary")
dinkey_df <- st_transform(dinkey_boundary, "+init=epsg:4326")

SAFNames <- unique(SAF_class$SAF_Name)
palrainbow <- colorFactor(palette = rainbow(18), domain = SAFNames)

RegNames <- unique(veg_name$Name)
palrainbow_regdom <- colorFactor(palette = rainbow(51), domain = RegNames)

RegimeNames <- unique(fireregime$FireRegime)
palfireregime <- colorFactor(c("brown", "yellow","orange","red", "grey", "lightblue"), RegimeNames[1:6])


# Adapting this for my data

#requires: SDI260_CFL_Change from SavingSierras_Stats.Rmd (which requires Subset_2000.csv)
    #      spatial plot from HW 3 (ggplot like we created in lab 7 using geom_sf)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  
  dashboardHeader(title = "Title"),
  
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Fire History", tabName = "tab_3"),
      menuItem("Forest Cover", tabName = "tab_4")
  
      )
      
    ),
  
  
  dashboardBody(
    

      tabItem(tabName = "tab_3",
              fluidRow(
                box(leafletOutput("my_graph3", height = 432)),
                box(title = "Historical Fire Regime",
                    selectInput("regime_class", 
                                "Choose Regime Level:", 
                                choices = unique(regime_class$FireRegime)))
                )
              ),
      
      tabItem(tabName = "tab_4",
              fluidRow(
                box(leafletOutput("my_graph4", height = 700, width = 700))
              )
      
              )
      )
    )
  

    
  
server <- function(input, output){
  
  
  output$my_graph3 <- renderLeaflet({
    regime_sub <- regime_class %>%
      filter(FireRegime == input$regime_class)

    leaflet(regime_sub) %>% 
      addTiles() %>% 
      addPolygons(weight = 0.5,
                  color = "black",
                  fillColor = ~palfireregime(RegimeNames),
                  fillOpacity = 0.5) %>%
      addPolygons(data = dinkey_df,
                  weight = 1,
                  color = "black",
                  fillColor = "transparent") %>%
      addPolygons(data = private_tclass,
                  weight = 0.5,
                  color = "black",
                  fillColor = "yellow",
                  fillOpacity = 0.3)})
    
  output$my_graph4 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data = private_tclass,
                  weight = 0.5,
                  color = "black",
                  fillColor = "yellow",
                  fillOpacity = 0.5,
                  group = "Private Parcels") %>%
      addPolygons(data = dinkey_df,
                  weight = 1,
                  color = "black",
                  fillColor = "grey",
                  fillOpacity = 0.5,
                  group = "Dinkey Boundary") %>%
      addPolygons(data = SAF_class,
                  weight = 0.5,
                  color = "black",
                  fillColor = ~palrainbow(SAFNames),
                  fillOpacity = 0.5,
                  group = "Vegetation") %>%
      addLegend(pal = palrainbow, 
                values = SAFNames,
                title = "Forest Cover Types") %>%
      addLayersControl(
        baseGroups = c("Vegetation"),
        overlayGroups = c("Dinkey Boundary", "Private Parcels"),
        options = layersControlOptions(collapsed = FALSE)
      )
      
      
    })
  
}
  
  
  
  
shinyApp(ui = ui, server = server)

