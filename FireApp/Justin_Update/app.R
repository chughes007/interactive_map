#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Lives as lab_5 app




# Final App, dashboard


library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(sp)
library(raster)
library(tidyverse)

Subset_2000 <- read.csv("Subset_2000.csv")

SDI260_CFL_Change <- Subset_2000 %>% 
  mutate(CFL_Change = Average_260 - Average_nt) %>% 
  select(CFL_Change)


choice <- c("standras", "aspectras30", "SlopTeal30d")

standras <- raster("standras.tif")

aspectras30 <- raster("aspectras30.tif")

tiff_stack <- raster::stack("standras.tif", "aspectras30.tif", "SlopTeal30d.tif")

private <- st_read(dsn = ".", layer = "Private_Parcels")

private_t <- st_transform(private, "+init=epsg:4326")

private_tclass <- private_t %>% 
  select(Stand_Id)


pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(tiff_stack),
                    na.color = "transparent")


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

pines <- veg_df %>%
  filter(Type == "Pines")
oaks <- veg_df %>%
  filter(Type == "Oaks")
shrubs <- veg_df %>%
  filter(Type == "Shrubs")
wet <- veg_df %>%
  filter(Type == "Wet")
firs <- veg_df %>%
  filter(Type == "Firs")
grass <- veg_df %>%
  filter(Type == "Grasses_and_Forbs")
other <- veg_df %>%
  filter(Type == "Other")
resistant <- veg_df %>%
  filter(Fire == "Resistant")
not_resistant <- veg_df %>%
  filter(Fire == "Not_Resistant")
neutral <- veg_df %>%
  filter(Fire == "Neutral")


SAF_Cover <- st_read(dsn = ".", layer = "ForestCover")
SAF_Cover_df <- st_transform(SAF_Cover, "+init=epsg:4326")
SAF_class <- SAF_Cover_df %>%
  select(SAF_Name)

Mountain_Hemlock <- SAF_Cover_df %>%
  filter(SAF_Name == "Mountain Hemlock")
Red_Fir <- SAF_Cover_df %>%
  filter(SAF_Name == "Red Fir")
Whitebark_Pine <- SAF_Cover_df %>%
  filter(SAF_Name == "Whitebark Pine")
White_Fir <- SAF_Cover_df %>%
  filter(SAF_Name == "White Fir")
Western_White_Pine <- SAF_Cover_df %>%
  filter(SAF_Name == "Western White Pine")
Aspen <- SAF_Cover_df %>%
  filter(SAF_Name == "Aspen")
Lodgepole_Pine <- SAF_Cover_df %>%
  filter(SAF_Name == "Lodgepole Pine")
Cottonwood_Willow <- SAF_Cover_df %>%
  filter(SAF_Name == "Cottonwood - Willow")
Mixed_Conifer <- SAF_Cover_df %>%
  filter(SAF_Name == "Sierra Nevada Mixed Conifer")
Ponderosa_Pine <- SAF_Cover_df %>%
  filter(SAF_Name == "Pacific Ponderosa Pine")
Black_Oak <- SAF_Cover_df %>%
  filter(SAF_Name == "California Black Oak")
Jeffrey_Pine <- SAF_Cover_df %>%
  filter(SAF_Name == "Jeffrey Pine")
Canyon_Live_Oak <- SAF_Cover_df %>%
  filter(SAF_Name == "Canyon Live Oak")
Blue_Oak <- SAF_Cover_df %>%
  filter(SAF_Name == "Blue Oak - Gray Pine")
Cost_Live_Oak <- SAF_Cover_df %>%
  filter(SAF_Name == "California Coast Live Oak")
Mixed_Subalpine <- SAF_Cover_df %>%
  filter(SAF_Name == "California Mixed Subalpine")
Hard_Chaparral <- SAF_Cover_df %>%
  filter(SAF_Name == "Hard Chaparral")

dinkey_boundary <- st_read(dsn = ".", layer = "DinkeyBoundary")
dinkey_df <- st_transform(dinkey_boundary, "+init=epsg:4326")


palrainbow <- colorFactor(palette = rainbow(18), domain = SAFNames)
palrainbow_regdom <- colorFactor(palette = rainbow(51), domain = RegNames)

SAFNames <- unique(SAF_class$SAF_Name)
RegNames <- unique(veg_name$Name)
# Adapting this for my data

#requires: SDI260_CFL_Change from SavingSierras_Stats.Rmd (which requires Subset_2000.csv)
    #      spatial plot from HW 3 (ggplot like we created in lab 7 using geom_sf)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  
  dashboardHeader(title = "Title"),
  
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("First Tab", tabName = "tab_1"),
      menuItem("Second Tab", tabName = "tab_2"),
      menuItem("Historic Fire", tabName = "tab_3"),
      menuItem("Fuel Loading", tabName = "tab_4"),
      menuItem("Forest Cover", tabName = "tab_5")
  
      )
      
    ),
  
  
  dashboardBody(
    
    
    tabItems(
      
      tabItem(tabName = "tab_1",
              fluidRow(
                box(plotOutput("my_graph1", height = 342)), 
                box(title = "Choose Color:", 
                    sliderInput("bins",
                                "Number of bins:",
                                min = 1,
                                max = 50,
                                value = 30))
              )),
      
      tabItem(tabName = "tab_2",
              fluidRow(
                box(leafletOutput("my_graph2", height = 432)),
                box(title = "Choose Map:",
                    selectInput("class", 
                                "Choose Map:", 
                                choices = choice))
              )),
      
      tabItem(tabName = "tab_3",
              fluidRow(
                box(leafletOutput("my_graph3", height = 432)),
                box(title = "Departure from Historical Fire Regime",
                    selectInput("condclass", 
                                "Choose Departure Level:", 
                                choices = unique(cond_class$Departure)))
                )
              ),
      
      tabItem(tabName = "tab_4",
              fluidRow(
                box(leafletOutput("my_graph4", height = 432)),
                box(title = "Regional Dominant Vegetation")
              )
      ),
      
      tabItem(tabName = "tab_5",
              fluidRow(
                box(leafletOutput("my_graph5", height = 700, width = 700))
              )
      
              )
      )
    )
  )
  

    
  
server <- function(input, output){
  
  output$my_graph1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- SDI260_CFL_Change$CFL_Change 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$my_graph2 <- renderLeaflet({
    
    tiffmap <- subset(tiff_stack, input$class, drop=TRUE)

    leaflet(private_tclass) %>% 
      addTiles() %>% 
      addRasterImage(tiffmap, colors = pal, opacity = 0.8) %>% 
      addLegend (pal = pal, values = values(tiffmap),
                 title = "This is Map") %>% 
      addPolygons(color = "black",
                  weight = 0.5)})
  
  
  output$my_graph3 <- renderLeaflet({
    cond_sub <- cond_class %>%
      filter(Departure == input$condclass)

    leaflet(cond_sub) %>% 
      addTiles() %>% 
      addPolygons(data = cond_class,
                  weight = 0.5,
                  color = "red",
                  fillColor = "red",
                  fillOpacity = 0.5) %>%
      addPolygons(data = dinkey_df,
                  weight = 1,
                  color = "black",
                  fillColor = "grey",
                  fillOpacity = 0.3) %>%
      addPolygons(data = private_tclass,
                  weight = 0.5,
                  color = "black",
                  fillColor = "yellow",
                  fillOpacity = 0.3)})
    
  output$my_graph5 <- renderLeaflet({
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

