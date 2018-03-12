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

RegimeNames <- unique(fireregime$FireRegime)
palfireregime <- colorFactor(palette = c("burlywood4", "yellow","orange","red", "darkgrey", "deepskyblue"), domain = RegimeNames)


Subset_2000 <- read.csv("Subset_2000.csv")

SDI260_CFL_Change <- Subset_2000 %>% 
  mutate(CFL_Change = Average_260 - Average_nt) %>% 
  select(CFL_Change)


choice <- c("Aspect", "Slope", "Elevation")



tiff_stack <- raster::stack("Aspect.tif", "Slope.tif", "Elevation.tif")

private <- st_read(dsn = ".", layer = "Private_Parcels")

private_t <- st_transform(private, "+init=epsg:4326")

private_tclass <- private_t %>% 
  select(Stand_Id)

# next, craig's data



private_cfl <- st_read(dsn = '.', layer = "private_cfls")

private_tcfl <- st_transform(private_cfl, "+init=epsg:4326") %>% 
  select(CFL_Cat_30, NoTreatSev, IgnitGA25, geometry)

Treatment <- private_tcfl%>% 
  select(CFL_Cat_30) %>% 
  mutate("Treated")
colnames(Treatment) <- c("Severity", "Treatment", "geometry")

NoTreatment <- private_tcfl%>% 
  select(NoTreatSev) %>% 
  mutate("UnTreated")
colnames(NoTreatment) <- c("Severity", "Treatment", "geometry")

DataT <- rbind(NoTreatment, Treatment)

color <- colorFactor(palette = "Reds", 
                     domain = c(0,1,2,3,4,5,6), 
                     na.color = "transparent")


dinkey_boundary <- st_read(dsn = '.', layer = "DinkeyBoundary")
dinkey_df <- st_transform(dinkey_boundary, "+init=epsg:4326")



# Adapting this for my data

#requires: SDI260_CFL_Change from SavingSierras_Stats.Rmd (which requires Subset_2000.csv)
    #      spatial plot from HW 3 (ggplot like we created in lab 7 using geom_sf)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  
  dashboardHeader(title = "Saving Sierras App"),
  
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Fire Severity Histogram", tabName = "tab_1"),
      menuItem("Topographical Information", tabName = "tab_2"),
      menuItem("Fire History", tabName = "tab_3"),
      menuItem("Forest Cover", tabName = "tab_4"),
      menuItem("Fire Severity on Private Lands", tabName = "tab_5")
  
      )
      
    ),
  
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "tab_1",
              fluidRow(
                box(plotOutput("my_graph1", height = 342)), 
                box(title = "Choose Bins:", 
                    sliderInput("bins",
                                "Number of bins:",
                                min = 1,
                                max = 100,
                                value = 30),
                    selectInput(inputId = "bincolor",
                                label = "Color",
                                choices = colors(),
                                selected = "firebrick"),
                    
                    checkboxInput(inputId = "addmean",
                                  label = "Add Mean Line?",
                                  value = FALSE)),
                tabBox(tabPanel("Summary", verbatimTextOutput("summary")))
              )),
      
      tabItem(tabName = "tab_2",
              fluidRow(
                box(leafletOutput("my_graph2", height = 432)),
                box(title = "Dinkey Landscape Maps",
                    selectInput("class", 
                                "Choose Map:", 
                                choices = choice))
              )),

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
      
              ),
      
      tabItem(tabName = "tab_5",
              fluidRow(
                box(leafletOutput("my_graph5", height = 432)),
                box(title = "Private Lands Fire Severity",
                    selectInput("treatment", 
                                "Choose Treatment Type:", 
                                choices = unique(DataT$Treatment)))
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
    
    output$summary <- renderPrint({
      x <- SDI260_CFL_Change$CFL_Change             # Define x again
      summary(x, digits = 3)
    })
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, border = 'white', col = input$bincolor, main = "Change in Fire Severity After Treatment", xlab = "Change in Conditional Flame Length (ft)")
    if(input$addmean) {
      
      abline(v = mean(x),
             lwd = 2,
             lty = 2)
      
    }
    
  })
  
  
  
  
  output$my_graph2 <- renderLeaflet({
    
    tiffmap <- subset(tiff_stack, input$class, drop=TRUE)
    
    
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(tiffmap),
                        na.color = "transparent")
    #leaflet(private_tclass) %>% 
    #addTiles() %>% 
    
    
    leaflet(private_tclass) %>% 
      addTiles() %>% 
      addRasterImage(tiffmap, colors = pal, opacity = 0.8) %>% 
      addLegend (pal = pal, values = values(tiffmap),
                 title = input$class) %>% 
      addPolygons(color = "black",
                  weight = 0.5, fill = NA)
    
    
    
    
    
  })
  
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
  
  output$my_graph5 <- renderLeaflet({
    private_map <- DataT %>%
      filter(Treatment == input$treatment)
    
    leaflet(private_map) %>% 
      addTiles() %>% 
      addPolygons(weight = 0.5,
                  color = "Black",
                  fillColor = ~color(private_map$Severity),
                  fillOpacity = .9) %>%
      addPolygons(data = dinkey_df,
                  weight = 2.0,
                  color = "Grey",
                  fillColor = "Transparent",
                  opacity = 1.0) %>% 
      addLegend (pal = color, values = DataT$Severity,
                 title = "Fire Severity Level",
                 opacity = 1.0)
  })
  
}
  
  
  
  
shinyApp(ui = ui, server = server)

