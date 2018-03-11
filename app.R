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

Subset_2000 <- read.csv("H:/2018 Winter/ESM 244 Adv Data Analysis/WebApp Code/Subset_2000.csv")

SDI260_CFL_Change <- Subset_2000 %>% 
  mutate(CFL_Change = Average_260 - Average_nt) %>% 
  select(CFL_Change)


choice <- c("standras", "aspectras30", "SlopTeal30d")

standras <- raster("G:/Models/FVS/ModelRuns/Inputs/standras.tif")

aspectras30 <- raster("G:/Models/FVS/ModelRuns/Inputs/aspectras30.tif")

tiff_stack <- raster::stack("G:/Models/FVS/ModelRuns/Inputs/standras.tif", "G:/Models/FVS/ModelRuns/Inputs/aspectras30.tif", "G:/Models/FVS/ModelRuns/Inputs/SlopTeal30d.tif")

private <- st_read(dsn = "G:/Data_Archive/Fresno_Parcels", layer = "Private_Parcels")

private_t <- st_transform(private, "+init=epsg:4326")

private_tclass <- private_t %>% 
  select(Stand_Id)



pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(tiff_stack),
                    na.color = "transparent")

# Adapting this for my data

#requires: SDI260_CFL_Change from SavingSierras_Stats.Rmd (which requires Subset_2000.csv)
    #      spatial plot from HW 3 (ggplot like we created in lab 7 using geom_sf)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  
  dashboardHeader(title = "Title"),
  
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("First Tab", tabName = "tab_1"),
      menuItem("Second Tab", tabName = "tab_2")
      
  
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
              ))
      
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
    
    #leaflet(private_tclass) %>% 
      #addTiles() %>% 
      
    
    leaflet(private_tclass) %>% 
      addTiles() %>% 
      addRasterImage(tiffmap, colors = pal, opacity = 0.8) %>% 
      addLegend (pal = pal, values = values(tiffmap),
                 title = "This is Map") %>% 
      addPolygons(color = "black",
                  weight = 0.5)
    

    
    
  })
  
  
}
  
  
  
  
shinyApp(ui = ui, server = server)

