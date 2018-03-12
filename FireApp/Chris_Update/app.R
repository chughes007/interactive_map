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

# Adapting this for my data

#requires: SDI260_CFL_Change from SavingSierras_Stats.Rmd (which requires Subset_2000.csv)
    #      spatial plot from HW 3 (ggplot like we created in lab 7 using geom_sf)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  
  dashboardHeader(title = "Fire Severity"),
  
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Fire Severity Histogram", tabName = "tab_1"),
      menuItem("Static Maps", tabName = "tab_2")
      
  
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
              ))
      

      

      
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
  
  

    
  }
  

  
  
  
  
shinyApp(ui = ui, server = server)

