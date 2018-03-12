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

private_cfl <- st_read(dsn = "G:/ShinyApp/FireApp/CraigUpdate", layer = "private_cfls")

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


dinkey_boundary <- st_read(dsn = "G:/ShinyApp/FireApp", layer = "DinkeyBoundary")
dinkey_df <- st_transform(dinkey_boundary, "+init=epsg:4326")


# Adapting this for my data

#requires: SDI260_CFL_Change from SavingSierras_Stats.Rmd (which requires Subset_2000.csv)
    #      spatial plot from HW 3 (ggplot like we created in lab 7 using geom_sf)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  
  dashboardHeader(title = "Craig's Tabs"),
  
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Fire Severity on Private Lands", tabName = "tab_3")
      
  
      )
      
    ),
  
  
  dashboardBody(
    tabItems(

      tabItem(tabName = "tab_3",
              fluidRow(
                box(leafletOutput("my_graph3", height = 432)),
                box(title = "Private Lands Fire Severity",
                    selectInput("treatment", 
                                "Choose Treatment Type:", 
                                choices = unique(DataT$Treatment)))
                )
              )
    )
))

    
  
server <- function(input, output){
  
  
  output$my_graph3 <- renderLeaflet({
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

