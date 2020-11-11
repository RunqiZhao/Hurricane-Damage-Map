#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(drat)
library(maps)
library(lubridate)
library(rgdal)
library(geojsonio)
library(tigris)
library(usmap)
library(leaflet)

# Load data
hrcc <- read.csv("hrc_county.csv", head = TRUE)
hrcs <- read.csv("hrc_statewide.csv", head = TRUE)
# hurrProj <- read.csv("PublicAssistanceFundedProjectsDetails.csv", head=TRUE)

# Prepare for mapping
# hrcc$projectSize <- as.factor(hrcc$projectSize)
hrcc$Fips <- str_pad(hrcc$Fips, 5, side = "left", "0")

# total obligated amount
hrccJ <- hrcc %>% 
    group_by(Fips,state,county) %>% 
    summarize(TotalAmount = sum(totalObligated), .groups = "drop") %>%
    rename(GEO_ID = Fips)

# json file
loadJ <- geojsonio::geojson_read("gz_2010_us_050_00_5m.json", what = "sp")

# match fips
loadJ$GEO_ID <- loadJ$GEO_ID %>% substr(start = 10, stop = 14)

# join two files
hrccJoin <- geo_join(loadJ, hrccJ, by = "GEO_ID", how = "inner")


# Shiny app
ui <- fluidPage(
    useShinydashboard(),
    title="Public Assistance Program Summary of Obligations",
    
    fluidRow(
        column(3,
               # radioButtons("view", "Select a view", c("Total Obligated", "Total Declarations", "Applicants with Funded Projects")),
               selectInput("diState", "Disaster-State", choices=c("All", unique(hrcc$state)))
               ),
        column(3,
               sliderInput("decDate", "Declaration Date", min=2009, max=2018, value=c(2009, 2018)),
               sliderInput("obDate", "Obligated Date", min=2009, max=2018, value=c(2009, 2018))
               ),
        column(6,
               # textOutput("TotalOb")
               # infoBox("Total Obligations", 10*1, icon=icon("credit-card")),
               valueBoxOutput("totalObBox")
               )
    ),
          leafletOutput("FEMAleaflet")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$FEMAleaflet <- renderLeaflet({
        # join two files
        hrccJSub <- hrccJ
        
        if(input$diState!="All"){
            hrccJSub <- hrccJSub %>%
                filter(state==input.diState)
        }
        
        hrccJoinSub <- geo_join(loadJ, hrccJSub, by = "GEO_ID", how = "inner")
        
        # color pal
        pal <- colorNumeric("Blues", domain = hrccJoinSub$TotalAmount)

        # popup
        i_popup <- paste0("<strong>State: </strong>", hrccJoinSub$state, "<br>",
                          "<strong>County: </strong>", hrccJoinSub$county, "<br>",
                          "<strong>Total Obligated Amount: </strong>", round(hrccJoinSub$TotalAmount,2))

        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            addProviderTiles(providers$Stamen.TonerLines,
                             options = providerTileOptions(opacity = 0.75)) %>%
            setView(-89.275673, 37.098, zoom = 4) %>%
            addPolygons(data = hrccJoinSub,
                        fillColor = ~pal(TotalAmount),
                        color = "#BDBDC3",
                        fillOpacity  = 1,
                        smoothFactor = 0.2,
                        weight = 1,
                        popup = i_popup) %>%
            addLegend(pal = pal,
                      values = hrccJoinSub$TotalAmount,
                      position="bottomright",
                      title = "Total Obligated Amount(2009-2018)")
    })
    # output$totalOb <- renderText({
    #     print("Total Obligated: ")
    # })
    output$totalObBox <- renderValueBox({
        valueBox(
            "Total", "100,000",
            color="blue"
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
