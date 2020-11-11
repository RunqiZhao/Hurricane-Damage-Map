library(shiny)
library(shinydashboard)
library(knitr)
library(tidyverse)
library(magrittr)
library(drat)
library(maps)
library(ggplot2)
library(dplyr)
library(rgdal)
library(geojsonio)
library(tigris)
library(usmap)
library(leaflet)
library(grid)
library(kableExtra)
library(gridExtra)
library(lubridate)

hrcc <- read.csv("hrc_county.csv", head = TRUE)
hrcc$obligatedDate <-  as_datetime(hrcc$obligatedDate)
hrcc$obligatedYear <-  year(hrcc$obligatedDate)
hrcctable <- hrcc[,c(1:3,7,9:12,15)]
colnames(hrcctable) <-  c("DisasterNum","Declaration_Year","Incident","State","County","ProjectAmount","FederalShareObligated","ToatlObligated","Obligated_Year")


loadJ <- geojsonio::geojson_read("gz_2010_us_050_00_5m.json", what = "sp")
loadJ$GEO_ID <- loadJ$GEO_ID %>% substr(start = 10, stop = 14)


ui <- dashboardPage(
        dashboardHeader(title="MappingFEMA"),
        dashboardSidebar(
            sidebarMenu(
            menuItem("Table", tabName="Table"),
            menuItem("Map", tabName="Map")
            )
        ),
        
        dashboardBody(
          tabItems(
            tabItem("Table",
                         fluidRow(
                           column(4,
                                  sliderInput("Year",
                                              "Declaration Year:",
                                              min=2009, max=2018, value=c(2009, 2018), sep=""),
                                  sliderInput("Year3",
                                              "Obligated Year:",
                                              min=2009, max=2018, value=c(min(hrcctable$Obligated_Year), max(hrcctable$Obligated_Year)), sep="")
                           ), 
                           
                           column(4,
                                    selectInput("DisasterNum",
                                                "DisasterNum: ",
                                                c("All",
                                                  unique(hrcctable$DisasterNum)))
                             ),
                             
                            column(4,
                                    selectInput("Incident",
                                                "Incident Type:",
                                                c("All",
                                                  unique(hrcctable$Incident)))
                             ), 
                             column(4,
                                    selectInput("State",
                                                "State:",
                                                c("All",
                                                  unique(hrcctable$State)))
                             ),
                           
                             column(4,
                                    selectInput("County",
                                                "County:",
                                                c("All",
                                                  unique(hrcctable$County)))
                             ),
                         ),
                         DT::dataTableOutput("table")
                ),
                
            tabItem("Map",
                         
                         fluidRow(
                             column(4,
                                    sliderInput("Year2", "Declaration Year:", min=2009, max=2018, value=c(2009, 2018), sep=""),
                                    sliderInput("Year4",
                                                "Obligated Year:",
                                                min=2009, max=2018, value=c(min(hrcc$obligatedYear),max(hrcc$obligatedYear)),sep="")
                                    
                             ),
                             column(4,
                                    selectInput("Incident2",
                                                "Incident Type:",
                                                c("All",
                                                  unique(hrcc$incidentType)))
                             ),
                             column(4,
                                    selectInput("State2",
                                                "State:",
                                                c("All",
                                                  unique(hrcc$state)))
                             ),
                             column(4,
                                    radioButtons("Amount",
                                                "Select a View:",
                                                c("ProjectAmount","FederalShareObligated", "ToatlObligated"))
                             ),
                         ),
                         leafletOutput("LeafletPlot")
                )
            )
        )
)

server <- function(input, output) {
    
    output$table <- DT::renderDataTable(DT::datatable({
        data <- hrcctable
        data <- filter(data,Declaration_Year >= input$Year[1] & Declaration_Year <= input$Year[2])
        data <- filter(data,Obligated_Year >= input$Year3[1] & Obligated_Year <= input$Year3[2])
        if (input$DisasterNum != "All") {
            data <- data[data$DisasterNum == input$DisasterNum,]
        }
        # if (input$Year != "All") {
        #     data <- data[data$Year == input$Year,]
        # }
        if (input$Incident != "All") {
            data <- data[data$Incident == input$Incident,]
        }
        if (input$State != "All") {
            data <- data[data$State == input$State,]
        }
        if (input$County != "All") {
            data <- data[data$County == input$County,]
        }
        data
    }))
    
    output$LeafletPlot <- renderLeaflet({
        data <- hrcc
        loadJ <- loadJ
        # if (input$Year2 != "All") {
        #     data <- data[data$declarationYear == input$Year2,]
        # }
        data <- filter(data,declarationYear >= input$Year2[1] & declarationYear <= input$Year2[2])
        data <- filter(data,obligatedYear >= input$Year4[1] & obligatedYear <= input$Year4[2])
        if (input$Incident2 != "All") {
            data <- data[data$incidentType == input$Incident2,]
        }
        if (input$State2 != "All") {
            data <- data[data$state == input$State2,]
        }
       
        dataplot <- data %>% group_by(Fips)
        if (input$Amount == "ProjectAmount"){
            dataJ <-dataplot %>% 
                    summarise(state = unique(state), county = unique(county), 
                          PlotAmount = sum(projectAmount)/1000)
            
        }
        else if (input$Amount == "FederalShareObligated"){
            dataJ <-dataplot %>% 
                summarise(state = unique(state), county = unique(county), 
                          PlotAmount = sum(federalShareObligated)/1000)
        }
        else if (input$Amount == "ToatlObligated"){
            dataJ <-dataplot %>% 
                summarise(state = unique(state), county = unique(county), 
                          PlotAmount = sum(totalObligated)/1000)
        }
        
        dataJ <- dataJ %>% rename(GEO_ID = Fips)
        outValue <- boxplot(dataJ$PlotAmount)$out
        dataJ <- dataJ %>% filter(!(PlotAmount %in% outValue))
        dataJoin <- geo_join(loadJ, dataJ, by = "GEO_ID", how = "inner")
        pal <- colorNumeric("Blues", domain = dataJoin$PlotAmount)
        # popup 
        i_popup <- paste0("<strong>State: </strong>", 
                          dataJoin$state, "<br>", 
                          "<strong>County: </strong>", 
                          dataJoin$county, "<br>", 
                          "<strong>Amount: </strong>",
                          round(dataJoin$PlotAmount,2) ,
                          " ","thousands") 
        LeafletMap <- leaflet() %>% 
                        addProviderTiles("CartoDB.Positron") %>%
                        addProviderTiles(providers$Stamen.TonerLines,
                                         options = providerTileOptions(opacity = 0.75)) %>%
                        setView(-89.275673, 37.098, zoom = 4) %>%
                        addPolygons(data = dataJoin,
                                    fillColor = ~pal(PlotAmount), 
                                    color = "#BDBDC3",
                                    fillOpacity  = 0.5, 
                                    smoothFactor = 0.2,
                                    weight = 1,
                                    popup = i_popup) %>%
                        addLegend(pal = pal,
                                  values = dataJoin$PlotAmount,
                                  position="bottomright",
                                  title = paste0("Amount in thousands"))
        LeafletMap
    }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
