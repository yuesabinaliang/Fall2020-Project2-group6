# server

packages.used <- c("shiny","leaflet", "wordcloud2", "DT", "stringr", "dplyr", "tidyverse", "tibble")

# check packages that need to be installed.

packages.needed <- setdiff(packages.used, 
                           intersect(installed.packages()[,1], 
                                     packages.used))

# install additional packages
if(length(packages.needed) > 0){
    install.packages(packages.needed, dependencies = TRUE)
}


library(shiny)
library(leaflet)
library(readr)
library(wordcloud2)
library(DT)
library(stringr)
library(tidyverse)
library(dplyr)
library(tibble)
library(viridis)
library(dplyr)
library(tibble)
library(tidyverse)
library(shinythemes)
library(sf)
library(RCurl)
library(tmap)
library(rgdal)
library(leaflet)
library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(reshape2)
#can run RData directly to get the necessary date for the app
#global.r will enable us to get new data everyday
#update data with automated script

shinyServer(function(input,output){
    # tab panel 1 - Maps
    # Xinying Feng Maps part
    na_drop <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/data-by-modzcta.csv")
    na_drop <- melt(na_drop, id.vars=1:3, value.name="Count", variable.name="Category")
    zip_code <- read.csv('https://raw.githubusercontent.com/daling11/xxxxxxxx/master/zip_code_database.csv')
    colnames(zip_code)<-c('MODIFIED_ZCTA', 'lat', 'lon')
    na_drop <- merge(na_drop, zip_code, by='MODIFIED_ZCTA')
    # na_drop <- na_drop[rep(rownames(na_drop),na_drop$Count),]
    
    shiny_data <- reactive(na_drop[which(na_drop$Category %in% input$Category & na_drop$BOROUGH_GROUP %in% input$Borough),])

    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 8, maxZoom = 18)) %>%
            setView(-73.9252853, 40.7910694, zoom = 10) %>%
            addTiles() %>%
            addMarkers(data = shiny_data()$Count, lng = shiny_data()$lon, lat = shiny_data()$lat,
                       clusterOptions = markerClusterOptions()
            )
    })

    observe({
        df.marker <- shiny_data()
        leafletProxy("mapMarker", data = df.marker) %>%
            clearMarkerClusters()%>%
            clearPopups() %>%
            clearMarkers() %>%
            addMarkers(lng = shiny_data()$lon, lat = shiny_data()$lat,
                       popup = paste("<b>", "Category:", "Count", shiny_data()$Count,
                                     "<br/>", "<b>", "Borough:", shiny_data()$BOROUGH_GROUP))
        clusterOptions <- markerClusterOptions()
    })
    
    # tab panel 2 - Home -------------------------------------------------------------------------------------------------------
    
    ## Yue Liang boxes part
    case_number <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/summary.csv")
    output$total_case <- renderValueBox({
        formatC(as.numeric(as.character(case_number[1,2])), format="d", big.mark=",") %>%
            valueBox(subtitle = "Cumulative Counts of Cases",
                     icon = icon("biohazard"),
                     color = "orange")
    })
    
    output$death_rate <- renderValueBox({
        paste(round(100*as.numeric(as.character(case_number[3,2]))/as.numeric(as.character(case_number[1,2])), 2), "%", sep="") %>%
            valueBox(subtitle = "Comfirmed Death Rate",
                     icon = icon("percentage"),
                     color = "purple")
    })
    
    output$date <- renderValueBox({
        valueBox(paste(substring(as.character(case_number[5,2]),1,3),substring(as.character(case_number[5,2]),str_locate(as.character(case_number[5,2]), ' ')[1,1]+1,str_locate(as.character(case_number[5,2]), ',')[1,1]-1),sep=" "),subtitle = "Update Date",
                     icon = icon("calendar"),
                     color = "green")
    })
    
    # Home end -----------------------------------------------------------------------------------------------------
    
    by_boro <- data.frame(read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/by-boro.csv"))
    by_boro$CASE_RATE <- by_boro$CASE_RATE/100000
    by_boro$HOSPITALIZED_RATE <- by_boro$HOSPITALIZED_RATE/100000
    by_boro$DEATH_RATE <- by_boro$DEATH_RATE/100000
    
    output$CASE_RATE <- renderPlot({
        ggplot(by_boro, aes(y=CASE_RATE, x=reorder(by_boro$BOROUGH_GROUP,c(1,2,3,4,5,6)))) + 
        geom_bar(stat="identity",fill="lightblue") +
        xlab("BOROUGH_GROUP")
        })
        
    output$HOSPITALIZED_RATE <- renderPlot({
        ggplot(by_boro, aes(y=HOSPITALIZED_RATE, x=reorder(by_boro$BOROUGH_GROUP,c(1,2,3,4,5,6)))) + 
        geom_bar(stat="identity",fill="lightblue") +
        xlab("BOROUGH_GROUP")
    })
    
    output$DEATH_RATE <- renderPlot({
        ggplot(by_boro, aes(y=DEATH_RATE, x=reorder(by_boro$BOROUGH_GROUP,c(1,2,3,4,5,6)))) + 
        geom_bar(stat="identity",fill="lightblue") +
        xlab("BOROUGH_GROUP")
    })
    
    output$CASE_COUNT <- renderPlot({
        ggplot(by_boro, aes(y=CASE_COUNT, x=reorder(by_boro$BOROUGH_GROUP,c(1,2,3,4,5,6)))) + 
        geom_bar(stat="identity",fill="lightblue") +
        xlab("BOROUGH_GROUP")
    })
    
    output$HOSPITALIZED_COUNT <- renderPlot({
        ggplot(by_boro, aes(y=HOSPITALIZED_COUNT, x=reorder(by_boro$BOROUGH_GROUP,c(1,2,3,4,5,6)))) + 
        geom_bar(stat="identity",fill="lightblue") +
        xlab("BOROUGH_GROUP")
    })
    
    output$DEATH_COUNT <- renderPlot({
        ggplot(by_boro, aes(y=DEATH_COUNT, x=reorder(by_boro$BOROUGH_GROUP,c(1,2,3,4,5,6)))) + 
        geom_bar(stat="identity",fill="lightblue") +
        xlab("BOROUGH_GROUP")
    })
    
    by_poverty <- data.frame(read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/by-poverty.csv"))
    
    poverty <- ggplot(by_poverty, aes(y=CASE_RATE_ADJ, x=POVERTY_GROUP)) + 
        geom_bar(stat="identity",fill="#edae49")

    output$case_rate_poverty <- renderPlotly({
        ggplotly(poverty)
    })
    
    index <- data.frame(read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/case-hosp-death.csv"))
    
    index_df <- melt(index[,1:4],id="DATE_OF_INTEREST")
    index_df$DATE_OF_INTEREST<- as.Date(index_df$DATE_OF_INTEREST,format = "%m/%d/%y")
    
    index_gg <- ggplot(index_df,aes(x=DATE_OF_INTEREST,y=value,color=variable,group= variable)) +
                        geom_line() +
                        scale_x_date(date_labels = "%b/%d")
    
    output$ggplotly_index <- renderPlotly({
        ggplotly(index_gg)
    })
    
    
})

