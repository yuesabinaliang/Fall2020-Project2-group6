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
    
})


# shinyServer(function(input, output) {
# #----------------------------------------
# #tab panel 1 - Home Plots
# #preapare data for plot
# output$case_overtime <- renderPlotly({
#     #determin the row index for subset
#     req(input$log_scale)
#     end_date_index <- which(date_choices == input$date)
#     #if log scale is not enabled, we will just use cases
#     if (input$log_scale == FALSE) {
#         #render plotly figure
#         case_fig <- plot_ly()
#         #add comfirmed case lines
#         case_fig <- case_fig %>% add_lines(x = ~date_choices[1:end_date_index], 
#                              y = ~as.numeric(aggre_cases[input$country,])[1:end_date_index],
#                              line = list(color = 'rgba(67,67,67,1)', width = 2),
#                              name = 'Confirmed Cases')
#         #add death line 
#         case_fig <- case_fig %>% add_lines(x = ~date_choices[1:end_date_index],
#                                y = ~as.numeric(aggre_death[input$country,])[1:end_date_index],
#                                name = 'Death Toll')
#         #set the axis for the plot
#         case_fig <- case_fig %>% 
#             layout(title = paste0(input$country,'\t','Trend'),
#                    xaxis = list(title = 'Date',showgrid = FALSE), 
#                    yaxis = list(title = 'Comfirmed Cases/Deaths',showgrid=FALSE)
#                    )
#         }
#     #if enable log scale, we need to take log of the y values
#     else{
#         #render plotly figure
#         case_fig <- plot_ly()
#         #add comfirmed case lines
#         case_fig <- case_fig %>% add_lines(x = ~date_choices[1:end_date_index], 
#                                            y = ~log(as.numeric(aggre_cases[input$country,])[1:end_date_index]),
#                                            line = list(color = 'rgba(67,67,67,1)', width = 2),
#                                            name = 'Confirmed Cases')
#         #add death line 
#         case_fig <- case_fig %>% add_lines(x = ~date_choices[1:end_date_index],
#                                            y = ~log(as.numeric(aggre_death[input$country,])[1:end_date_index]),
#                                            name = 'Death Toll')
#         #set the axis for the plot
#         case_fig <- case_fig %>% 
#             layout(title = paste0(input$country,'<br>','\t','Trends'),
#                    xaxis = list(title = 'Date',showgrid = FALSE), 
#                    yaxis = list(title = 'Comfirmed Cases/Deaths(Log Scale)',showgrid=FALSE)
#             )
#     }
#     return(case_fig)
#         })
# #----------------------------------------
# #tab panel 2 - Maps
# na_drop <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/data-by-modzcta.csv")
# na_drop <- melt(na_drop, id.vars=1:3, value.name="Count", variable.name="Category")
# zip_code <- read.csv('https://raw.githubusercontent.com/daling11/xxxxxxxx/master/zip_code_database.csv')
# colnames(zip_code)<-c('MODIFIED_ZCTA', 'lat', 'lon')
# na_drop <- merge(na_drop, zip_code, by='MODIFIED_ZCTA')
# 
# shiny_data <- reactive(na_drop[which(na_drop$Category %in% input$Category & na_drop$BOROUGH_GROUP %in% input$Borough),])
# 
# output$map <- renderLeaflet({
#     leaflet(options = leafletOptions(minZoom = 8, maxZoom = 18)) %>%
#         setView(-73.9252853, 40.7910694, zoom = 10) %>%
#         addTiles() %>%
#         addMarkers(data = shiny_data()$Count, lng = shiny_data()$lon, lat = shiny_data()$lat,
#                    clusterOptions = markerClusterOptions()
#         )
# })
# 
# observe({
#     df.marker <- shiny_data()
#     leafletProxy("mapMarker", data = df.marker) %>%
#         clearMarkerClusters()%>%
#         clearPopups() %>%
#         clearMarkers() %>%
#         addMarkers(lng = shiny_data()$lon, lat = shiny_data()$lat,
#                    popup = paste("<b>", "Category:", "Count", shiny_data()$Count,
#                                  "<br/>", "<b>", "Borough:", shiny_data()$BOROUGH_GROUP))
#     clusterOptions <- markerClusterOptions()
# })
# }