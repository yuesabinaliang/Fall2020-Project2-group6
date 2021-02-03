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
library(dplyr)
library(tibble)
library(viridis)
library(tidyverse)
library(shinythemes)
library(sf)
library(RCurl)
library(tmap)
library(rgdal)
library(plotly)
library(ggplot2)
library(reshape2)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)

#can run RData directly to get the necessary date for the app
#global.r will enable us to get new data everyday
#update data with automated script

shinyServer(function(input,output){
    # tab panel 1 - Maps
    
    ## Xinying Feng Maps part starts ------------------------------------------------------------------------------
    
    sum.formula <- JS("function(cluster) {
                var markers = cluster.getAllChildMarkers();
                var sum = 0;
                for (i = 0; i < markers.length; i++) {
                  sum += Number(markers[i].options.mag);
                }
                var size = sum/10000
                var c = ' marker-cluster-';
                  if (sum < 3000) {
                    c += 'small';
                  } else if (sum < 13000) {
                    c += 'medium';
                  } else {
                    c += 'large';
                  }
                return new L.DivIcon({ html: '<div><span>' + sum + '</span></div>', className: 'marker-cluster' + c, iconSize: L.point(size, size)});
              }")
    
    na_drop <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/data-by-modzcta.csv")
    na_drop <- melt(na_drop, id.vars=1:3, value.name="Count", variable.name="Category")
    zip_code <- read.csv('https://raw.githubusercontent.com/daling11/xxxxxxxx/master/zip_code_database.csv')
    colnames(zip_code)<-c('MODIFIED_ZCTA', 'lat', 'lon')
    na_drop <- merge(na_drop, zip_code, by='MODIFIED_ZCTA')
    #na_drop <- na_drop[which(na_drop$Category == "COVID_CASE_COUNT" | na_drop$Category == "COVID_DEATH_COUNT"),]
    #na_drop <- na_drop[rep(rownames(na_drop), na_drop$Count),]
    address <- read.csv('https://raw.githubusercontent.com/daling11/xxxxxxxx/master/Covid_test_site.csv')
    address$Zip <- as.character(address$Zip)
    
    shiny_data <- reactive(na_drop[which(na_drop$Category %in% input$Category & na_drop$BOROUGH_GROUP %in% input$Borough),])
    
    output$map <- renderLeaflet({
        map<-leaflet(options = leafletOptions(minZoom = 8, maxZoom = 18)) %>%
            setView(-73.9252853, 40.7910694, zoom = 10) %>%
            addTiles() %>%
            addCircleMarkers(lng = shiny_data()$lon, lat = shiny_data()$lat, options = markerOptions(mag = shiny_data()$Count), color='red', stroke = FALSE, fillOpacity = 0.5,
                             clusterOptions = markerClusterOptions(iconCreateFunction=JS(sum.formula),
                                                                   popup = paste("<b>", "Count", shiny_data()$Count, "<br/>", "<b>", "Borough:", shiny_data()$BOROUGH_GROUP)))%>%
            addProviderTiles(providers$CartoDB.Positron)%>%
            addLabelOnlyMarkers(lng = shiny_data()$lon, lat = shiny_data()$lat,
                                options = markerOptions(mag = shiny_data()$Count),
                                label =  shiny_data()$Count,
                                labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T),
                                clusterOptions = markerClusterOptions(iconCreateFunction=JS(sum.formula)))
        
    })
    ## Xinying Feng Maps part ends  ------------------------------------------------------------------------------
    
    # tab panel 2 - Search 
    
    ## Xinying Feng Search part starts ------------------------------------------------------------------------------
    
    output$table <- DT::renderDataTable(
        DT::datatable(address, filter = "top"))
    
    ## Xinying Feng Search part ends ------------------------------------------------------------------------------
    
    # tab panel 3 - Home 
    
    ## Yue Liang boxes part starts---------------------------------------------------------------------------------------
    case_number <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/summary.csv")
    output$total_case <- renderText({
        formatC(as.numeric(as.character(case_number[3,2])), format="d", big.mark=",") 
    })
    
    output$hospital_rate <- renderText({
        paste(round(100*as.numeric(as.character(case_number[4,2]))/as.numeric(as.character(case_number[3,2])), 1), "%", sep="")
    })
    
    output$death_rate <- renderText({
        paste(round(100*as.numeric(as.character(case_number[5,2]))/as.numeric(as.character(case_number[3,2])), 1), "%", sep="")
    })
    
    output$date <- renderText({
        paste(substring(as.character(case_number[8,2]),1,3),substring(as.character(case_number[8,2]),str_locate(as.character(case_number[8,2]), ' ')[1,1]+1,str_locate(as.character(case_number[8,2]), ',')[1,1]-1),sep=" ")
    })
    ## Yue Liang boxes part ends---------------------------------------------------------------------------------------
    
    # tab panel 4 - Statistical Analysis ------------------------------------------------------------------------------
    
    ## Yue Liang Statistical Plots starts -----------------------------------------------------------------------------
    
    by_boro <- data.frame(read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/by-boro.csv"))
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

    by_poverty <- data.frame(read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/by-poverty.csv"))
    
    by_poverty$CASE_RATE_ADJ <- by_poverty$CASE_RATE_ADJ/100000
    by_poverty$HOSPITALIZED_RATE_ADJ <- by_poverty$HOSPITALIZED_RATE_ADJ/100000
    by_poverty$DEATH_RATE_ADJ <- by_poverty$DEATH_RATE_ADJ/100000

    output$case_rate_poverty <- renderPlot({
        ggplot(by_poverty, aes(y=CASE_RATE_ADJ, x=reorder(by_poverty$POVERTY_GROUP,c(1,2,3,4)))) +
            geom_bar(stat="identity",fill="#edae49") +
            xlab("POVERTY_GROUP")
    })

    output$hospitalized_rate_poverty <- renderPlot({
        ggplot(by_poverty, aes(y=HOSPITALIZED_RATE_ADJ, x=reorder(by_poverty$POVERTY_GROUP,c(1,2,3,4)))) +
            geom_bar(stat="identity",fill="#edae49") +
            xlab("POVERTY_GROUP")
    })

    output$death_rate_poverty <- renderPlot({
        ggplot(by_poverty, aes(y=DEATH_RATE_ADJ, x=reorder(by_poverty$POVERTY_GROUP,c(1,2,3,4)))) +
            geom_bar(stat="identity",fill="#edae49") +
            xlab("POVERTY_GROUP")
    })

    output$case_count_poverty <- renderPlot({
        ggplot(by_poverty, aes(y=CASE_COUNT, x=reorder(by_poverty$POVERTY_GROUP,c(1,2,3,4)))) +
            geom_bar(stat="identity",fill="#edae49") +
            xlab("POVERTY_GROUP")
    })

    output$hospitalized_count_poverty <- renderPlot({
        ggplot(by_poverty, aes(y=HOSPITALIZED_COUNT, x=reorder(by_poverty$POVERTY_GROUP,c(1,2,3,4)))) +
            geom_bar(stat="identity",fill="#edae49") +
            xlab("POVERTY_GROUP")
    })

    output$death_count_poverty <- renderPlot({
        ggplot(by_poverty, aes(y=DEATH_COUNT, x=reorder(by_poverty$POVERTY_GROUP,c(1,2,3,4)))) +
            geom_bar(stat="identity",fill="#edae49") +
            xlab("POVERTY_GROUP")
    })
    
    
    poverty <- ggplot(by_poverty, aes(y=DEATH_RATE_ADJ, x=reorder(by_poverty$POVERTY_GROUP,c(1,2,3,4)))) + 
        geom_bar(stat="identity",fill="#edae49") +
        xlab("POVERTY_GROUP")
    
    output$death_rate_poverty_plot <- renderPlot({
        poverty
    })
        
    index <- data.frame(read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/data-by-day.csv"))
    
    index_df <- melt(index[,1:4],id="date_of_interest")
    index_df$date_of_interest<- as.Date(index_df$date_of_interest,format = "%m/%d/%y")
    
    index_gg <- ggplot(index_df,aes(x=date_of_interest,y=value,color=variable,group= variable)) +
                        geom_line() +
                        scale_x_date(date_labels = "%b/%d")
    
    output$ggplotly_index <- renderPlotly({
        ggplotly(index_gg) %>%
            layout(hoverlabel= c(bgcolor =  'black'))
    })
    
    ## Yue Liang Statistical Plots ends -----------------------------------------------------------------------------
    
    ## Yue Liang About parts starts -----------------------------------------------------------------------------
    url_YL <- a("LinkedIn", href="https://www.linkedin.com/in/yue-liang-cu/")
    url_XLS <- a("LinkedIn", href="https://www.linkedin.com/in/xiaolisun11235/")
    url_XYF <- a("LinkedIn", href="https://www.linkedin.com/in/feng-xinying/")
    url_HYW <- a("LinkedIn", href="https://www.linkedin.com/in/hanyi-wang-52004b17a/")
    
    
    output$LinkedIn_YL <- renderUI({
        url_YL
    })
    
    output$LinkedIn_XLS <- renderUI({
        url_XLS
    })
    
    output$LinkedIn_XYF <- renderUI({
        url_XYF
    })
    
    output$LinkedIn_HYW <- renderUI({
        url_HYW
    })
    
    ## Yue Liang About parts ends -----------------------------------------------------------------------------
})

