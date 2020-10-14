#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#ag
#    http://shiny.rstudio.com/
#
# Define UI for application that draws a histogram

# Load all the required libraries 
packages.used <- c("shiny", "shinydashboard", 
                   "leaflet", "shinyWidgets","plotly","shinythemes","reshape2","DT")

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

bootstrapPage(
  tags$head(includeHTML("www/gtag.html")),
  navbarPage(theme = shinytheme("flatly"),
             collapsible = TRUE,
             "NYC COVID-19",
             id="nav",

           ### Yue Liang Home part starts--------------------------------------------------------------------------------------------------------

           tabPanel("Home",icon = icon("home"),

                    ## Yue Liang Summary part -----------------------------------------------------------------------------------
                    
                    div(class = "container-fluid",
                      fluidRow(column(width=2,align="center",
                                      div(class="container",
                                          h3(br()),
                                          h3("Updated on",
                                             br(),br(),
                                             textOutput("date"))),
                                      br(),
                                      h3(textOutput("total_case"),
                                        style = "font-family: Impact;
                                                  font-size:40px ;
                                                  line-height:50%;"),
                                      h3("Cases",style = "line-height: 60%"),
                                      p("_________",
                                        style = "line-height: 0;
                                        font-size=10px;"),
                                        br(),
                                      column(width=1,align="right",
                                             h4(textOutput("hospital_rate"),
                                                style = "font-family: Impact;
                                                  font-size:30px ;
                                                  line-height:50%;"),
                                             h4("Hospital Rate",style = "line-height: 100%"),
                                             p("_________",
                                               style = "line-height: 0;
                                                font-size=10px;")),
                                      column(width=1,align="center",
                                             h4(textOutput("death_rate"),
                                                style = "font-family: Impact;
                                                  font-size:30px ;
                                                  line-height:50%;"),
                                             h4("Death Rate",style = "line-height: 100%"),
                                             p("_________",
                                               style = "line-height: 0;
                                        font-size=10px;"),offset=4),
                                      br(),br(),br(),br(),br(),br(),
                                        fluidRow(p("The world cloud of NYC Covid-19 report shows that we care most about how does Covid-19 affect NYC and how can we get tests."))),
                              
                        ## Xiaoli Sun Word Cloud part begins----------------------------------------------------------------------------             
                               column(width=8,
                                      box(width = "100%",height=300,includeHTML("wordcloud.html")))
                                    ))
                        ## Xiaoli Sun Word Cloud part ends----------------------------------------------------------------------------             
                  ),


            ### Yue Liang home end --------------------------------------------------------------------------------------------------------

            ### Xinying Feng MAP Part Begin ------------------------------------------------------------------------------------

             tabPanel("MAP",icon = icon("map-marked-alt"),
                  div(class="outer",
                     tags$head(includeCSS("www/styles.css")),
                      leafletOutput("map",width="100%",height="100%"),
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                    top = 160, left = 160, right = "auto", bottom = "auto", width = 250, height = "auto",

                                    span(tags$i(h4("Select your borough to see the local cases numbers and locations."))),
                                    checkboxGroupInput("Borough", "Choose Borough:",
                                                       choices = c("Manhattan", "Staten Island",
                                                                   "Bronx", "Queens",
                                                                   "Brooklyn"),
                                                       selected = c("Manhattan", "Staten Island",
                                                                    "Bronx", "Queens",
                                                                    "Brooklyn")
                                    ),
                                    span(tags$i(h4("Check the comfirmed case numbers or the death numbers."))),
                                    radioButtons("Category", "Choose category:",
                                                 choices = c("COVID_CASE_COUNT","COVID_DEATH_COUNT")),
                                    span(tags$i(h4("Click on the numbers to zoom in and see further details.")))
                                    )
                     )
                  ),

          ### Xinying Feng MAP Part Ends -------------------------------------------------------------

          ### Xiaoli Sun & Yue Liang statistical analysis part begins-------------------------------------------------

          tabPanel("Time Series",icon = icon("chart-line"),
              fluidPage(
                  fluidRow(column(12,
                                h2("The number of cases changing from time to time"),
                                h4("Since we are about how Covid-19 have impact on the NYC, let's see the the number of 
                                   cases changing from time to time."),
                                tags$div(tags$ul(
                                  tags$li("Hover the mouse over a line to read detailed numbers in the time series."),
                                  tags$li("Select a time period by circling on the plot to zoom in and see details. Double
                                          click to zoom back out."),
                                  tags$li("You may select", span("Compare Data On Hover", style = "background-color: #D3D3D3;"),
                                          "or", span("Toggle Spike Lines", style = "background-color: #D3D3D3;"),
                                          "to help you visualize the data on the control panel, which is on the right upper
                                          corner in the plot showing when hover over.")
                                )),
                         plotlyOutput("ggplotly_index")))
                         )),

          navbarMenu("Analysis", icon = icon("chart-area"),

                # Yue Liang sub1 begins---------------------------------------------------------------------------------------
                     tabPanel("Borough",icon = icon("location-arrow"),
                              fluidPage(
                                fluidRow(column(12,
                                       h3("Covid-19 Stats by Borough"),
                                       "In the plots on the left hand side, we plot the numbers of comfirmed cases,
                                        hospitalized cases and death cases grouping by different boroughs 
                                        in New York City. 
                                        On the right hand side, we plot the rates of confirmed cases, hospitalizations, and deaths,
                                        by NYC borough of residence in percentage.",
                                       br(),
                                       tags$div(tags$ul(
                                         tags$li("You may select different tab items on the two plots, and compare the two plots."),
                                         tags$li("Compare to the citywide rates, you may get a sense of which borough is more affected by the Covid-19.")
                                       )),
                                       br()
                                       )),
                                fluidRow
                                (tabBox(
                                  width=6,
                                  title = tagList(shiny::icon("user"), "Cases Number by Borough"),
                                  tabPanel("Case Count",
                                           plotOutput("CASE_COUNT")),
                                  tabPanel("Hospitalized Count",
                                           plotOutput("HOSPITALIZED_COUNT")),
                                  tabPanel("Death Count",
                                           plotOutput("DEATH_COUNT"))),

                                  tabBox(
                                    width=6,
                                    title = tagList(shiny::icon("percent"), "Cases Rate by Borough Population"),
                                    tabPanel("Case Rate",
                                             plotOutput("CASE_RATE")),
                                    tabPanel("Hospitalized Rate",
                                             plotOutput("HOSPITALIZED_RATE")),
                                    tabPanel("Death Rate",
                                             plotOutput("DEATH_RATE")))
                                ))),
                # Yue Liang sub1 ends---------------------------------------------------------------------------------------

                # Xiaoli Sun sub2 begins----------------------------------------------------------------------------------------

                     tabPanel("Race",icon = icon("user-friends"),
                              fluidPage(
                                fluidRow(column(8,
                                                h3("Covid-19 Stats by Race"),
                                                "In the fowllowing plots, we plot the numbers of comfirmed cases,
                                                hospitalized cases and death cases grouping by different races. 
                                                We use D3 object here to implement interactive plot. ",
                                                br(),
                                                tags$div(tags$ul(
                                                  tags$li("By default, the bar chart in the right hand side shows the overall number 
                                                          of comfirmed cases in different race groups."),
                                                  tags$li("Click the name of the statistic summary you want to see on the left pie chart. 
                                                           The bar chart on the right hand side shows the stats.")
                                                ))),
                                         column(3,
                                                br(),
                                                p(strong(em("NOTE"))),
                                                tags$div(tags$ul(
                                                  tags$li(em("Data on people identified as American Indian/Alaska Native, two or more races,
                                                          or other races/ethnicities not listed are not included in this table. ")),
                                                  tags$li(em("ADJ means age-adjusted rate of number of cases per 100,000 people by race/ethnicity group"))
                                                ))),
                                         box(width = 12,
                                             height=500,
                                             align="center",
                                             status = "primary",solidHeader = FALSE,
                                             includeHTML("by_race_dashboard.html"))
                                ))),
                # Xiaoli Sun sub2 ends--------------------------------------------------------------------------------------------

                # Yue Liang sub3 starts----------------------------------------------------------------------------------------

                    tabPanel("Wealth",icon = icon("dollar-sign"),
                      fluidPage(
                        fluidRow(column(8,
                                        h3("Covid-19 Stats by Poverty"),
                                        "Some hypothesis saying that high poverty would lead to a high death rate.
                                        Thus we plot the counts and rates of comfirmed cases, hospitalizations, and deaths
                                        grouping by different level of poverty in New York City",
                                        br(),
                                        tags$div(tags$ul(
                                          tags$li("You may select different tab items on the two plots, and compare the two plots."),
                                          tags$li("Rate means age-adjusted according to the US 2000 standard population per 100,000 people by ZCTA of residence. 
                                                  After our preprocessing, it is now shown as a percentage.")
                                        ))),
                                 column(3,
                                        br(),
                                        p(strong(em("NOTE"))),
                                        tags$div(tags$ul(
                                          tags$li(em("Low: <10% of residents in ZCTA living below the FPT ")),
                                          tags$li(em("Medium: 10% to <20%")),
                                          tags$li(em("High: 20% to <30%")),
                                          tags$li(em("Very high: ≥30% residents living below the FPT")),
                                          tags$li(em("ZCTA: ZIP Code Tabulation Area")),
                                          tags$li(em("FPT: Federal Poverty Threshold"))
                                        )))),
                          fluidRow(plotlyOutput("case_rate_poverty_plotly"))))
                ),

                # Yue Liang sub3 ends--------------------------------------------------------------------------------------------

            ### Xiaoli Sun & Yue Liang statistical analysis part ends-------------------------------------------------

            ### Xinying Feng Search Part begins ------------------------------------------------------------------------------------
                  tabPanel( "Test Sites",icon = icon("syringe"),
                      fluidPage(
                        img(src='StopTheSpread.png',
                            style="width: 100%; height: 300px;"),
                        fluidRow(column(12,
                                       mainPanel( width=12,
                                       h1(strong("Test Sites Near Me"), align = "center")),
                                       HTML("<hr width=80>"),
                                       p("Don't worry on the Covid-19 impact, we've got you covered!",align="center"),
                                        p("Select your borough and zipcode to find the nearest test center to you.",align="center"),
                                  DT::dataTableOutput('table'))))),
            ### Xinying Feng MAP Part ends ------------------------------------------------------------------------------------

            ### Yue Liang About starts--------------------------------------------------------------------------------------------------------
                tabPanel( "About",icon = icon("book-open"),
                    fluidPage(
                      img(src='Document.jpeg', 
                          style="width: 100%; height: 100%;"),
                      mainPanel( width=12,
                                 h1(strong("What you'll find here"), align = "center"),
                                 HTML("<hr width=400>"),
                                 p(" Everyone is worried about the Covid-19. So we build a Shiny App to provide daily updated data dashboard, 
                                     and the data stats visualization to help you understand the Covid-19 in New York City. You may also find 
                                     the comfirmed cases number in you borough and near your street through the map, which is helpful for you 
                                     to understand the Covid-19 in your neibourhood. After learning all the impact of Covid-19 in New York City,
                                     New Yorkers know how to protect ourselves, starting with get the Covid tests. You may find a Covid test 
                                     sites near you in the Test Sites tabs. ",
                                     style="margin-left:100px;
                                              margin-right:100px"),
                                 h4(strong("HOPE EVERYONE STAY HEALTHY!"),align="center"),
                                 h4(strong("HOPE THINGS WILL GET BACK TO NORMAL SOON!"),align="center")),
                      br(),
                      mainPanel( width=12,
                                 h1(strong("Contributors"), align = "center")),
                      HTML("<hr width=300>"),
                      column(3,align = "center",
                             img(src='0.jpg', 
                                 style="border-radius: 50%;
                                          border:3px solid #ddd;
                                          width: 100px;
                                          height: 100px;"),
                             h4("Yue Liang"),
                             p(strong("Master in Statistics at Columbia University")),
                             p("Contribute the User Interface, Home and About page design, several the Statistical Analysis polts to the App."),
                             fluidRow(uiOutput("LinkedIn_YL"))
                      ),
                      column(3,align = "center",
                             img(src='1.jpeg', 
                                 style="border-radius: 50%;
                                          border:3px solid #ddd;
                                          width: 100px;
                                          height: 100px;"),
                             h4("Xinying Feng"),
                             p(strong("Master in Acturial Science at Columbia University")),
                             p("Contribute the Map function and Search Box function to the App."),
                             fluidRow(uiOutput("LinkedIn_XYF"))
                             ),
                      column(3,align = "center",
                             img(src='2.jpeg', 
                                 style="border-radius: 50%;
                                          border:3px solid #ddd;
                                          width: 100px;
                                          height: 100px;"),
                             h4("Xiaoli Sun"),
                             p(strong("Master in Statistics at Columbia University")),
                             p("Contribute several Statistical Analysis plots and the World Cloud to the App."),
                             fluidRow(uiOutput("LinkedIn_XLS"))
                      ),
                      column(3,align = "center",
                             img(src='3.jpeg', 
                                 style="border-radius: 50%;
                                          border:3px solid #ddd;
                                          width: 100px;
                                          height: 100px;"),
                             h4("Hanyi Wang"),
                             p(strong("Research Assistant at Columbia University")),
                             p("The contributer of the App as a presenter."),
                             fluidRow(uiOutput("LinkedIn_HYW"))
                      ),
                      br(),
                      mainPanel( width=12,
                                 h1(strong("About Dataset"),align="center"),
                                 HTML("<hr width=300>"),
                                 p("The datasets we used are from ",
                                   a("NYC-Covid19",href="https://github.com/nychealth/coronavirus-data"),
                                   " and ",
                                   a("Test-Sites",href="http://nychealthhandhospitals.org/covid-19-testing-sites/"),
                                   ". ",align="center"),
                                 p("NYC-Covid19 dataset contains Coronavirus Disease 2019 (COVID-19) in New York City. 
                                   Data are updated daily.",align="center"),
                                 p("Test-Sites dataset contains the address of all available Covid-19 Testing Sites in
                                   New York City, which is updated on Oct 12, 2020."
                                   ,align="center"),
                                 h1(br()))
                                ))
            ### Yue Liang About ends--------------------------------------------------------------------------------------------------------

 ))