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
                   "leaflet", "shinyWidgets","plotly","shinythemes","reshape2")

# check packages that need to be installed.
packages.needed <- setdiff(packages.used, 
                           intersect(installed.packages()[,1], 
                                     packages.used))
# install additional packages
if(length(packages.needed) > 0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(shiny)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)
library(reshape2)

# bootstrapPage(
#   tags$head(includeHTML("www/gtag.html")),
#   navbarPage(theme = shinytheme("flatly"),
#              collapsible = TRUE,
#              "NYC COVID-19",
#              id="nav",
# 
#            ### Home --------------------------------------------------------------------------------------------------------
#              tabPanel("Home",icon = icon("home"),
#                   ## Yue Liang Summary part -----------------------------------------------------------------------------------
#                   
# 
#                   fluidRow(
#                   valueBoxOutput("total_case"),
#                   valueBoxOutput("death_rate"),
#                   valueBoxOutput("date")
#                   ),
# 
#                   box(width = "100%",height=300,includeHTML("wordcloud.html"))
#                   
#                   ## Xiaoli Sun Word Cloud part ------------------------------------------------------------------------------
#                     # column(width=8,
#                     #        box(width = 12,title = "Word Cloud of Covid-19",status = "primary",solidHeader = TRUE,
#                     #            includeHTML("wordcloud.html")),
#                     #        offset = 1
#                     # )
#                   ),
# 
#                   ## Hanyi Wang Home part -----------------------------------------------------------------------------------
#             ### home end --------------------------------------------------------------------------------------------------------
# 
#             ### Xinying Feng MAP Part Begin ------------------------------------------------------------------------------------
# 
#              tabPanel("MAP",icon = icon("map-marked-alt"),
#                   div(class="outer",
#                      tags$head(includeCSS("www/styles.css")),
#                       leafletOutput("map",width="100%",height="100%"),
#                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
#                                     top = 160, left = 20, right = "auto", bottom = "auto", width = 250, height = "auto",
# 
#                                     span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
#                                     h3(textOutput("reactive_case_count"), align = "right"),
#                                     h4(textOutput("reactive_death_count"), align = "right"),
# 
#                                     checkboxGroupInput("Borough", "Choose Borough:",
#                                                        choices = c("Manhattan", "Staten Island",
#                                                                    "Bronx", "Queens",
#                                                                    "Brooklyn"),
#                                                        selected = c("Manhattan", "Staten Island",
#                                                                     "Bronx", "Queens",
#                                                                     "Brooklyn")
#                                     ),
#                                     radioButtons("Category", "Choose category:",
#                                                  choices = c("COVID_CASE_COUNT","COVID_DEATH_COUNT", "TOTAL_COVID_TESTS", "PERCENT_POSITIVE"))
#                                     )
#                      )
#                   ),
# 
#           ### Xinying Feng MAP Part End -------------------------------------------------------------
# 
#           ### Xiaoli Sun statistical analysis part begin-------------------------------------------------
# 
#           tabPanel("Time Series",icon = icon("chart-line"),
#               fluidPage(
#                   fluidRow(column(12,
#                                 h3("Interactive Dashboard"),
#                                 "By default, the bar chart shows the sum of segments by year as the height of each bar, and pie chart shows the percentage of total crime shootings in each borough.",
#                                 tags$div(tags$ul(
#                                   tags$li("Hover the mouse over a year bar in histogram will modify the pie chart and legend."),
#                                   tags$li("Hover the mouse over pie slice should change the histogram.")
#                                 ))),
#                          box(width = 12,height = 500,status = "primary",solidHeader = FALSE,
#                              includeHTML("index.html"))
#                          ))),
# 
#           navbarMenu("Analysis", icon = icon("chart-area"),
# 
#                 #sub1 begin---------------------------------------------------------------------------------------
#                      tabPanel("Borough",icon = icon("location-arrow"),
#                               fluidPage(
#                                 fluidRow(column(12,
#                                                 h3("Interactive Dashboard"),
#                                                 "By default, the bar chart shows the sum of segments by year as the height of each bar, and pie chart shows the percentage of total crime shootings in each borough.",
#                                                 tags$div(tags$ul(
#                                                   tags$li("Hover the mouse over a year bar in histogram will modify the pie chart and legend."),
#                                                   tags$li("Hover the mouse over pie slice should change the histogram.")
#                                                 ))),
# 
#                                          box(width=6,
#                                              height=300,
#                                              status = "primary",solidHeader = FALSE,
#                                              includeHTML("by_boro1.html")),
#                                          hr(),
#                                          box(width=6,
#                                              height=300,
#                                              status = "primary",solidHeader = FALSE,
#                                              includeHTML("by_boro2.html"),
#                                              offset=1)
#                                 ))),
#                 #sub1 end---------------------------------------------------------------------------------------
# 
#                 #sub2 begin----------------------------------------------------------------------------------------
# 
#                      tabPanel("Race",icon = icon("user-friends"),
#                               fluidPage(
#                                 fluidRow(column(12,
#                                                 h3("Interactive Dashboard"),
#                                                 "By default, the bar chart shows the sum of segments by year as the height of each bar, and pie chart shows the percentage of total crime shootings in each borough.",
#                                                 tags$div(tags$ul(
#                                                   tags$li("Hover the mouse over a year bar in histogram will modify the pie chart and legend."),
#                                                   tags$li("Hover the mouse over pie slice should change the histogram.")
#                                                 ))),
#                                          box(width = 12,
#                                              height=500,
#                                              status = "primary",solidHeader = FALSE,
#                                              includeHTML("by_race_dashboard.html"))
# 
#                                 ))),
#                 #sub2 end--------------------------------------------------------------------------------------------
# 
#                 #sub3 begin----------------------------------------------------------------------------------------
# 
#                     tabPanel("Wealth",icon = icon("dollar-sign"),
#                       fluidPage(
#                         fluidRow(column(12,
#                                         h3("Interactive Dashboard"),
#                                         "By default, the bar chart shows the sum of segments by year as the height of each bar, and pie chart shows the percentage of total crime shootings in each borough.",
#                                         tags$div(tags$ul(
#                                           tags$li("Hover the mouse over a year bar in histogram will modify the pie chart and legend."),
#                                           tags$li("Hover the mouse over pie slice should change the histogram.")
#                                         )))
#                                  # ,
#                                  # box(width = 12, height = 500,
#                                  #     status = "primary",solidHeader = FALSE,
#                                  #        includeHTML("by-poverty.html"))
#                         )))),
# 
#                 #sub3 end--------------------------------------------------------------------------------------------
# 
#             ## Hanyi Wang About --------------------------------------------------------------------------------------------------------
#                 tabPanel( "About",icon = icon("book-open"),
#                     fluidPage(
#                       mainPanel( width=12,
#                                  h1(strong("What you'll find here"),align = "center")
#                                 )))
#             ## Hanyi About end --------------------------------------------------------------------------------------------------------
# 
#  ))
# 
#                       
dashboardPage(
  skin = "blue",
  dashboardHeader(title = "NYC COVID-19"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("presentation")),
    menuItem("MAP", tabName = "MAP", icon = icon("map-marked-alt")),
    menuItem("TimeSeries", tabName = "TimeSeries", icon = icon("chart-line")),
    menuItem("Analysis", tabName = "Analysis", icon = icon("chart-area"),startExpanded = TRUE,
             menuSubItem("Borough",tabName = "Borough", icon = icon("location-arrow")),
             menuSubItem("Race",tabName = "Race",icon = icon("user-friends")),
             menuSubItem("Wealth",tabName = "Wealth",icon = icon("dollar-sign"))
    ),
    menuItem("About", tabName = "About", icon = icon("book-open"))
  )),

  dashboardBody(fill = FALSE,tabItems(

    ## Han yi Wang About --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "About",
            fluidPage(
              mainPanel( width=12,
                         h1(strong("What you'll find here"),align = "center")
                         )
                         )),
    ## Han yi About end --------------------------------------------------------------------------------------------------------

    # Home --------------------------------------------------------------------------------------------------------
    tabItem(tabName = "Home",
            ## Yue Liang Summary part -----------------------------------------------------------------------------------
            fluidRow(
              valueBoxOutput("total_case"),
              valueBoxOutput("death_rate"),
              valueBoxOutput("date")
            ),

            ## Xiaoli Sun Word Cloud part ------------------------------------------------------------------------------
            fluidRow(box(width = 12,title = "Word Cloud of Covid-19",status = "primary",solidHeader = TRUE,
                         includeHTML("wordcloud.html")
            )
            )),

            ## Hanyi Wang Home part -----------------------------------------------------------------------------------
    # home end --------------------------------------------------------------------------------------------------------

    # MAP
    # Xinying Feng MAP Part Begin ------------------------------------------------------------------------------------
    tabItem(tabName = "MAP",
            fluidPage(
              leafletOutput("map",width="100%",height=600),
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                            top = 200, left = 310, right = "auto", bottom = "auto", width = 250, height = "auto",

                  checkboxGroupInput("Borough", "Choose Borough:",
                                 choices = c("Manhattan", "Staten Island",
                                             "Bronx", "Queens",
                                             "Brooklyn"),
                                 selected = c("Manhattan", "Staten Island",
                                              "Bronx", "Queens",
                                              "Brooklyn")
                                    ),
                  radioButtons("Category", "Choose category:",
                           choices = c("COVID_CASE_COUNT","COVID_DEATH_COUNT", "TOTAL_COVID_TESTS", "PERCENT_POSITIVE"))
              )
            )),

    ### Xinying Feng MAP Part End -------------------------------------------------------------

    ### Xiaoli Sun statistical analysis part begin-------------------------------------------------
    tabItem(tabName = "TimeSeries",
              fluidPage(
                          fluidRow(column(12,
                                        h3("Interactive Dashboard"),
                                        "By default, the bar chart shows the sum of segments by year as the height of each bar, and pie chart shows the percentage of total crime shootings in each borough.",
                                        tags$div(tags$ul(
                                          tags$li("Hover the mouse over a year bar in histogram will modify the pie chart and legend."),
                                          tags$li("Hover the mouse over pie slice should change the histogram.")
                                        ))),
                                 box(width = 12,height = 500,status = "primary",solidHeader = FALSE,
                                     includeHTML("index.html"))
                                 ))),

    #sub1 begin---------------------------------------------------------------------------------------

    tabItem(tabName = "Borough",
                      fluidPage(
                        fluidRow(column(12,
                                        h3("Interactive Dashboard"),
                                        "By default, the bar chart shows the sum of segments by year as the height of each bar, and pie chart shows the percentage of total crime shootings in each borough.",
                                        tags$div(tags$ul(
                                          tags$li("Hover the mouse over a year bar in histogram will modify the pie chart and legend."),
                                          tags$li("Hover the mouse over pie slice should change the histogram.")
                                        ))),

                                        box(width=6,
                                            height=300,
                                            status = "primary",solidHeader = FALSE,
                                            includeHTML("by_boro1.html")),
                                        hr(),
                                        box(width=6,
                                            height=300,
                                            status = "primary",solidHeader = FALSE,
                                            includeHTML("by_boro2.html"),
                                            offset=1)
))),

    #sub1 end---------------------------------------------------------------------------------------

    #sub2 begin----------------------------------------------------------------------------------------

            tabItem(tabName = "Race",
                      fluidPage(
                        fluidRow(column(12,
                                        h3("Interactive Dashboard"),
                                        "By default, the bar chart shows the sum of segments by year as the height of each bar, and pie chart shows the percentage of total crime shootings in each borough.",
                                        tags$div(tags$ul(
                                          tags$li("Hover the mouse over a year bar in histogram will modify the pie chart and legend."),
                                          tags$li("Hover the mouse over pie slice should change the histogram.")
                                        ))),
                                 box(width = 12,
                                     height=500,
                                     status = "primary",solidHeader = FALSE,
                                     includeHTML("by_race_dashboard.html"))

                        ))),

    #sub2 end--------------------------------------------------------------------------------------------

    #sub3 begin----------------------------------------------------------------------------------------

            tabItem(tabName = "Wealth",
                      fluidPage(
                        fluidRow(column(12,
                                        h3("Interactive Dashboard"),
                                        "By default, the bar chart shows the sum of segments by year as the height of each bar, and pie chart shows the percentage of total crime shootings in each borough.",
                                        tags$div(tags$ul(
                                          tags$li("Hover the mouse over a year bar in histogram will modify the pie chart and legend."),
                                          tags$li("Hover the mouse over pie slice should change the histogram.")
                                        )))
                                 ,
                                 box(width = 12, height = 500,
                                     status = "primary",solidHeader = FALSE,
                                        includeHTML("by-poverty.html"))
                        )))

  #sub3 end--------------------------------------------------------------------------------------------

 )))