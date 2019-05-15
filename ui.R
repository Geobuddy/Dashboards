#=========== Libraries ==============

library(shiny)
library(shinydashboard)
library(r2d3) #include D3 bar chart
library(leaflet) #add maps
library(readr) #read csv
library(plotly)
library(rsconnect)
library(dplyr)
library(tableHTML)

#=========== Data processing & cleanning ==============
#data <- read_csv("Chicago_Crimes_2012_to_2017.csv") #load dataset
#data <- na.omit(data) #remove rows with NULL/NA values
#data <- data[-c(1000:1418365),] #testing only part of the dataset
#data$Date <- as.Date(data$Date, format = "%m/%d/%Y ") #convert date to %y/%m/%d format
#=========== Dashboard ==============

navbarPage("Crime Dashboard",
           tabPanel("Map",
                    fluidPage(
                      fluidRow(
                        box(
                          selectInput(inputId = "offence", label = "Offence Group", choices = data$`Primary Type`, width = "100%"),
                          box(selectInput(inputId = "arrest", label = "Arrest", choices = data$Arrest, width = "100%")),
                          box(selectInput(inputId = "domestic", label = "Domestic", choices = data$Domestic, width = "100%"))
                        ),
                        box(
                          sliderInput(inputId = "rangeslider", label = "Choose date range:", value = c(min(data$Date) + 60, max(data$Date) - 60), min = min(data$Date), max = max(data$Date), width = "100%")),
                        column(3,
                                 box(title = h4(HTML("<center>","Crime Rate (per 1000 pop)","</center>")), width = "100%", background = "light-blue", status = "primary",
                                     h4(htmlOutput("txtout4")))),
                        column(3,
                                 box(title = h4(HTML("<center>","Crime count","</center>")), width = "100%", background = "light-blue", status = "primary",
                                     h4(htmlOutput("txtout3"))))
                      ),
                      fluidRow(
                        #Create map (Leaflet)
                        box(leafletOutput(outputId = "map", height = 750, width = "100%")),
                        #Create charts (Plotly.Js)
                        box(plotlyOutput(outputId = "plot1", height = 350, width = "100%")),
                        box(plotlyOutput(outputId = "plot2", height = 250, width = "100%")),
                        column(3,
                            box(width = "100%", background = "light-blue", status = "primary",
                                h5(htmlOutput("txtout1")))),
                        column(3,
                               box(width = "100%", background = "navy", status = "primary",
                                   h5(htmlOutput("txtout2"))))
                      ) 
                  ) 
           ),
           tabPanel("Data",
                    DT::dataTableOutput('summary')
                    ),
           tabPanel("About"),
           # zoom control behind the dropdown. source:https://stackoverflow.com/questions/49118331/leaflet-controls-overlay-navbarmenu-in-shiny
           tags$head(tags$style(".leaflet-top {z-index:999!important;}"))
)
