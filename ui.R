library(shiny)
library(DT)
library(markdown)
library(visNetwork)

shinyUI(navbarPage("Refugees Population",
                   tabPanel("Map",
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput(inputId = "data_year",
                                                label = "Choose the year you want to summarize:",
                                                choices = unique(total_origin_data$Year)),
                                    radioButtons(inputId = "filterType", 
                                                 label = "Filter by:", 
                                                 choices = c("Origin Country" = "origin", "Aslyum Country" = "Aslyum")),
                                   
                                ),
                                mainPanel(
                                    h3(textOutput("title")),
                                    DT::dataTableOutput(outputId = "populationList"),
                                    leafletOutput("map")
                                )
                            )
                   ),
                   tabPanel("Plot",
                        plotOutput("linePlot"),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(inputId = "origin_country_plot", 
                                            label = "Filter by:", 
                                            choices = sort(unique(total_origin_data$Origin.Country))
                                            ,
                                            multiple = TRUE),
                                width = 2,
                            ),
                            mainPanel(
                                plotOutput("linePlotComparison"),
                            )
                        )
                        
                        
                   ),
                   tabPanel("Network",
                            sidebarPanel(
                                selectInput(inputId = "origin_country",
                                            label = "Choose the country:",
                                            choices = sort(unique(total_origin_data$Origin.Country))),

                            ),
                            mainPanel(
                                visNetworkOutput("network", width = "100%", height = "750px"),
                                DT::dataTableOutput(outputId = "populationList2")
                            )
                            
                   ),
                   tabPanel("Summary",
                            DT::dataTableOutput(outputId = "summary")
                   )
                   
))
