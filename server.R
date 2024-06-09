library(shiny)
library(DT)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(visNetwork)

# Load the data
data <- read.csv('data/demographics.csv', header = TRUE, skip = 14)

summary_info <- read.csv('data/footnotes.csv')

# Clean and rename columns
data <- rename(data, 
               Origin.Country = Country.of.origin, 
               Origin.Country.ISO = Country.of.origin..ISO., 
               Asylum.Country = Country.of.asylum,
               Asylum.Country.ISO = Country.of.asylum..ISO.)

# Select relevant columns
total_origin_data <- select(data, Year, Origin.Country, Origin.Country.ISO, Asylum.Country, Asylum.Country.ISO, Total)

# Load world shapefile data using rnaturalearth
world_sf <- ne_countries(scale = "medium", returnclass = "sf")


shinyServer(function(input, output) {
    filtered_data <- reactive({
        if(input$filterType == "origin") {
            total_origin_data %>%
                filter(Year == input$data_year) %>%
                group_by(Origin.Country.ISO, Origin.Country) %>%
                summarize(totalPopulation = sum(Total)) %>%
                rename(ISO = Origin.Country.ISO, CountryName = Origin.Country) %>%
                arrange(desc(totalPopulation)) %>%
                ungroup()
        }
        else {
            total_origin_data %>%
                filter(Year == input$data_year) %>%
                group_by(Asylum.Country.ISO, Asylum.Country) %>%
                summarize(totalPopulation = sum(Total)) %>%
                rename(ISO = Asylum.Country.ISO, CountryName = Asylum.Country) %>%
                arrange(desc(totalPopulation)) %>%
                ungroup()
        }
        
    })
    
    output$title <- renderText({
        if (input$filterType == "origin") {
            paste("Population of Refugees (Based on Origin Country) For Year ", input$data_year)
        } else {
            paste("Population of Refugees (Based on Aslyum Country) For Year", input$data_year)
        }
    })
    
    output$populationList <- renderDataTable({
        
        DT::datatable(data = filtered_data(),
                      options = list(pageLength = 10),
                      rownames = FALSE)
    })
    
    output$map <- renderLeaflet({
        year_data <- filtered_data()
        world_sf$POP2005 <- world_sf$wb_a3 %>% 
            lapply(function(country) {
                total_population <- year_data$totalPopulation[year_data$ISO == country]
                if(length(total_population) > 0) {
                    return(total_population)
                } else {
                    return(NA)
                }
            }) %>% 
            unlist()
        
        mybins <- c(0, 10000, 50000, 100000, 500000, 1000000, Inf)
        ##if(input$filterType == "origin") {
            mypalette <- colorBin(
                palette = "YlOrBr", domain = world_sf$POP2005,
                na.color = "transparent", bins = mybins
            )
       ## }
        ##else {
         ##   mypalette <- colorBin(
          ##      palette = "BuGn", domain = world_sf$POP2005,
          ##      na.color = "transparent", bins = mybins
          ##  )
       ## }
        
        
        mytext <- paste(
            "Country: ", world_sf$name, "<br/>",
            "Population: ", round(world_sf$POP2005, 2),
            sep = ""
        ) %>% 
            lapply(htmltools::HTML)
        
        leaflet(world_sf) %>%
            addTiles() %>%
            setView(lat = 10, lng = 0, zoom = 1.5) %>%
            addPolygons(
                fillColor = ~mypalette(POP2005),
                stroke = TRUE,
                fillOpacity = 0.9,
                color = "white",
                weight = 0.3,
                label = mytext,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"
                )
            ) %>%
            addLegend(
                pal = mypalette, values = ~POP2005, opacity = 0.9,
                title = "Population (M)", position = "bottomleft"
            )
    })
    
    output$linePlot <- renderPlot({
        data <- total_origin_data %>%
            group_by(Year) %>%
            summarize(totalPopulation = sum(Total)) %>%
            ungroup()
        
        ggplot(data, aes(x = Year, y = totalPopulation)) +
            geom_line() +
            theme_minimal() +
            labs(title = "Population Trend For All Countries Over Years")
    })
    
  
    
   
    output$linePlotComparison <- renderPlot({
        data <- total_origin_data %>%
            filter(Origin.Country %in% input$origin_country_plot) %>%
            group_by(Origin.Country,Year) %>%
            summarize(totalPopulation = sum(Total)) %>%
            ungroup()
        
        ggplot(data, aes(x = Year, y = totalPopulation, color = Origin.Country)) +
            geom_line() +
            theme_minimal() +
            labs(title = "Population Trend For All Countries Over Years")
    })
    
    
    
    network_data <- reactive({
        total_origin_data %>%
        filter(Origin.Country == input$origin_country) %>%
        group_by(Origin.Country, Asylum.Country) %>%
        summarize(totalPopulation = sum(Total)) %>%
        arrange(desc(totalPopulation))  %>%
        ungroup()
      
    })
    
    output$populationList2 <- renderDataTable({
      
        DT::datatable(data = network_data(),
                      options = list(pageLength = 10),
                      colnames = c("Origin Country","Destnation Country", "Total Refugee Population"))
    })
    
    output$network <- renderVisNetwork({
        set.seed(100)
        data <- network_data() 
        # Print the data to check its structure
        print(data)
        
        if(nrow(data) == 0) {
            return(NULL)
        }
        # Ensure there are no NAs in the data
        data <- na.omit(data)
        
        nodes <- data.frame(id = unique(c(data$Origin.Country, data$Asylum.Country)))
        edges <- data.frame(
            from = data$Origin.Country,
            to = data$Asylum.Country,
            value = data$totalPopulation
        )
        
        visNetwork(nodes, edges) %>%
            visNodes(color = list(background = "lightblue", 
                                  border = "darkblue"),shape = "dot", size = 7) %>%
            visEdges(arrows = "to", selfReferenceSize = 2 , dashes =TRUE) %>%
            visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T)) 
            
        
        
    })
    
    output$summary <- renderDataTable({
        
        DT::datatable(data = summary_info,
                      options = list(pageLength = 10),
                      rownames = FALSE)
    })
   
})
