#=========== Libraries ==============

library(shiny)
library(shinydashboard)

#=========== Dashboard ==============

shinyServer(function(input,output){
  
  #Create the map
  output$map <- renderLeaflet({
    leaflet()%>%
      addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
      addTiles(group = "osm")%>%
      addProviderTiles("CartoDB.DarkMatterNoLabels", group = "CartoDB.DarkMatterNoLabels")%>%
      #setView(lng = mean(data$Longitude), lat = mean(data$Latitude), zoom = 10)%>%
      addMarkers(lng = data$Longitude, lat = data$Latitude,
                 clusterOptions = markerClusterOptions(),group = "Markers")%>%
      addLayersControl(
        baseGroups = c("CartoDB.DarkMatter", "CartoDB.DarkMatterNoLabels","osm"),
        overlayGroups = "Markers",
        options = layersControlOptions(collapsed = TRUE))
      
  })
  
  #Plot chart 
  output$plot1 <- renderPlotly({
    stats <- aggregate(data.frame(count = data$`Primary Type`), list(value = data$`Primary Type`), length)
    plot_ly(x = stats[,2], y = reorder(stats[,1],stats[,2]), type = 'bar', orientation = 'h')%>%
      add_annotations(xref = 'x1', yref = 'y',
                      x = stats[,2] + 6,  y = stats[,1],
                      text = paste(round(stats[,2], 2)),
                      font = list(family = 'Arial', size = 12),
                      showarrow = FALSE)
  })
  
  #Plot chart 
  output$plot2 <- renderPlotly({
    stats1 <- data[data$Arrest == "True", ]
    stats1$Date <- format(stats1$Date, "%m/%Y")
    stats1 <- aggregate(data.frame(count = stats1$Arrest), list(value = stats1$Date), length)
    plot_ly(stats1, x = stats1[,1])%>%
      add_lines(y = stats1[,2])%>%
      layout(xaxis = list(showgrid = F), yaxis = list(ticks='', showticklabels= F))
  })
  
  
  # code sourced from:https://shiny.rstudio.com/gallery/datatables-options.html
  output$summary <- DT::renderDataTable(DT::datatable(
    data,
    options = list(rowCallback = DT::JS(
      'function(row, data) {
        // Bold cells for those >= 5 in the first column
        if (parseFloat(data[1]) >= 5.0)
          $("td:eq(1)", row).css("font-weight", "bold");
      }'
    ))
  ))
  
  output$txtout1 <- renderText({
    HTML("<center>",
    paste("<b>",input$rangeslider[2], "</b>", "<br/>" , "compared to", "<br/>", "previous month:"),"</center>"
    )
  })
  
  output$txtout2 <- renderText({
    HTML("<center>",
    paste("12 months to","<b>", input$rangeslider[2], "</b>", "<br/>", "compared to", "<br/>", " the previous 12 months:"),"</center>"
    )
  })
  
})
