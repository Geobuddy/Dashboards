#=========== Libraries ==============

library(shiny)
library(shinydashboard)

#=========== Dashboard ==============

shinyServer(function(input,output){
  
  #Create the map
  output$map <- renderLeaflet({
    #if (length(input$variables) == 0) return(mtcars)
    data <- subset(data, Date >= input$rangeslider[1] & Date <= input$rangeslider[2] & Arrest == input$arrest &
                     Domestic == input$domestic & Primary.Type == input$offence)
    leaflet()%>%
      addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter")%>%
      addTiles(group = "osm")%>%
      addProviderTiles("CartoDB.DarkMatterNoLabels", group = "CartoDB.DarkMatterNoLabels")%>%
      addMarkers(data, lng = data$Longitude, lat = data$Latitude, group = "Markers") %>%
      #setView(lng = mean(data$Longitude), lat = mean(data$Latitude), zoom = 10)%>%
      addLayersControl(
        baseGroups = c("CartoDB.DdatarkMatter", "CartoDB.DarkMatterNoLabels","osm"),
        overlayGroups = "Markers",
        options = layersControlOptions(collapsed = TRUE))%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    
  })
  
  observeEvent(input$map_marker_click, {
    leafletProxy("map")
  })
  
  #Plot chart 
  output$plot1 <- renderPlotly({
    data <- subset(data, Date >= input$rangeslider[1] & Date <= input$rangeslider[2] & Arrest == input$arrest &
                     Domestic == input$domestic & Primary.Type == input$offence)
    data <- aggregate(data.frame(count = data$Primary.Type), list(value = data$Primary.Type), length)
    plot_ly(x = data[,2], y = reorder(data[,1],data[,2]), type = 'bar', orientation = 'h',space = 1)%>%
      add_annotations(xref = 'x1', yref = 'y',
                      x = data[,2] + 6,  y = data[,1],
                      text = paste(round(data[,2], 2)),
                      font = list(family = 'Arial', size = 12),
                      showarrow = FALSE)
  })

    #Plot chart 
  output$plot2 <- renderPlotly({
    data <- subset(data, Date >= input$rangeslider[1] & Date <= input$rangeslider[2] & Arrest == input$arrest &
                     Domestic == input$domestic & Primary.Type == input$offence)
    data <- aggregate(data.frame(count = data$Arrest), list(value = format.Date(data$Date,"%b %Y")), length)
    plot_ly(data, x = data[,1])%>%
      add_lines(y = data[,2], name = "Offences", line = list(width = 3))%>%
      add_lines(y = mean(data[,2]), name = "Average", line = list(color = 'grey'))%>%
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
  
  output$txtout3 <- renderText({
    HTML("<center>",
    paste("<b>",format(input$rangeslider[2], "%b %Y"), "</b>", "<br/>" , "compared to", "<br/>", "previous month:", "<br/>", "<br/>", "<b>", h4(format(input$rangeslider[2], "%b %Y"))), "<b/>","</center>"
    )
  })
  
  output$txtout4 <- renderText({
    HTML("<center>",
    paste("12 months to","<b>", format(input$rangeslider[2], "%b %Y"), "</b>", "<br/>", "compared to", "<br/>", " the previous 12 months:", "<br/>", "<br/>", "<b>", h4(format(input$rangeslider[2], "%b %Y"))), "<b/>","</center>"
    )
  })
  
  output$txtout1 <- renderText({
    data <- subset(data, Date >= input$rangeslider[1] & Date <= input$rangeslider[2] & Arrest == input$arrest &
                     Domestic == input$domestic & Primary.Type == input$offence)
    HTML("<center>",
         paste("<b>", format(input$rangeslider[2], "%b %Y"), "</b>"),"</center>"
    )
  })
  
  output$txtout2 <- renderText({
    data <- subset(data, Date >= input$rangeslider[1] & Date <= input$rangeslider[2] & Arrest == input$arrest &
                     Domestic == input$domestic & Primary.Type == input$offence)
    data <- aggregate(data.frame(count = data$Primary.Type), list(value = data$Primary.Type), length)
    HTML("<center>",
         paste("<b>", sum(data[,2]), "</b>"),"</center>"
    )
  })
  
})
