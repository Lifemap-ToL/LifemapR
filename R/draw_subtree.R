library(shiny)
library(leaflet)

draw_subtree <- function(df,basemap="ncbi"){
  
  map <- display_map(df,basemap) %>% 
    fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  for(row in 1:nrow(df)) {
    map <- map %>% addPolylines(lng=c(df[row,"lon"],df[df$taxid == df[row,]$father,"lon"]), lat=c(df[row,"lat"],df[df$taxid == df[row,]$father,"lat"]), color="red")
  }
  
  ui <- fluidPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("mymap", width = "100%", height = "1000px"),
    p(),
    absolutePanel(top = 10, right = 20,
                  textOutput("zoom"),
                  textOutput("boundaries"),
                  tags$head(tags$style("#param{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                  )
                  )
    ),
  )
  
  server <- function(input, output, session) {
    
    # define the zone visible by the users
    df_zoom_bounds <- reactive(
      df[df$zoom <= input$mymap_zoom &
           df$lat > input$mymap_bounds$south &
           df$lat < input$mymap_bounds$north &
           df$lon > input$mymap_bounds$west &
           df$lon < input$mymap_bounds$east,]
    )
    # output of the map
    output$mymap <- renderLeaflet({ map })
    
    # labels <- reactive({
    #   HTML(paste("<p>", data_input()$Electorate,"</p>",
    #              "<p>", "Number of Visitors: ", round(data_input()$user_id, digit = 0), "</p>",
    #              sep = ""))
    # })
    
    
    # modification of the map to display the rights markers
    observe({
      leafletProxy("mymap", session=session, data=df_zoom_bounds()) %>%
        clearMarkers() %>%
        addMarkers(lng=~lon, 
                   lat=~lat)
    })
    
    observeEvent(input$mymap_marker_mouseover,{
      req(input$mymap)
      leafletProxy("mymap", data=df_zoom_bounds) %>%
        addPopups(lng=~lon,lat=~lat, popup = ~sci_name)
    })
    
    output$zoom <- renderText(input$mymap_zoom)
    output$boundaries <- renderText(input$mymap_bounds$north)
    output$new_df <- renderDataTable(df_zoom_bounds())
    output$param <- renderDataTable(calcul_param())
    
  }
  
  shinyApp(ui, server)
}