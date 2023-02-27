library(shiny)
library(leaflet)

continuous_datas <- function(df,basemap="ncbi",by_col=NULL,param="size"){

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

    dataTableOutput("param"),
    dataTableOutput("new_df")
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

    # calcul_param <- reactive({
    #   ids_zoom <- df_zoom_bounds()$taxid
    #   for (taxa in 1:nrow(df)){
    #     if ()
    #   }
    #   df[ids_zoom %in% df$ascend,]
    # })

    # output of the map
    output$mymap <- renderLeaflet({
      display_map(df,basemap) %>% fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    })

    #modification of the map to display the rights markers
    observe({
      leafletProxy("mymap", session=session) %>%
        clearMarkers() %>%
        addMarkers(lng=df_zoom_bounds()$lon, lat=df_zoom_bounds()$lat,
        )
    })

    output$zoom <- renderText(input$mymap_zoom)
    output$boundaries <- renderText(input$mymap_bounds$north)
    output$new_df <- renderDataTable(df_zoom_bounds())
    output$param <- renderDataTable(calcul_param())

  }

  shinyApp(ui, server)
}
