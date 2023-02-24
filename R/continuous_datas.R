library(shiny)
library(leaflet)

continuous_datas <- function(df,basemap){

  r_colors <- rgb(t(col2rgb(colors()) / 255))
  names(r_colors) <- colors()

  ui <- fluidPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("mymap", width = "100%", height = "1000px"),
    p(),
    absolutePanel(top = 10, right = 20,
                  textOutput("zoom"),
                  textOutput("boundaries"),
                  tags$head(tags$style("#zoom{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                  )
                  )
    ),
    actionButton("button", "Clear Markers"),
    dataTableOutput("new_df")

  )

  server <- function(input, output, session) {

    points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)

    df_zoom_bounds <- reactive(
      df[df$zoom <= input$mymap_zoom & df$lat > input$mymap_bounds$south & df$lat < input$mymap_bounds$north & df$lon > input$mymap_bounds$west & df$lon < input$mymap_bounds$east,]
    )

    output$mymap <- renderLeaflet({
      display_map(df,basemap) %>% fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    })

    observeEvent(
      eventExpr = input$mymap_zoom,{
      leafletProxy("mymap", session=session) %>%
        clearMarkers() %>%
        addMarkers(lng=df_zoom_bounds()$lon, lat=df_zoom_bounds()$lat,
        )
    })

    output$zoom <- renderText(input$mymap_zoom)
    output$boundaries <- renderText(input$mymap_bounds$north)
    output$new_df <- renderDataTable(df_zoom_bounds())

    observeEvent(input$button,{
      leafletProxy("mymap") %>%
        clearMarkers()
    })
  }

  shinyApp(ui, server)
}

adding_markers(df,"ncbi")
