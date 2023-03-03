library(shiny)
library(leaflet)

draw_subtree <- function(lm){

  df <- lm[[1]]
  basemap <- lm[[2]]

  ui <- fluidPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("mymap", width = "100%", height = "1000px"),
    p(),
    dataTableOutput("new_df"),
    dataTableOutput("new_df2")
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

    df_descendants <- reactive({
      visibles <- df_zoom_bounds()$taxid
      df[df$ancestor %in% visibles,]
    })

    # output of the map
    output$mymap <- renderLeaflet({
      display_map(df,map = basemap) %>% fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    })

    # modification of the map to display the rights markers
    observe({
      proxy <- leafletProxy("mymap", session=session) %>%
        clearShapes()
      for (id in df_zoom_bounds()$taxid) {
        print(df_zoom_bounds()[df_zoom_bounds()$taxid == id,])
        print("yo")
        print(df_descendants()[df_descendants()$ancestor == id,])
        for (desc in df_descendants()[df_descendants()$ancestor == id,]$taxid) {
        proxy <- proxy %>%
          addPolylines(lng=c(df_zoom_bounds()[df_zoom_bounds()$taxid == id,"lon"],df_descendants()[df_descendants()$taxid == desc,"lon"]),
                       lat=c(df_zoom_bounds()[df_zoom_bounds()$taxid == id,"lat"],df_descendants()[df_descendants()$taxid == desc,"lat"]),
                       color="red")
        }
      }
      proxy
    })

    output$new_df <- renderDataTable(df_zoom_bounds())
    output$new_df2 <- renderDataTable(df_descendants())

  }

  shinyApp(ui, server)
}
