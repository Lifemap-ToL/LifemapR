library(shiny)
library(leaflet)

#' Draw the subtree of the ToL based on the taxids given
#'
#' @param lm_obj a lifemap object
#'
#' @return a shiny application
#' @export
#'
#' @examples draw_subtree(LM_df)
#' df <- read.csv("data/taxids_example.txt", row.names = 1)
#'
#' LM_df <- LifemapR::construct_dataframe(df)
#' draw_subtree(LM_df)
draw_subtree <- function(lm_obj){

  df <- lm_obj[[1]]
  basemap <- lm_obj[[2]]

  ui <- fluidPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("mymap", width = "100%", height = "1000px"),
    p()
  )

  server <- function(input, output, session) {

    # define the zone visible by the users
    df_zoom_bounds <- reactive(
      df[df$zoom <= (input$mymap_zoom+5) &
           df$lat > input$mymap_bounds$south &
           df$lat < input$mymap_bounds$north &
           df$lon > input$mymap_bounds$west &
           df$lon < input$mymap_bounds$east,]
      )

    # define the descendants of df_zoom_bounds taxids
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
        for (desc in df_descendants()[df_descendants()$ancestor == id,]$taxid) {
        proxy <- proxy %>%
          addPolylines(lng=c(df_zoom_bounds()[df_zoom_bounds()$taxid == id,"lon"],df_descendants()[df_descendants()$taxid == desc,"lon"]),
                       lat=c(df_zoom_bounds()[df_zoom_bounds()$taxid == id,"lat"],df_descendants()[df_descendants()$taxid == desc,"lat"]),
                       color="red")
        }
      }
      proxy
    })
  }

  shinyApp(ui, server)
}
