#' Draw the subtree of the ToL based on the taxids given
#'
#' @param lm_obj a lifemap.obj object
#' @param col the line's color
#' @param lwd the line's weight
#' @param ... any parameter that can be passed to the leaflet::addPolylines() function
#'
#' @return a shiny application
#' @export
#'
#' @importFrom shiny fluidPage reactive observe shinyApp
#' @importFrom leaflet leafletOutput renderLeaflet fitBounds leafletProxy addPolylines clearShapes providerTileOptions
#'
#' @examples draw_subtree(LM_df)
#' df <- read.csv("data/taxids_example.txt", row.names = 1)
#'
#' LM_df <- LifemapR::construct_dataframe(df)
#' draw_subtree(LM_df)
draw_subtree <- function(lm_obj, col="yellow", lwd=5,...){

  df <- lm_obj$df
  basemap <- lm_obj$basemap

  ui <- shiny::fluidPage(
    # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leaflet::leafletOutput("mymap", width = "100%", height = "1000px")
    # p()
  )

  server <- function(input, output, session) {

    # define the zone visible by the users
    df_zoom_bounds <- shiny::reactive(
      df[df$zoom <= (input$mymap_zoom+5) &
           df$lat > input$mymap_bounds$south &
           df$lat < input$mymap_bounds$north &
           df$lon > input$mymap_bounds$west &
           df$lon < input$mymap_bounds$east,]
      )

    # define the descendants of df_zoom_bounds taxids
    df_descendants <- shiny::reactive({
      visibles <- df_zoom_bounds()$taxid
      df[df$ancestor %in% visibles,]
    })

    # output of the map
    output$mymap <- leaflet::renderLeaflet({
      display_map(df,map = basemap) %>% leaflet::fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    })

    # modification of the map to display the right lines
    shiny::observe({
      proxy <- leaflet::leafletProxy("mymap", session=session) %>%
        leaflet::clearShapes()
      for (id in df_zoom_bounds()$taxid) {
        for (desc in df_descendants()[df_descendants()$ancestor == id,]$taxid) {
        proxy <- proxy %>%
          leaflet::addPolylines(lng=c(df_zoom_bounds()[df_zoom_bounds()$taxid == id,"lon"],df_descendants()[df_descendants()$taxid == desc,"lon"]),
                       lat=c(df_zoom_bounds()[df_zoom_bounds()$taxid == id,"lat"],df_descendants()[df_descendants()$taxid == desc,"lat"]),
                       color=col,
                       weight=lwd,
                       ...)
        }
      }
      proxy
    })
  }

  shiny::shinyApp(ui, server)
}

#ordonne les taxids pour pourvoir construire le format phylo
#' Prepare the datas for a transformation into a phylo format
#'
#' @param taxid the taxid of the descendant's ancestor we're looking at
#' @param df tha dataframe of a lifemap object
#' @param phylo the dtaframe to be created that contains sorted informations
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' phylo <- data.frame()
#'
#' phylo <- ord_tax(0,LM_df$df, phylo)
ord_tax <- function(taxid,df, phylo) {
  # cat("hey", taxid,"\n")
  phylo <- dplyr::bind_rows(phylo, df[which(df$taxid == taxid),])
  desc <- as.vector(df[which(df$ancestor == taxid),]$taxid)
  if (length(desc) == 1) {
    phylo <- ord_tax(desc[1],df, phylo)
  } else if(length(desc) >1) {
    for (element in desc) {
      phylo <- ord_tax(element,df, phylo)
    }
  }
  return(phylo)
}





