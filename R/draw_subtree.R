#' Draw the subtree of the ToL based on the taxids given
#'
#' @param lm_obj a lifemap.obj object
#' @param col the line's color
#' @param FUN the function to be applied when visualising data on branches
#' @param pal the palette used to display information
#' @param ... any parameter that can be passed to the leaflet::addPolylines() function
#'
#' @return a shiny application
#' @export
#'
#' @importFrom shiny fluidPage reactive observe shinyApp
#' @importFrom leaflet leafletOutput renderLeaflet fitBounds leafletProxy addPolylines clearShapes providerTileOptions addLegend
#'
#' @examples draw_subtree(LM_df)
#' df <- read.csv("data/taxids_example.txt", row.names = 1)
#' LM_df <- LifemapR::construct_dataframe(df)
#'
#' # to simply view the subtree
#' draw_subtree(LM_df)
#'
#' # to color branches according to a variable
#' draw_subtree(LM_df, col="GC.")
draw_subtree <- function(lm_obj, col = "yellow", FUN = "mean", pal = "Accent",legend = TRUE, ...){

  df <- lm_obj$df
  basemap <- lm_obj$basemap

  # pass the info if a variable needs to be represented
  if (col %in% colnames(df)) {
    new_df <- pass_infos(df, information = col, my_function = FUN)
    for (id in 1:nrow(new_df)) {
      df[df$taxid == new_df[id, "ancestors"], col] <- new_df[id, ]$value
    }
  }

  ui <- shiny::fluidPage(
    shiny::tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leaflet::leafletOutput("mymap", width = "100%", height = "1000px"),
    shiny::p()
  )

  server <- function(input, output, session) {

    # define the taxa visible by the users
    df_zoom_bounds <- shiny::reactive(
      df[df$zoom <= (input$mymap_zoom+5) &
           df$lat > input$mymap_bounds$south &
           df$lat < input$mymap_bounds$north &
           df$lon > input$mymap_bounds$west &
           df$lon < input$mymap_bounds$east, ]
      )

    # define the descendants of df_zoom_bounds' taxids
    df_descendants <- shiny::reactive({
      visibles <- df_zoom_bounds()$taxid
      df[df$ancestor %in% visibles,]
    })

    # output of the map
    output$mymap <- leaflet::renderLeaflet({
      display_map(df,map = basemap) %>% leaflet::fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    })

    # modification of the map to draw the right branches
    shiny::observe({
      # clearing all the already existing shapes/controls
      proxy <- leaflet::leafletProxy("mymap", session=session) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls()

      if (col %in% colnames(df)) {
        make_col <- leaflet::colorNumeric(palette = pal, domain = df[[col]])
      }

      for (id in df_zoom_bounds()$taxid) {
        # for each descendant of each taxid
        for (desc in df_descendants()[df_descendants()$ancestor == id, ]$taxid) {

          if (col %in% colnames(df)) {
            col_info <- make_col(df_descendants()[df_descendants()$taxid == desc, col])
          } else { col_info <- col}

          proxy <- proxy %>%
          leaflet::addPolylines(lng = c(df_zoom_bounds()[df_zoom_bounds()$taxid == id, "lon"],
                                        df_descendants()[df_descendants()$taxid == desc, "lon"]),
                                lat = c(df_zoom_bounds()[df_zoom_bounds()$taxid == id, "lat"],
                                        df_descendants()[df_descendants()$taxid == desc, "lat"]),
                                color = col_info,
                                ...)
          }
      }
      # adding legend if necessary
      if(legend == TRUE && col %in% colnames(df)) {
        proxy <- proxy %>%
          leaflet::addLegend(position = "bottomright",
                             title = col,
                             pal = make_col,
                             values = df_descendants()[[col]])
      }

      proxy
    })
  }

  shiny::shinyApp(ui, server)
}

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





