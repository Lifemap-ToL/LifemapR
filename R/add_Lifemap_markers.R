#' Passes information from the leaves to the nodes
#'
#' @param df a dataframe from a lifemap_object
#' @param information the variable by wich the data needs to be represented
#' @param my_function the function to be applied fom the representation
#'
#' @return a dataframe containing for each node the value for one variable according to a function
#'
#' @examples pass_infos (df, "GC_content", sum)
pass_infos <- function(df, information, my_function) {
  all_ancestors <- unique(unlist(df[df$type=="requested",]$ascend))
  tax_request <- as.vector(df[df$type=="requested",]$taxid)
  only_leaves <- setdiff(tax_request,all_ancestors)

  info <- data.frame(ancestors=all_ancestors)
  for (id in only_leaves) {
    info_tmp <- data.frame(unlist(df[df$taxid==id,"ascend"]),df[df$taxid==id,information])
    colnames(info_tmp) <- c("ancestors", as.character(id))

    info <- left_join(info,info_tmp, by="ancestors")
  }

  res <- apply(info,MARGIN=1,function(x){
    tmp <- x[!is.na(x)]
    my_function(tmp[-1])
  })

  final_res <- data.frame(info$ancestors)
  final_res$information <- res
  colnames(final_res) <- c("ancestors","value")

  return(final_res)

}


#' Represent continuous datas on a Lifemap background
#'
#' @param lm_obj a Lifemap object
#' @param information the variable used to represent the datas
#' @param my_function the function to be applied on the variable
#'
#' @return a shiny application
#' @export
#'
#' @examples
#' df <- read.csv("data/taxids_example.txt", row.names = 1)
#' df <- distinct(df)
#' info1 <- runif(n=950, min=1, max=200)
#' df$info1 <- info1
#' LM_df <- construct_dataframe(df)
#' add_Lifemap_markers(LM_df, "info1", my_function = sum)
add_Lifemap_markers <- function(lm_obj, information, my_function){

  df <- lm_obj[[1]]
  basemap <- lm_obj[[2]]

  new_df <- pass_infos(df,information=information, my_function=my_function)
  for (id in 1:nrow(new_df)) {
    df[df$taxid==new_df[id, "ancestors"], information] <- new_df[id,]$value
  }

  ui <- fluidPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("mymap", width = "100%", height = "1000px"),
    p()
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
    output$mymap <- renderLeaflet({
      display_map(df,map = basemap) %>% fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    })

    # modification of the map to display the rights markers
    observe({
        leafletProxy("mymap", session=session, data = df_zoom_bounds()) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addCircleMarkers(lng=~lon, lat=~lat, radius=~(info1/1000), fillColor = "red")

    })

  }

  shinyApp(ui, server)
}
