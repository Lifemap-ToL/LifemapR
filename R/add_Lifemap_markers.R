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

  # compute the value for each row
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
#' @param by the way the variable needs to be represented ("size", or "color")
#' @param pass_info to pass the information to the nodes or not (either TRUE or FALSE)
#' (note that not to pass the informations may cause a lot of markers to be displayed)
#'
#' @return a shiny application
#' @export
#' @importFrom dplyr left_join
#'
#' @examples
#' df <- read.csv("data/taxids_example.txt", row.names = 1)
#' df <- distinct(df)
#' info1 <- runif(n=950, min=1, max=200)
#' df$info1 <- info1
#' LM_df <- construct_dataframe(df)
#' add_Lifemap_markers(LM_df, "info1", my_function = sum)
add_Lifemap_markers <- function(lm_obj,
                                information,
                                col_info = NULL,
                                my_function,
                                by="size",
                                pass_info=TRUE,
                                legend=TRUE){

  df <- lm_obj$df[,c("taxid", information,"lon", "lat", "sci_name", "zoom", "ascend", "genomes", "type", "ancestor")]
  basemap <- lm_obj$basemap

  if (is.null(col_info)) {
    col_info <- hcl.colors(length(information),palette = "set3")
  }

  # pass the info
  for (i in 1:length(information)){
    new_df <- pass_infos(df,information=information[i], my_function=my_function)
    for (id in 1:nrow(new_df)) {
      df[df$taxid==new_df[id, "ancestors"], information[i]] <- new_df[id,]$value
    }
  }

  # calculate the new scale for datas
  OldRange <- c()
  NewRange <- c()
  for (i in 1:length(information)) {
    OldRange <- append(OldRange,(max(df[[information[i]]],na.rm = TRUE) - min(df[[information[i]]],na.rm = TRUE)))
    NewRange <- append(NewRange,(50 - 20))
  }

  ui <- fluidPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("mymap", width = "100%", height = "1000px"),
    p()
  )

  server <- function(input, output, session) {

    # define the zone visible by the users
    df_zoom_bounds <- reactive(
      df[df$zoom <= (input$mymap_zoom +5) &
           df$lat > input$mymap_bounds$south &
           df$lat < input$mymap_bounds$north &
           df$lon > input$mymap_bounds$west &
           df$lon < input$mymap_bounds$east,]
    )

    # output of the map
    output$mymap <- renderLeaflet({
      display_map(df,map = basemap) %>% fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    })

    colorpal <- reactive({
      colorNumeric("Accent", df[[information]])
    })

    # modification of the map to display the rights markers
    observe({
      proxy <- leafletProxy("mymap", session=session) %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls()

      for (i in 1:length(information)){
        if (by == "size") {
          radius_info <- (((df_zoom_bounds()[[information[i]]] - 1) * NewRange[i]) / OldRange[i]) + 20
          fillCol_info <- col_info[i]
        }
        if (by == "color") {
          radius_info <- 20
          fillCol_info <- pal(df_zoom_bounds()[[information[i]]])
        }
        proxy <- addCircleMarkers(proxy, lng=df_zoom_bounds()$lon,
                                  lat=df_zoom_bounds()$lat,
                                  radius=radius_info,
                                  fillColor = fillCol_info,
                                  fillOpacity = 0.5,
                                  stroke=FALSE)
        if (legend == TRUE) {
          proxy <- addLegend(proxy, position = "bottomright", title=information,
                             pal = pal, values = df_zoom_bounds()[[information]])

        }
      }
      if (by == "size" && legend == TRUE) {
        proxy <- addLegend(proxy, position = "bottomright", title="size",
                           colors = col_info, labels = information)
      }
    })

    showSciName <- function(taxid, lng, lat) {
      selectedId <- df[round(df$lon, digits=6) == round(lng,digits=6) & round(df$lat, digits=6) == round(lat, digits = 6),]
      content <- as.character(selectedId$taxid)
      for (i in 1:length(information)) {
        new_string <- paste(information[i]," : ", selectedId[[information[i]]], sep="")
        content <- paste(content,new_string, sep="\n")
      }
      leafletProxy("mymap") %>% addPopups(lng, lat, content)
    }

    observe({
      leafletProxy("mymap") %>% clearPopups()
      event <- input$mymap_marker_click
      if (is.null(event))
        return()

      isolate({
        showSciName(event$id, event$lng, event$lat)
      })
    })

  }

  shinyApp(ui, server)
}
