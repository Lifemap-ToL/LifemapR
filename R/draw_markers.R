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
  # separate leaves from nodes
  all_ancestors <- unique(unlist(df[df$type=="requested",]$ascend))
  tax_request <- as.vector(df[df$type=="requested",]$taxid)
  only_leaves <- setdiff(tax_request,all_ancestors)

  info <- data.frame(ancestors=all_ancestors)
  for (id in only_leaves) {
    info_tmp <- data.frame(unlist(df[df$taxid==id,"ascend"]),df[df$taxid==id,information])
    colnames(info_tmp) <- c("ancestors", as.character(id))

    info <- left_join(info,info_tmp, by="ancestors")
  }

  my_function <- match.fun(my_function)
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
#' @param aes a dataframe representing the aesthetics for the representation
#'
#' @return a shiny application
#' @export
#' @importFrom dplyr left_join
#' @importFrom shiny fluidPage reactive observe shinyApp
#' @importFrom leaflet leafletOutput renderLeaflet fitBounds leafletProxy addCircleMarkers clearMarkers clearShapes clearControls colorNumeric clearPopups
#'
#' @examples
#' df <- read.csv("data/eukaryotes_1000.txt", header=TRUE, sep="\t")
#' df_test[,"GC."] <- as.numeric(as.character(df_test[,"GC."]))
#' df_test[,"Genes"] <- as.numeric(as.character(df_test[,"Genes"]))
#' LM_df <- construct_dataframe(df)
#' draw_markers(LM_df, information = c("GC.","Genes"), my_function=mean, legend = FALSE)
draw_markers <- function(lm_obj, legend=TRUE){

  # df <- lm_obj$df[,c("taxid", information,"lon", "lat", "sci_name", "zoom", "ascend", "genomes", "type", "ancestor")]
  df <- lm_obj$df
  basemap <- lm_obj$basemap
  aes <- lm_obj$aes

  #pass the information to the nodes or not
  for (i in 1:nrow(aes)){

    # si pas d'info de couleur, on rempli aes
    # current_row <- aes[i,]

    # passing informations if the function is given
    if (!(is.null(aes[i,"pass_info"]))) {
      new_df <- pass_infos(df,information=aes[i,"size"], my_function=aes[i,"pass_info"])
      new_df2 <- pass_infos(df,information=aes[i,"col"], my_function=aes[i,"pass_info"])
      for (id in 1:nrow(new_df)) {
        df[df$taxid==new_df[id, "ancestors"], aes[i,"size"]] <- new_df[id,]$value
        df[df$taxid==new_df[id, "ancestors"], aes[i,"col"]] <- new_df2[id,]$value
      }
    }

    # calculating the ranges of values for marker's size
    aes$OldRange <- max(df[[aes[i,"size"]]], na.rm = TRUE) - min(df[[aes[i,"size"]]], na.rm = TRUE)
    aes$NewRange <- aes[i,"max"] - aes[i,"min"]

  }

  ui <- shiny::fluidPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leaflet::leafletOutput("mymap", width = "100%", height = "1000px"),
    p()
  )

  server <- function(input, output, session) {

    # define the zone visible by the users
    df_zoom_bounds <- shiny::reactive(
      df[df$zoom <= (input$mymap_zoom +5) &
           df$lat > input$mymap_bounds$south &
           df$lat < input$mymap_bounds$north &
           df$lon > input$mymap_bounds$west &
           df$lon < input$mymap_bounds$east,]
    )

    # output of the map
    output$mymap <- leaflet::renderLeaflet({
      display_map(df,map = basemap) %>% leaflet::fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    })

    # modification of the map to display the rights markers
    # shiny::observe({
    #   proxy <- leaflet::leafletProxy("mymap", session=session) %>%
    #     leaflet::clearShapes() %>%
    #     leaflet::clearMarkers() %>%
    #     leaflet::clearControls()
    #
    #   for (i in 1:nrow(aes)){
    #
    #     pal <- leaflet::colorNumeric(aes[i,"pal"], df[[aes[i,"col"]]])
    #     radius_info <- (((df_zoom_bounds()[[aes[i,"size"]]] -1) * aes[i,"NewRange"]) / aes[i,"OldRange"]) + aes[i,"min"]
    #
    #     proxy <- leaflet::addCircleMarkers(proxy, lng=df_zoom_bounds()$lon,
    #                                        lat=df_zoom_bounds()$lat,
    #                                        radius=radius_info,
    #                                        fillColor = pal(df_zoom_bounds()[[aes[i,"col"]]]),
    #                                        fillOpacity = 0.6,
    #                                        stroke=FALSE)
    #
    #   #   if (aes[i,"by"] == "size") {
    #   #     radius_info <- (((df_zoom_bounds()[[aes[i,"obj"]]] -1) * aes[i,"NewRange"]) / aes[i,"OldRange"]) + aes[i,"min"]
    #   #     fillCol_info <- aes[i,"col"]
    #   #     proxy <- leaflet::addCircleMarkers(proxy, lng=df_zoom_bounds()$lon,
    #   #                               lat=df_zoom_bounds()$lat,
    #   #                               radius=radius_info,
    #   #                               fillColor = fillCol_info,
    #   #                               fillOpacity = 0.5,
    #   #                               stroke=FALSE)
    #   #     # if (legend == TRUE) {
    #   #     #   proxy <- leaflegend::addLegendSize(proxy,
    #   #     #                                      values=df_zoom_bounds()[[aes[i,"obj"]]],
    #   #     #                                      color=aes[i,"col"],
    #   #     #                                      title = aes[i,"obj"],
    #   #     #                                      shape = "circle")
    #   #     # }
    #   #   } else if (aes[i,"by"] == "color") {
    #   #     pal <- leaflet::colorNumeric(aes[i,"col"], df[[aes[i,"obj"]]])
    #   #     radius_info <- aes[i,"min"]
    #   #     proxy <- leaflet::addCircleMarkers(proxy, lng=df_zoom_bounds()$lon,
    #   #                               lat=df_zoom_bounds()$lat,
    #   #                               radius=radius_info,
    #   #                               fillColor = pal(df_zoom_bounds()[[aes[i,"obj"]]]),
    #   #                               fillOpacity = 0.5,
    #   #                               stroke=FALSE)
    #   #     if (legend == TRUE) {
    #   #         proxy <- leaflet::addLegend(proxy,
    #   #                                     position = "bottomright",
    #   #                                     title=aes[i,"obj"],
    #   #                                     pal = pal,
    #   #                                     values = df_zoom_bounds()[[aes[i,"obj"]]])
    #   #     }
    #   #   }
    #   }
    #   proxy
    # })

    # functions to add popups
    showSciName <- function(taxid, lng, lat) {
      print(class(lat))
      print(class(df_zoom_bounds()$lat))
      selectedId <- df[round(df$lon, digits=6) == round(lng,digits=6) & round(df$lat, digits=6) == round(lat, digits = 6),]
      # selectedId <- df[which(identical(round(df$lon, digits=6),round(lng,digits=6)) && identical(round(df$lat, digits=6), round(lat, digits = 6))),]
      print(selectedId)
      content <- as.character(selectedId$taxid)
      for (i in 1:nrow(aes)) {
        new_string <- paste(aes[i, "size"]," : ", selectedId[[aes[i,"size"]]], sep="")
        content <- paste(content,new_string, sep="\n")
      }
      leafletProxy("mymap") %>% addPopups(lng, lat, content)
    }

    # when clicking on a marker, show a popup
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

