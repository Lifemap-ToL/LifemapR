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
#'
#' @examples
#' df <- read.csv("data/eukaryotes_1000.txt", header=TRUE, sep="\t")
#' df_test[,"GC."] <- as.numeric(as.character(df_test[,"GC."]))
#' df_test[,"Genes"] <- as.numeric(as.character(df_test[,"Genes"]))
#' LM_df <- construct_dataframe(df)
#' add_Lifemap_markers(LM_df, information = c("GC.","Genes"), my_function=mean, legend = FALSE)
add_Lifemap_markers <- function(lm_obj,
                                legend=TRUE){

  # df <- lm_obj$df[,c("taxid", information,"lon", "lat", "sci_name", "zoom", "ascend", "genomes", "type", "ancestor")]
  df <- lm_obj$df
  basemap <- lm_obj$basemap
  aes <- lm_obj$aes

  #pass the information to the nodes or not
  for (i in 1:nrow(aes)){

    # si pas d'info de couleur, on rempli aes
    # current_row <- aes[i,]

    if (!(is.null(aes[i,"pass_info"]))) {
      new_df <- pass_infos(df,information=aes[i,"obj"], my_function=aes[i,"pass_info"])
      for (id in 1:nrow(new_df)) {
        df[df$taxid==new_df[id, "ancestors"], aes[i,"obj"]] <- new_df[id,]$value
      }
    }

    aes$OldRange <- max(df[[aes[i,"obj"]]],na.rm = TRUE) - min(df[[aes[i,"obj"]]],na.rm = TRUE)
    aes$NewRange <- aes[i,"max"] - aes[i,"min"]

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

    # modification of the map to display the rights markers
    observe({
      proxy <- leafletProxy("mymap", session=session) %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls()

      for (i in 1:nrow(aes)){

        if (aes[i,"by"] == "size") {
          radius_info <- (((df_zoom_bounds()[[aes[i,"obj"]]] -1) * aes[i,"NewRange"]) / aes[i,"OldRange"]) + aes[i,"min"]
          fillCol_info <- aes[i,"col"]
          proxy <- addCircleMarkers(proxy, lng=df_zoom_bounds()$lon,
                                    lat=df_zoom_bounds()$lat,
                                    radius=radius_info,
                                    fillColor = fillCol_info,
                                    fillOpacity = 0.5,
                                    stroke=FALSE)
        } else if (aes[i,"by"] == "color") {
          pal <- colorNumeric(aes[i,"col"], df[[aes[i,"obj"]]])
          radius_info <- aes[i,"min"]
          proxy <- addCircleMarkers(proxy, lng=df_zoom_bounds()$lon,
                                    lat=df_zoom_bounds()$lat,
                                    radius=radius_info,
                                    fillColor = pal(df_zoom_bounds()[[aes[i,"obj"]]]),
                                    fillOpacity = 0.5,
                                    stroke=FALSE)
        }

        # proxy <- addCircleMarkers(proxy, lng=df_zoom_bounds()$lon,
        #                           lat=df_zoom_bounds()$lat,
        #                           radius=radius_info,
        #                           fillColor = fillCol_info(df_zoom_bounds()[[aes[i,"obj"]]]),
        #                           fillOpacity = 0.5,
        #                           stroke=FALSE)
        # if (legend == TRUE) {
        #   proxy <- addLegend(proxy, position = "bottomright", title=information,
        #                      pal = pal, values = df_zoom_bounds()[[information]])
        #
        # }
      }
      # if (by == "size" && legend == TRUE) {
      #   proxy <- addLegend(proxy, position = "bottomright", title="size",
      #                      colors = col_info, labels = information)
      # }
      proxy
    })

    # showSciName <- function(taxid, lng, lat) {
    #   selectedId <- df[round(df$lon, digits=6) == round(lng,digits=6) & round(df$lat, digits=6) == round(lat, digits = 6),]
    #   content <- as.character(selectedId$taxid)
    #   for (i in 1:length(information)) {
    #     new_string <- paste(information[i]," : ", selectedId[[information[i]]], sep="")
    #     content <- paste(content,new_string, sep="\n")
    #   }
    #   leafletProxy("mymap") %>% addPopups(lng, lat, content)
    # }
    #
    # observe({
    #   leafletProxy("mymap") %>% clearPopups()
    #   event <- input$mymap_marker_click
    #   if (is.null(event))
    #     return()
    #
    #   isolate({
    #     showSciName(event$id, event$lng, event$lat)
    #   })
    # })

  }

  shinyApp(ui, server)
}

