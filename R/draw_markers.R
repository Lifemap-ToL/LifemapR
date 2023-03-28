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

  # fill the matrix (the value of each leaf is added to all of it's ancestors)
  info <- data.frame(ancestors=all_ancestors)
  for (id in only_leaves) {
    info_tmp <- data.frame(unlist(df[df$taxid==id,"ascend"]),df[df$taxid==id,information])
    colnames(info_tmp) <- c("ancestors", as.character(id))

    info <- left_join(info,info_tmp, by="ancestors")
  }

  # apply of the function to get the final value
  if (length(unique(df[df$type=="requested",information])) > 10) {
    my_function <- match.fun(my_function)
    # compute the value for each row
    res <- apply(info,MARGIN=1,function(x){
      tmp <- x[!is.na(x)]
      my_function(tmp[-1])
    })
  } else {
    print(table(df[df$type=="requested",information]))
    print(info)}


  final_res <- data.frame(info$ancestors)
  final_res$information <- res
  colnames(final_res) <- c("ancestors","value")

  return(final_res)

}

#' compute a new scale for a value
#'
#' @param value the value
#' @param df the full dataframe
#' @param df2 the dataframe containing visibles taxas
#' @param min the new range min
#' @param max the new range max
#'
#' @return a vector of values
create_value_range <- function(value, df, df2, min, max, map){
  if (value %in% colnames(df)) {
    old_min <- min(df[[value]], na.rm = TRUE)
    old_max <- max(df[[value]], na.rm = TRUE)
    old_range <- old_max - old_min
    new_range <- max - min
    info <- (((df2[[value]] - old_min) * new_range) / old_range) + min
  } else { info <- value }
  return(info)
}


#' Represent continuous datas on a Lifemap background
#'
#' @param lm_obj a Lifemap object
#'
#' @return a shiny application
#' @export
#' @importFrom dplyr left_join
#' @importFrom shiny fluidPage reactive observe shinyApp
#' @importFrom leaflet leafletOutput renderLeaflet fitBounds leafletProxy addPopups clearMarkers clearShapes clearControls colorNumeric clearPopups
#' @importFrom leaflegend addLegendSize addSymbolsSize
#'
#' @examples
#' load("data/eukaryote_1000.RData")
#' LM_df <- construct_dataframe(eukaryote_1000, basemap = "fr")
#' LM_df + lm_markers(radius="GC.", fillColor="Genes", min=10, max=80, FUN="mean", pal="Accent", legend=TRUE, stroke = TRUE, weight="Size..Mb.")
draw_markers <- function(lm_obj){

  df <- lm_obj$df
  basemap <- lm_obj$basemap
  aes <- lm_obj$aes

  other <- c("min", "max", "pass_info","pal")
  # variables that are columns of the lm_obj dataframe
  variables <- colnames(aes[[1]])[!(colnames(aes[[1]]) %in% other)]

  #pass the information to the nodes or not
  for (i in 1:length(aes)){

    # passing informations if the function is given
    if (!(is.null(aes[[i]]$pass_info))) {
      for (column in variables) {
        if (aes[[i]][[column]] %in% colnames(df) ) {
          new_df <- pass_infos(df, information = aes[[i]][[column]], my_function = aes[[i]]$pass_info)
          for (id in 1:nrow(new_df)) {
            df[df$taxid == new_df[id, "ancestors"], aes[[i]][[column]]]<- new_df[id, ]$value
          }
        }
      }
    }

  }

  ui <- shiny::fluidPage(
    shiny::tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leaflet::leafletOutput("mymap", width = "100%", height = "1000px"),
    shiny::p()
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
      m <- display_map(df,map = basemap) %>% leaflet::fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))

      for (i in 1:length(aes)) {

            if (is.lm_markers(aes[[i]]) & aes[[i]]$legend == TRUE) {

              if (aes[[i]]$radius %in% colnames(df)) {
                m <- m %>%
                  leaflegend::addLegendSize(values = min(df[[aes[[i]]$radius]], na.rm = TRUE):max(df[[aes[[i]]$radius]], na.rm = TRUE),
                                            color = 'black',
                                            opacity = .5,
                                            fillOpacity = 0,
                                            title = aes[[i]]$radius,
                                            shape = aes[[i]]$shape,
                                            orientation = 'horizontal',
                                            baseSize = 50,
                                            position = "bottomright")
              }
              if (aes[[i]]$fillColor %in% colnames(df)) {
                make_fillColor <- leaflet::colorNumeric(aes[[i]]$pal, df[[aes[[i]]$fillColor]])

              m <- m %>% leaflet::addLegend(position = "bottomright",
                                 title = aes[[i]]$fillColor,
                                 pal = make_fillColor,
                                 values = df[[aes[[i]]$fillColor]])
              }
              if (aes[[i]]$color %in% colnames(df)) {
                make_color <- leaflet::colorNumeric(aes[[i]]$pal, df[[aes[[i]]$color]])

                m <- m %>% leaflet::addLegend(position = "bottomright",
                                              title = aes[[i]]$color,
                                              pal = make_color,
                                              values = df[[aes[[i]]$color]])
              }
            }
          }

      m
    })

    # modification of the map to display the rights markers
    shiny::observe({
      # clearing all the already existing shapes/markers/controls
      proxy <- leaflet::leafletProxy("mymap", session=session) %>%
        leaflet::clearShapes() %>%
        leaflet::clearMarkers()

      # adding the visible shapes
      for (i in 1:length(aes)){

        if (aes[[i]]$fillColor %in% colnames(df)) {
          make_fillColor <- leaflet::colorNumeric(aes[[i]]$pal, df[[aes[[i]]$fillColor]])
          fillColor_info <- make_fillColor(df_zoom_bounds()[[aes[[i]]$fillColor]])
        } else { fillColor_info <- aes[[i]]$fillColor }

        radius_info <- create_value_range(aes[[i]]$radius, df, df_zoom_bounds(), aes[[i]]$min, aes[[i]]$max)

        # stroke presence
        if(aes[[i]]$stroke %in% colnames(df)) {
          stroke_info <- df_zoom_bounds()[[aes[[i]]$stroke]]
        } else { stroke_info <- aes[[i]]$stroke }

        if (aes[[i]]$color %in% colnames(df)) {
          make_color <- leaflet::colorNumeric("viridis", df[[aes[[i]]$color]])
          color_info <- make_color(df_zoom_bounds()[[aes[[i]]$color]])
        } else { color_info <- aes[[i]]$color }

        # stroke opacity
        opacity_info <- create_value_range(aes[[i]]$opacity, df, df_zoom_bounds(), 0.1, 1)

        # stroke weight
        weight_info <- create_value_range(aes[[i]]$weight, df, df_zoom_bounds(), 1, 10)

        # fill opacity
        fillOpacity_info <- create_value_range(aes[[i]]$fillOpacity, df, df_zoom_bounds(), 0.1, 1)

        proxy <- leaflegend::addSymbolsSize(proxy,
                                           lng = df_zoom_bounds()$lon,
                                           lat = df_zoom_bounds()$lat,
                                           values = radius_info,
                                           shape = aes[[i]]$shape,
                                           color = color_info,
                                           fillColor = fillColor_info,
                                           fillOpacity = fillOpacity_info,
                                           opacity = opacity_info,
                                           strokeWidth = weight_info,
                                           baseSize = 50
        )
      }

      proxy
    })

    # functions to add popups
    showSciName <- function(taxid, lng, lat) {
      selectedId <- df[round(df$lon, digits=6) == round(lng,digits=6) & round(df$lat, digits=6) == round(lat, digits = 6),]
      content <- as.character(selectedId$taxid)
      for (i in 1:length(aes)) {
        new_string <- paste(aes[[i]]$radius," : ", selectedId[[aes[[i]]$radius]], sep="")
        content <- paste(content,new_string, sep="\n")
      }
      leafletProxy("mymap") %>% leaflet::addPopups(lng, lat, content)
    }

    # when clicking on a marker, show a popup
    observe({
      leafletProxy("mymap") %>% leaflet::clearPopups()
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

