
#' compute a new scale for a value
#'
#' @param value a vector of values
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


#' compute the aes for a set of points
#'
#' @param aes the dataframe containing the aesthetics informations (must be of lm_markers class)
#' @param df the full dataframe
#' @param df_zoom_bounds the dataframe containing visibles taxas
#'
#' @importFrom leaflet addCircleMarkers
#'
#' @return a list of values
add_lm_markers <- function(aes, df, df_visible, proxy) {

  if (aes$fillColor %in% colnames(df)) {
    make_fillColor <- leaflet::colorNumeric(aes$fillColor_pal, df[[aes$fillColor]])
    fillColor_info <- make_fillColor(df_visible[[aes$fillColor]])
  } else { fillColor_info <- aes$fillColor }

  radius_info <- create_value_range(aes$radius, df, df_visible, aes$min, aes$max)

  # stroke presence
  if(aes$stroke %in% colnames(df)) {
    stroke_info <- df_visible[[aes$stroke]]
  } else { stroke_info <- aes$stroke }

  #stroke color
  if (aes$color %in% colnames(df)) {
    make_color <- leaflet::colorNumeric(aes$color_pal, df[[aes$color]])
    color_info <- make_color(df_visible[[aes$color]])
  } else { color_info <- aes$color }

  # stroke opacity
  opacity_info <- create_value_range(aes$opacity, df, df_visible, 0.1, 1)

  # stroke weight
  weight_info <- create_value_range(aes$weight, df, df_visible, 1, 10)

  # fill opacity
  fillOpacity_info <- create_value_range(aes$fillOpacity, df, df_visible, 0.1, 1)


  proxy <- leaflet::addCircleMarkers(proxy,
                                     lng = df_visible$lon,
                                     lat = df_visible$lat,
                                     radius = radius_info,
                                     fillColor = fillColor_info,
                                     fillOpacity = fillOpacity_info,
                                     stroke = stroke_info,
                                     color = color_info,
                                     opacity = opacity_info,
                                     weight = weight_info
  )
  proxy

}


#' Represent continuous datas on a Lifemap background
#'
#' @description
#' draw a map and all the aesthetics in the order you put them, the last one will be on top of the others
#'
#' @param lm_obj a Lifemap object filled with aesthetics
#'
#' @return a shiny application
#' @export
#' @importFrom dplyr left_join
#' @importFrom shiny fluidPage reactive observe shinyApp
#' @importFrom leaflet leafletOutput renderLeaflet fitBounds leafletProxy addPopups clearMarkers clearShapes clearControls colorNumeric colorFactor clearPopups addPolylines
#' @importFrom leaflegend addLegendSize addSymbolsSize
#'
#' @examples
#' load("data/eukaryote_1000.RData")
#' LM <- build_Lifemap(eukaryote_1000, basemap = "fr")
#' LM +
#' lm_markers(radius = "GC.", fillColor = "Size..Mb.", min = 10, max = 80, FUN="mean", fillColor_pal = "Accent", legend = TRUE, stroke = TRUE) +
#' lm_branches(col = "Genes", FUN = "mean")
draw_markers <- function(lm_obj){

  df <- lm_obj$df
  basemap <- lm_obj$basemap
  aes <- lm_obj$aes

  M <- create_matrix(df)
  M_discrete <- create_matrix_discret(df)

  cat("passing the information to the nodes \n")
  #pass the information to the nodes or not
  for (i in 1:length(aes)){
    # passing informations if the function is given
    if (is.lm_markers(aes[[i]]) && !(is.null(aes[[i]]$FUN))) {
      for (parameter in aes[[i]]) {

        if (!(is.null(parameter)) && parameter %in% colnames(df)) {
          new_df <- pass_infos(M = M,
                               values = as.vector(df[df$type == "requested", parameter]),
                               ancestors = unique(unlist(df[df$type == "requested", "ascend"])),
                               my_func = aes[[i]]$FUN)
          for (id in 1:nrow(new_df)) {
            df[df$taxid == new_df[id, "ancestors"], parameter]<- new_df[id, ]$value
          }
        }
      }

    } else if (is.lm_branches(aes[[i]]) && aes[[i]]$color %in% colnames(df)) {
      new_df <- pass_infos(M = M,
                           values = as.vector(df[df$type == "requested",aes[[i]]$color]),
                           ancestors = unique(unlist(df[df$type == "requested","ascend"])),
                           my_func = aes[[i]]$FUN)
      print("oskour")
      for (id in 1:nrow(new_df)) {
        df[df$taxid == new_df[id, "ancestors"], aes[[i]]$color]<- new_df[id, ]$value
      }

    } else if (is.lm_discret(aes[[i]])) {
      new_df <- pass_infos_discret(M = M_discrete,
                                   values = df[df$type == "requested", aes[[i]]$param],
                                   tax = df$taxid)
      df <- merge(df, new_df, by.x = "taxid", by.y = "tax")
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
      df[df$zoom <= (input$mymap_zoom + 4) &
           df$lat > input$mymap_bounds$south &
           df$lat < input$mymap_bounds$north &
           df$lon > input$mymap_bounds$west &
           df$lon < input$mymap_bounds$east,]
    )

    # define the descendants of df_zoom_bounds' taxids
    df_descendants <- shiny::reactive({
      visibles <- df_zoom_bounds()$taxid
      df[df$ancestor %in% visibles,]
    })

    # output of the map
    output$mymap <- leaflet::renderLeaflet({
      m <- display_map(df,map = basemap) %>% leaflet::fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))

      for (i in 1:length(aes)) {

        if (is.lm_markers(aes[[i]]) & aes[[i]]$legend == TRUE) {

          if (aes[[i]]$radius %in% colnames(df)) {
            m <- m %>%
              leaflegend::addLegendSize(values = min(df[[aes[[i]]$radius]], na.rm = TRUE):max(df[[aes[[i]]$radius]], na.rm = TRUE),
                                        color = aes[[i]]$color,
                                        opacity = aes[[i]]$legendOpacity,
                                        fillOpacity = 0,
                                        title = aes[[i]]$radius,
                                        shape = "circle",
                                        orientation = aes[[i]]$legendOrientation,
                                        baseSize = (aes[[i]]$min + ((aes[[i]]$max - aes[[i]]$min) / 2)) * 2,
                                        position = aes[[i]]$legendPosition)
          }
          if (aes[[i]]$fillColor %in% colnames(df)) {
            make_fillColor <- leaflet::colorNumeric(aes[[i]]$fillColor_pal, df[[aes[[i]]$fillColor]])

            m <- m %>% leaflet::addLegend(position = "bottomright",
                                          title = aes[[i]]$fillColor,
                                          pal = make_fillColor,
                                          values = df[[aes[[i]]$fillColor]])
          }
          if (aes[[i]]$color %in% colnames(df)) {
            make_color <- leaflet::colorNumeric(aes[[i]]$color_pal, df[[aes[[i]]$color]])

            m <- m %>% leaflet::addLegend(position = "bottomright",
                                          title = aes[[i]]$color,
                                          pal = make_color,
                                          values = df[[aes[[i]]$color]])
          }
        } else if (is.lm_branches(aes[[i]])) {
          if (aes[[i]]$color %in% colnames(df)) {
            make_color <- leaflet::colorNumeric(aes[[i]]$color_pal, df[[aes[[i]]$color]])

            m <- m %>% leaflet::addLegend(position = "bottomright",
                                          title = paste("subtree : ", aes[[i]]$color, sep = "", collapse = ""),
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
        leaflet::clearMarkers() %>%
        leaflet.minicharts::clearMinicharts()

      # adding the visible shapes
      for (i in 1:length(aes)){

        if (!(is.null(aes[[i]]$taxids))) {
          ancestors <- unique(unlist(df[df$taxid %in% aes[[i]]$taxids[[1]],"ascend"]))
          all_taxids <- c(df[df$taxid %in% aes[[i]]$taxids[[1]],"taxid"], ancestors)
          df_visible = df_zoom_bounds()[df_zoom_bounds()$taxid %in% all_taxids,]
        } else { df_visible = df_zoom_bounds()}

        if (is.lm_markers(aes[[i]])) {
          proxy <- add_lm_markers(aes = aes[[i]], df = df, df_visible = df_visible, proxy = proxy)

        } else if (is.lm_branches(aes[[i]])) {
          if (aes[[i]]$color %in% colnames(df)) {
            make_col <- leaflet::colorNumeric(palette = aes[[i]]$color_pal, domain = df[[aes[[i]]$color]])
          }

          for (id in df_visible$taxid) {
            # for each descendant of each taxid
            for (desc in df_descendants()[df_descendants()$ancestor == id, ]$taxid) {
              if (aes[[i]]$color %in% colnames(df)) {
                col_info <- make_col(df_descendants()[df_descendants()$taxid == desc, aes[[i]]$color])
              } else { col_info <- aes[[i]]$color}

              if (!(is.null(aes[[i]]$taxids))) {
                descendants_visible <- df_descendants()[df_descendants()$taxid %in% all_taxids, ]
              } else { descendants_visible = df_descendants() }

              proxy <- leaflet::addPolylines(proxy,
                                      lng = c(df_visible[df_visible$taxid == id, "lon"],
                                              descendants_visible[descendants_visible$taxid == desc, "lon"]),
                                      lat = c(df_visible[df_visible$taxid == id, "lat"],
                                              descendants_visible[descendants_visible$taxid == desc, "lat"]),
                                      color = col_info,
                                      opacity = 0.7)
            }
          }
        } else if(is.lm_discret(aes[[i]]) && nrow(df_visible) > 0) {
          values <- unique(df[df$type == "requested", aes[[i]]$param])
          # values <- values[!is.na(values)]
          make_col <- colorFactor(aes[[i]]$pal,values)
            proxy <- proxy %>%
              leaflet.minicharts::addMinicharts(
                lng = df_visible$lon,
                lat = df_visible$lat,
                chartdata = df_visible[,values],
                type = aes[[i]]$type,
                colorPalette = make_col(values),
                width = aes[[i]]$width,
                height = aes[[i]]$height,
                opacity = aes[[i]]$opacity,
                showLabels = aes[[i]]$showLabels,
                transitionTime = 0,
                legend = aes[[i]]$legend,
                legendPosition = aes[[i]]$legendPosition
            )

        }
      }

      proxy
    })

    # functions to add popups
    showSciName <- function(taxid, lng, lat) {
      selectedId <- df[round(df$lon, digits=6) == round(lng,digits=6) & round(df$lat, digits=6) == round(lat, digits = 6),]
      content <- as.character(selectedId$taxid)
      for (i in 1:length(aes)) {
        new_string <- paste(aes[[i]]$fillColor," : ", selectedId[[aes[[i]]$fillColor]], sep="")
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
