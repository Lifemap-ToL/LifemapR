
#' Compute a new scale for a value
#'
#' @param value A vector of values.
#' @param df The full dataframe.
#' @param df2 The dataframe containing visibles taxa.
#' @param min The new minimum of the range.
#' @param max The new maximum of the range.
#'
#' @return A vector of values.
create_value_range <- function(value, df, df2, min, max) {
  if (value %in% colnames(df)) {
    old_min <- min(df[[value]], na.rm = TRUE)
    old_max <- max(df[[value]], na.rm = TRUE)
    old_range <- old_max - old_min
    new_range <- max - min
    info <- (((df2[[value]] - old_min) * new_range) / old_range) + min
  } else {
    info <- value
  }
  return(info)
}


#' Compute the aesthetics for markers visualisation.
#'
#' @param aes The dataframe containing the aesthetics information (must be of lm_markers class).
#' @param df The full dataframe.
#' @param df_visible The dataframe containing visible taxa.
#' @param proxy The map to be modified.
#' @param group_info The ID of this group of markers.
#'
#' @importFrom leaflet addCircleMarkers colorNumeric
#'
#' @return An updated map with the new layer added.
add_lm_markers <- function(proxy, aes, df, df_visible, group_info) {

  if (!(aes$var_fillColor %in% "default")) {
    if (is.numeric(df[[aes$var_fillColor]])) {
      make_fillColor <- leaflet::colorNumeric(palette = aes$fillPalette, domain = df[[aes$var_fillColor]], reverse = TRUE)
      fillColor_info <- make_fillColor(df_visible[[aes$var_fillColor]])
    } else {
      make_fillColor <- leaflet::colorFactor(palette = aes$fillPalette, domain = df[[aes$var_fillColor]], reverse = TRUE)
      fillColor_info <- make_fillColor(df_visible[[aes$var_fillColor]])
    }
  } else {
    fillColor_info <- aes$fillColor
  }

  if (aes$radius %in% "default") {
    radius_info <- create_value_range(aes$value, df, df_visible, aes$min, aes$max)
    # radius_info <- aes$value
  } else {
    radius_info <- create_value_range(aes$radius, df, df_visible, aes$min, aes$max)
  }

  # stroke presence
  if (isTRUE(aes$stroke)) {
    if (!(aes$var_color %in% "default")) {
      if (is.numeric(df[[aes$var_color]])) {
        make_color <- leaflet::colorNumeric(aes$palette, df[[aes$var_color]], reverse = TRUE)
        color_info <- make_color(df_visible[[aes$var_color]])
      } else {
        make_color <- leaflet::colorFactor(palette = aes$palette, domain = df[[aes$var_color]], reverse = TRUE)
        color_info <- make_color(df_visible[[aes$var_color]])
      }
    } else {
      color_info <- aes$color
    }
  } else {
    color_info <- aes$color
  }

  # stroke opacity
  opacity_info <- create_value_range(aes$opacity, df, df_visible, 0.1, 1)

  # stroke weight
  weight_info <- create_value_range(aes$weight, df, df_visible, 1, 10)

  # fill opacity
  fillOpacity_info <- create_value_range(aes$fillOpacity, df, df_visible, 0.1, 1)

  ### to improve ###
  if (is.null(aes$label)) {
    proxy <- leaflet::addCircleMarkers(proxy,
                                       lng = df_visible$lon,
                                       lat = df_visible$lat,
                                       radius = radius_info,
                                       fillColor = fillColor_info,
                                       fillOpacity = fillOpacity_info,
                                       stroke = aes$stroke,
                                       color = color_info,
                                       opacity = opacity_info,
                                       weight = weight_info,
                                       group = group_info
    )
  } else if (length(df_visible[[aes$label]]) > 0) {
    proxy <- leaflet::addCircleMarkers(proxy,
                                       lng = df_visible$lon,
                                       lat = df_visible$lat,
                                       radius = radius_info,
                                       fillColor = fillColor_info,
                                       fillOpacity = fillOpacity_info,
                                       stroke = aes$stroke,
                                       color = color_info,
                                       opacity = opacity_info,
                                       weight = weight_info,
                                       group = group_info,
                                       label = df_visible[[aes$label]]
    )
  }
  proxy
}

#' Compute the aesthetics for a subtree visualisation.
#'
#' @param proxy The map to be modified.
#' @param aes The dataframe containing the aesthetics details (must be of lm_branches class).
#' @param df The full dataframe.
#' @param df_visible The dataframe containing visible taxa.
#' @param df_descendants The dataframe containing all the information on the descendants of visible taxa.
#' @param group_info the ID of this group of lines.
#' @param all_taxids A vector containing all the visible taxids and their direct descendants.
#'
#' @importFrom leaflet addPolylines colorNumeric
#'
#' @return An updated map with the new layer added.
add_lm_branches <- function(proxy, aes, df, df_visible, df_descendants, group_info, all_taxids) {
  if (!(aes$var_color %in% "default")) {
    make_col <- leaflet::colorNumeric(palette = aes$palette, domain = df[[aes$var_color]], reverse = TRUE)
  }

  if (aes$size %in% colnames(df)) {
    old_min <- min(df[[aes$size]], na.rm = TRUE)
    old_max <- max(df[[aes$size]], na.rm = TRUE)
    old_range <- old_max - old_min
    new_range <- aes$max - aes$min
  }

  if (!(is.null(aes$taxids))) {
    descendants_visible <- df_descendants[df_descendants$taxid %in% all_taxids, ]
  } else {
    descendants_visible <- df_descendants
  }

  for (id in df_visible$taxid) {
    # for each descendant of each taxid
  
    for (desc in descendants_visible[descendants_visible$ancestor == id, ]$taxid) {
      if (!(aes$var_color %in% "default")) {
        col_info <- make_col(descendants_visible[descendants_visible$taxid == desc, aes$var_color])
      } else {
        col_info <- aes$color
      }
    
      if (aes$size %in% colnames(df)) {
        size_info <- (((descendants_visible[descendants_visible$taxid == desc, aes$size] - old_min) * new_range) / old_range) + aes$min
      } else {
        size_info <- aes$value
      }

      proxy <- leaflet::addPolylines(proxy,
                                     lng = c(df_visible[df_visible$taxid == id, "lon"],
                                             descendants_visible[descendants_visible$taxid == desc, "lon"]),
                                     lat = c(df_visible[df_visible$taxid == id, "lat"],
                                             descendants_visible[descendants_visible$taxid == desc, "lat"]),
                                     color = col_info,
                                     opacity = aes$opacity,
                                     fillOpacity = 0.5,
                                     group = group_info,
                                     weight = size_info)
    }
  }
  proxy
}
#' Compute the aesthetics for discret values visualisation.
#'
#' @param aes The dataframe containing the aesthetics details (must be of lm_piecharts class).
#' @param df The full dataframe.
#' @param df_visible The dataframe containing visible taxa.
#' @param proxy The map to be modified.
#' @param layer The ID of this group of charts
#'
#' @importFrom leaflet.minicharts addMinicharts
#' @importFrom leaflet colorFactor
#'
#' @return An updated map with the new layer added.
add_lm_piecharts <- function(proxy, aes, df, df_visible, layer) {
  values <- unique(df[df$type == "requested", aes$param])
  layerId_info <- sapply(X = seq_len(nrow(df_visible)), FUN = function(x) {paste(layer, x, collapse = "", sep = "")})
  make_col <- leaflet::colorFactor(aes$pal, values)
  proxy <- proxy |>
    leaflet.minicharts::addMinicharts(
      lng = df_visible$lon,
      lat = df_visible$lat,
      chartdata = df_visible[, values],
      type = aes$type,
      colorPalette = make_col(values),
      width = aes$width,
      height = aes$height,
      opacity = aes$opacity,
      showLabels = aes$showLabels,
      transitionTime = 0,
      legend = aes$legend,
      legendPosition = aes$legendPosition,
      layerId = layerId_info
    )
  proxy
}


#' Compute the different display options.
#'
#' @param m The map to be modified.
#' @param aes The dataframe containing the aesthetics details
#' @param df The full dataframe.
#' @param type A string indicating the type of representation, either "markers" or "discret"
#' @param leaves The Vector of all the terminal taxids.
#' @param i The index of the aesthetics.
#'
#' @return An updated map.
display_option <- function(m, aes, df, type, leaves, i) {

  if (aes$display == "requested") {
    df_visible <- df[df$type == "requested", ]
  } else if (aes$display == "all") {
    df_visible <- df
  } else if (aes$display == "leaves") {
    df_visible <- df[df$taxid %in% leaves, ]
  }
  ancestors <- unique(unlist(df[df$taxid %in% aes$taxids[[1]], "ascend"]))
  all_taxids <- c(df[df$taxid %in% aes$taxids[[1]], "taxid"], ancestors)
  if (!(is.null(aes$taxids))) {
    df_visible <- df_visible[df_visible$taxid %in% all_taxids, ]
  }
  if (nrow(df_visible) < 5000) {

    if (type == "markers") {
      m <- m |>
        add_lm_markers(aes = aes, df = df,
                       df_visible = df_visible,
                       group_info = as.character(i))
    } else if (type == "discret") {
      m <- m |>
        add_lm_piecharts(aes = aes, df = df,
                         df_visible = df_visible,
                         layer = as.character(i))
    }
  } else {
    stop("you are trying to draw to many points at a time, maybe you shoud try another options")
  }
  return(m)
}


#' Represent data on a Lifemap basemap.
#'
#' @description
#' Draw a map and all the aesthetics in the order you put them in, the last one will be on top of the others.
#'
#' @param lm_obj A Lifemap object filled with aesthetics.
#'
#' @return A shiny application
#'
#' @export
#' @importFrom dplyr left_join
#' @importFrom shiny fluidPage reactive observe shinyApp isolate
#' @importFrom htmltools tags HTML p
#' @importFrom leaflet leafletOutput renderLeaflet fitBounds leafletProxy addPopups clearMarkers clearShapes clearControls colorNumeric colorFactor clearPopups addPolylines clearGroup addLegend
#' @importFrom leaflet.minicharts clearMinicharts
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' data(LM_eukaryotes)
#' lifemap(LM_eukaryotes) + lm_markers() + lm_branches()
#' }
draw_Lifemap <- function(lm_obj) {

  df <- lm_obj$df
  basemap <- lm_obj$basemap
  aes <- lm_obj$aes
  zoom_level <- lm_obj$options$zoom

  all_ancestors <- unique(unlist(df$ascend))
  leaves <- df[!(df$taxid %in% all_ancestors), "taxid"]

  variables <- c()
  for (i in seq_along(aes)) {
    for (param in aes[[i]]){
      if ((is.character(param)) && param %in% colnames(df)) {
        variables <- append(variables, param)
      }
    }
  }
  variables <- unique(variables)

  if (length(variables) > 0) {
    M <- create_matrix(df, variables)
  }

  cat("passing the information to the nodes \n")
  #pass the information to the nodes or not
  for (i in seq_along(aes)) {
    # passing information if the function is given
    if (is.lm_markers(aes[[i]]) && !(is.null(aes[[i]]$FUN))) {
      for (parameter in aes[[i]]) {

        if (!(is.null(parameter)) && is.character(parameter) && parameter %in% colnames(df)) {
          new_df <- pass_infos(M = M,
                               FUN = aes[[i]]$FUN,
                               value = parameter)
          for (id in names(new_df)) {
            if (is.na(df[df$taxid == id, parameter])) {
              df[df$taxid == id, parameter] <- new_df[id]
            }
          }
        }
      }
    } else if (is.lm_branches(aes[[i]]) && !(is.null(aes[[i]]$FUN))) {
      for (parameter in aes[[i]]) {
        if (!(is.null(parameter)) && is.character(parameter) && parameter %in% colnames(df)) {
          new_df <- pass_infos(M = M,
                               FUN = aes[[i]]$FUN,
                               value = parameter)
          for (id in names(new_df)) {
            if (is.na(df[df$taxid == id, parameter])) {
              df[df$taxid == id, parameter] <- new_df[id]
            }
          }
        }
      }
    } else if (is.lm_piecharts(aes[[i]])) {
      new_df <- pass_infos_discret(M = M,
                                   value = aes[[i]]$param)
      df <- merge(df, new_df, by.x = "taxid", by.y = "taxid")
    }
  }

  ui <- shiny::fluidPage(
    htmltools::tags$head(
      htmltools::tags$style(
        htmltools::HTML("
            .leaflet-container {
             background: #000000;
             outline: 0;
            }
            .container-fluid {
                padding:0px;
            }
            #mymap {
                width:100% !important;
                height:100% !important;
                position:fixed !important;
            }
           ")
      )),
  
    # htmltools::tags$style(type = "text/css", "#mymap {height: calc(100vh) !important; }"),
    leaflet::leafletOutput("mymap"),
    htmltools::p()
  )

  server <- function(input, output, session) {
  
    # define the zone visible by the users
    df_zoom_bounds <- shiny::reactive({
      if (is.null(input$mymap_zoom) || is.null(input$mymap_bounds)) {
        return(df)
      }
      df[df$zoom <= (input$mymap_zoom + zoom_level) &
        df$lat > input$mymap_bounds$south &
        df$lat < input$mymap_bounds$north &
        df$lon > input$mymap_bounds$west &
        df$lon < input$mymap_bounds$east, ]
    })

    # define the descendants of df_zoom_bounds' taxids
    df_descendants <- shiny::reactive({
      visibles <- df_zoom_bounds()$taxid
      df[df$ancestor %in% visibles, ]
    })
  
    addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5, title, position) {
    
      make_shapes <- function(colors, sizes, borders, shapes) {
        shapes <- gsub("circle", "50%", shapes)
        shapes <- gsub("square", "0%", shapes)
        paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
      }
      make_labels <- function(sizes, labels) {
        paste0("<div style='display: inline-block;height: ",
               sizes, "px;margin-top: 4px;line-height: ",
               sizes, "px;'>", labels, "</div>")
      }
    
      legend_colors <- make_shapes(colors, sizes, borders, shapes)
      legend_labels <- make_labels(sizes, labels)
    
      return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, title = title, position = position))
    }
  
    # output of the map
    output$mymap <- leaflet::renderLeaflet({
      m <- display_map(df, basemap = basemap) |> leaflet::fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    
      for (i in seq_along(aes)) {
      
        if (is.lm_markers(aes[[i]])) {
        
          # if particular display option, it is drawn now
          if (!(aes[[i]]$display %in% "auto")) {
            m <- display_option(m = m, aes = aes[[i]], df = df, type = "markers", leaves = leaves, i = i)
          }
        
          # ading legend if necessary
          if (aes[[i]]$legend == TRUE) {
          
            if (aes[[i]]$radius %in% colnames(df)) {
              part <- (max(df[[aes[[i]]$radius]], na.rm = TRUE) - min(df[[aes[[i]]$radius]], na.rm = TRUE)) / 4
              part_vector <- c(min(df[[aes[[i]]$radius]], na.rm = TRUE),
                               min(df[[aes[[i]]$radius]], na.rm = TRUE) + part,
                               min(df[[aes[[i]]$radius]], na.rm = TRUE) + part * 2,
                               min(df[[aes[[i]]$radius]], na.rm = TRUE) + part * 3,
                               max(df[[aes[[i]]$radius]], na.rm = TRUE))

              old_min <- min(df[[aes[[i]]$radius]], na.rm = TRUE)
              old_max <- max(df[[aes[[i]]$radius]], na.rm = TRUE)
              old_range <- old_max - old_min
              new_range <- aes[[i]]$max - aes[[i]]$min

              colors <- c("white", "white", "white", "white", "white")
              labels <- as.character(round(part_vector))
              sizes <- sapply(part_vector, FUN = function(x) {(((x - old_min) * new_range) / old_range) + aes[[i]]$min})
              sizes <- sizes * 2
              shapes <- c("circle", "circle", "circle", "circle", "circle")
              borders <- c("black", "black", "black", "black", "black")

              m <- m |> addLegendCustom(colors, labels, sizes, shapes, borders,
                                        title = as.character(aes[[i]]$radius),
                                        position = aes[[i]]$legendPosition)
            }
            if ((!is.null(aes[[i]]$var_fillColor)) && aes[[i]]$var_fillColor %in% colnames(df)) {
              if (is.numeric(df[[aes[[i]]$var_fillColor]])) {
                make_fillColor <- leaflet::colorNumeric(palette = aes[[i]]$fillPalette, domain = df[[aes[[i]]$var_fillColor]], reverse = TRUE)
              } else {
                make_fillColor <- leaflet::colorFactor(palette = aes[[i]]$fillPalette, domain = df[[aes[[i]]$var_fillColor]], reverse = TRUE)
              }

              m <- m |> leaflet::addLegend(position = "bottomright",
                                           title = aes[[i]]$var_fillColor,
                                           pal = make_fillColor,
                                           values = df[[aes[[i]]$var_fillColor]])
            }
            if ((!is.null(aes[[i]]$var_color)) && aes[[i]]$var_color %in% colnames(df)) {
              if (is.numeric(df[[aes[[i]]$var_color]])) {
                make_color <- leaflet::colorNumeric(aes[[i]]$palette, df[[aes[[i]]$var_color]], reverse = TRUE)
              } else {
                make_color <- leaflet::colorFactor(aes[[i]]$palette, df[[aes[[i]]$var_color]], reverse = TRUE)
              }

              m <- m |> leaflet::addLegend(position = "bottomright",
                                           title = aes[[i]]$var_color,
                                           pal = make_color,
                                           values = df[[aes[[i]]$var_color]])
            }
          }
        } else if (is.lm_branches(aes[[i]])) {
          if (aes[[i]]$legend == TRUE) {
            if (!(aes[[i]]$var_color %in% "default")) {
              make_color <- leaflet::colorNumeric(aes[[i]]$palette, df[[aes[[i]]$var_color]], reverse = TRUE)

              m <- m |> leaflet::addLegend(position = aes[[i]]$legendPosition,
                                           title = paste("subtree : ", aes[[i]]$var_color, sep = "", collapse = ""),
                                           pal = make_color,
                                           values = df[[aes[[i]]$var_color]])
            }
          }
        } else if (is.lm_piecharts(aes[[i]])) {
          if (!(aes[[i]]$display %in% "auto")) {
            m <- display_option(m = m, aes = aes[[i]], df = df, type = "discret", leaves = leaves, i = i)
          }
        }
      }
      m
    })

    # modification of the map to display the rights markers
    shiny::observe({

      # clearing all the already existing shapes/markers/controls
      proxy <- leaflet::leafletProxy("mymap", session = session)

      # adding the visible shapes
      for (i in seq_along(aes)) {

        # for each aesthetic, if a sub dataset is given, compute the right taxids to be used
        ancestors <- unique(unlist(df[df$taxid %in% aes[[i]]$taxids[[1]], "ascend"]))
        all_taxids <- c(df[df$taxid %in% aes[[i]]$taxids[[1]], "taxid"], ancestors)
        if (!(is.null(aes[[i]]$taxids))) {
          df_visible <- df_zoom_bounds()[df_zoom_bounds()$taxid %in% all_taxids, ]
        } else {
          df_visible <- df_zoom_bounds()
        }

        # adding markers if aes[[i]] is an lm_markers object
        if (is.lm_markers(aes[[i]]) && aes[[i]]$display == "auto") {
          proxy <- leaflet::clearGroup(proxy, group = as.character(i)) |>
            add_lm_markers(aes = aes[[i]], df = df,
                           df_visible = df_visible,
                           group_info = as.character(i))

          # adding a subtree if aes[[i]] is an lm_branches object
        } else if (is.lm_branches(aes[[i]])) {
          proxy <- leaflet::clearGroup(proxy, group = as.character(i)) |>
            add_lm_branches(aes = aes[[i]], df = df,
                            df_visible = df_visible,
                            df_descendants = df_descendants(),
                            group_info = as.character(i),
                            all_taxids = all_taxids)

          # adding charts if aes[[i]] is an lm_piecharts object
        } else if (is.lm_piecharts(aes[[i]]) && nrow(df_visible) > 0 && aes[[i]]$display %in% "auto") {
          proxy <- leaflet.minicharts::clearMinicharts(proxy) |>
            add_lm_piecharts(aes = aes[[i]], df = df,
                             df_visible = df_visible,
                             layer = as.character(i))
        }
      }
      proxy
    })

    # functions to add popups
    showSciName_popup <- function(group, lng, lat) {
      if (!is.null(aes[[as.numeric(group)]]$popup)) {
        selectedId <- df[round(df$lon, digits = 6) == round(lng, digits = 6) & round(df$lat, digits = 6) == round(lat, digits = 6), ]
        content <- as.character(selectedId$taxid)
        content <- paste(content, ",", aes[[as.numeric(group)]]$popup, ":", selectedId[[aes[[as.numeric(group)]]$popup]])
        leafletProxy("mymap") |> leaflet::addPopups(lng, lat, content)
      }
    }

    # when clicking on a marker, show a popup
    shiny::observe({
      leafletProxy("mymap") |> leaflet::clearPopups()
      event <- input$mymap_marker_click
      if (is.null(event))
        return()

      shiny::isolate({
        showSciName_popup(event$group, event$lng, event$lat)
      })
    })
  }
  shiny::shinyApp(ui, server)
}
