bcgov_theme <- function(action = c("install","remove")) {
  action <- match.arg(action)

  # Injecting bcgov theme directly into bslib library
  target <- find.package("bslib")
  if (file.access(target,2) < 0) {
    stop("This must be run with write access to the bslib package")
  }

  src <- "./"
  f <- dir(, recursive = TRUE) |> grep("^fonts|^lib", x = _, value = TRUE)

  if (action == "install") {
    lapply(file.path(target, unique(dirname(f))), dir.create, showWarnings = FALSE, recursive = TRUE)
    file.copy(file.path(src, f), file.path(target, f))
  }

  if (action == "remove") {
    unlink(file.path(target, f))
    unlink(file.path(target, "lib/bsw5/dist/bcgov"), recursive = TRUE)
  }

  return(invisible())

}

if (!"bcgov" %in% bslib::bootswatch_themes()) {
  bcgov_theme("install")
}

latlontopoly <- function(latlon) {
  matrix(latlon, ncol = 2, byrow = TRUE) |>
    as.data.frame() |>
    sf::st_as_sf(coords = c(2,1), crs = sf::st_crs(4326)) |>
    sf::st_combine() |>
    sf::st_cast("POLYGON")
}

boundstopoly <- function(b) {
  if (abs(b$west) > 180 | abs(b$east) > 180 | abs(b$north) > 90 | abs(b$south) > 90) return(NULL)
  res <- sf::st_polygon(list(rbind(c(b$west, b$north),c(b$east, b$north),c(b$east, b$south),c(b$west, b$south),c(b$west, b$north)))) |>
    sf::st_sfc()
  res <- sf::st_set_crs(res, sf::st_crs(4326))
  res
}

##javascript source
wna_tileserver <- "https://tileserver.thebeczone.ca/data/WNA_MAP/{z}/{x}/{y}.pbf"
wna_tilelayer <- "WNA_MAP"

plugins <- {
  list(
    vgplugin =
      htmltools::htmlDependency(
        name = "leaflet.vectorgrid",
        version = "1.3.0",
        src = "www/htmlwidgets",
        script = "lfx-vgrid-prod.js"
      )
  )
}

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

add_wna <- function(map) {
  subzones_colours_ref <- data.table::fread("data/WNAv12_3_SubzoneCols.csv", key = "classification")
  map <- registerPlugin(map, plugins$vgplugin)
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$classification, "':'", subzones_colours_ref$colour,"'", collapse = ","), "}"), '
      
      var vectorTileOptions=function(layerName, layerId, activ,
                             lfPane, colorMap, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: activ, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fill: true,
                fillOpacity: 0.3
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
              return f.properties[id];
          }
        }
        
      };
      
      var subzLayer = L.vectorGrid.protobuf(
        "', wna_tileserver, '",
        vectorTileOptions("WNA BEC", "', wna_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
      )
      this.layerManager.addLayer(subzLayer, "tile", "WNA BEC", "WNA BEC");
      
      subzLayer.bindTooltip(function(e) {
        return e.properties.MAP_LABEL
      }, {sticky: true, textsize: "10px", opacity: 1});
      subzLayer.bringToFront();
    }'
  ))
  map
}

default_draw_tool <- function(mp) {
  mp |> leaflet.extras::addDrawToolbar(
    position = "bottomleft",
    polylineOptions = FALSE,
    circleMarkerOptions = FALSE,
    markerOptions = FALSE
  )
}

default_icon <- leaflet::makeAwesomeIcon("record", markerColor = "darkblue", iconColor = "#fcba19")

session_geometry <- function(mp) {

  sg <- tibble::tibble(
    id = integer(),
    wkt = character(),
    group = character(),
    source = character()
  )

  del_popup <- function(id) {
    shiny::actionButton(
      "sg_remove_%s" |> sprintf(id),
      "Remove",
      class = "btn btn-danger action-button",
      onclick = 'Shiny.setInputValue(\"sg_remove\", %s, {priority: \"event\"})' |> sprintf(id)
    ) |> 
      as.character()
  }
  
  update_map_marker <- function(sg) {
    fg <- sg |> dplyr::filter(group == "sg_marker")
    mp |> leaflet::clearGroup("sg_marker")
    if (nrow(fg)) {
      d <- terra::vect()
      mp |>leaflet::addAwesomeMarkers(
        data = terra::vect(fg[["wkt"]]),
        group = "sg_marker",
        popup = lapply(fg[["id"]], del_popup),
        icon = default_icon
      )
    }
  }

  update_map_shape <- function(sg) {
    fg <- sg |> dplyr::filter(group == "sg_shape")
    mp |> leaflet::clearGroup("sg_shape") |>
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) |>
      default_draw_tool()
    if (nrow(fg)) {
      d <- terra::vect()
      mp |>leaflet::addPolygons(
        data = terra::vect(fg[["wkt"]]),
        group = "sg_shape",
        popup = lapply(fg[["id"]], del_popup),
        fillColor = "#fcba19",
        color = "#036",
        opacity = 0.8,
        weight = 2
      )
    }
  }

  push <- function(new, l, s) {
    rbind(sg, tibble::tibble(id = max(c(sg$id,0)) + 1L, wkt = new, group = l, source = s))
  }

  rem <- function(rid) {
    g <- dplyr::filter(sg, id == rid)[["group"]]
    sg <<- dplyr::filter(sg, id != rid)
    switch(g,
      "sg_marker" = update_map_marker(sg),
      "sg_shape" = update_map_shape(sg)
    )
  }

  sg_methods <- list(
    add_point = function(lat,lng) {
       new_p <- "POINT (%s %s)" |> sprintf(lng, lat)
       sg <<- push(new_p, "sg_marker", "map_click")
       update_map_marker(sg)
    },
    add_draw_poly = function(poly) {
      ft <- poly[["properties"]][["feature_type"]]
      if (ft %in% c("polygon","rectangle")) {
        new_p <- paste0("POLYGON ((", paste(lapply(poly[["geometry"]][["coordinates"]][[1]], \(x) unlist(x) |> paste(collapse = " ")), collapse = ","), "))")
      } else if (ft == "circle") {
        new_p <- do.call(sprintf, c("POINT (%s %s)", poly$geometry$coordinates)) |>
          terra::vect(crs = "EPSG:4326") |>
          terra::buffer(poly$properties$radius) |>
          terra::geom(wkt = TRUE)
      }
      sg <<- push(new_p, "sg_shape", "map_draw")
      update_map_shape(sg)
    },
    add_rast = function(r) {

    },
    add_file = function(f) {

    },
    get = function() {
      return(sg)
    },
    rm = function(rid) {
      rem(rid)
    }
  )
  return(sg_methods)
}