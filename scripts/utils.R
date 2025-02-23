# App Theme ----
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

url_process <- function(tif_url) {
  resp <- list()
  p <- function(prev = NULL) {
    content <- jsonlite::fromJSON(paste(tif_url, prev, sep = "/")) |> data.table::setDT()
    fcontent <- content[!type %in% "directory", list(name, url = paste(tif_url, prev, name, sep = "/"))]
    fcontent <- labelf(fcontent)
    if (nrow(fcontent)) {
      resp[[paste0(prev, "/") |> gsub("^/|/$", "", x = _) |> gsub("/", " - ", x = _) |> gsub("_", " ", x = _)]] <<- fcontent
    }
    for (d in content[type %in% "directory"]$name) {
      p(prev = paste0(prev, "/", d))
    }
  }
  p()
  return(resp)
}

labelf <- function(fcontent) {
  seasons <- c("wt" = "Winter", "sp" = "Spring", "sm" = "Summer", "at" = "Autumn")
  months <- setNames(month.name, sprintf("%02d", 1:12))
  climatevars <- c(
    "Tave" = "mean temperatures (°C)",
    "Tmax" = "maximum mean temperatures (°C)",
    "Tmin" = "minimum mean temperatures (°C)",
    "PPT" = "precipitation (mm)",
    "Rad" = "solar radiation (MJ m-2 d-1)",
    "MAT" = "mean annual temperature (°C)",
    "MWMT" = "mean warmest month temperature (°C)",
    "MCMT" = "mean coldest month temperature (°C)",
    "TD" = "temperature difference between MWMT and MCMT, or continentality (°C)",
    "MAP" = "mean annual precipitation (mm)",
    "MSP" = "mean annual summer (May to Sept.) precipitation (mm)",
    "AHM" = "annual heat-moisture index (MAT+10)/(MAP/1000))",
    "SHM" = "summer heat-moisture index ((MWMT)/(MSP/1000))",
    "DD_0" = "degree-days below 0°C, chilling degree-days",
    "DDsub0" = "degree-days below 0°C, chilling degree-days",
    "DD5" = "degree-days above 5°C, growing degree-days",
    "DD_18" = "degree-days below 18°C, heating degree-days",
    "DDsub18" = "degree-days below 18°C, heating degree-days",
    "DD18" = "degree-days above 18°C, cooling degree-days",
    "NFFD" = "the number of frost-free days",
    "FFP" = "frost-free period",
    "bFFP" = "Day of the year on which the Frost-Free Period begins",
    "eFFP" = "Day of the year on which the Frost-Free Period ends",
    "PAS" = "precipitation as snow (mm)",
    "PET" = "Potential Evapotranspiration",
    "EMT" = "extreme minimum temperature over 30 years (°C)",
    "EXT" = "extreme maximum temperature over 30 years (°C)",
    "CMD" = "Hargreaves climatic moisture deficit (mm)",
    "CMI" = "Hogg’s climate moisture index (mm)",
    "DD1040" = "degree-days above 10°C and below 40°C",
    "Eref" = "Hargreaves reference evaporation (mm)",
    "RH" = "mean relative humidity (%)",
    "elev" = "North America Elevation CEC 2023",
    "lat" = "Latitude WSG 84"
  )
  nm <- fcontent$name
  lbl <- basename(nm) |> tools::file_path_sans_ext()
  season_idx <- grep(paste0("_", names(seasons), "$", collapse = "|"), lbl)
  monthly_idx <- grep(paste0("_?", names(months), "$", collapse = "|"), lbl)
  annual_idx <- setdiff(seq_along(lbl), c(season_idx, monthly_idx))
  resp <- data.table::data.table(
    name = c(
      fcontent$name[annual_idx],
      fcontent$name[season_idx],
      fcontent$name[monthly_idx]
    ),
    url = c(
      fcontent$url[annual_idx],
      fcontent$url[season_idx],
      fcontent$url[monthly_idx]
    ),
    label = c(
      climatevars[lbl[annual_idx]],
      {
        s1 <- strsplit(
          lbl[season_idx],
          paste0("_", names(seasons), "$", collapse = "|")
        ) |> unlist()
        s2 <- strsplit(
          lbl[season_idx],
          paste0("^", unique(s1), "_", collapse = "|")
        ) |> lapply(tail, 1) |> unlist()
        paste(climatevars[s1], seasons[s2], sep = " - ")
      },
      {
        s1 <- strsplit(
          lbl[monthly_idx],
          paste0("_?", names(months), "$", collapse = "|")
        ) |> unlist()
        s2 <- strsplit(
          lbl[monthly_idx],
          paste0("^", unique(s1), "_?", collapse = "|")
        ) |> lapply(tail, 1) |> unlist()
        paste(climatevars[s1], months[s2], sep = " - ")
      }
    ),
    temporality = c(
      rep("Annual", length(annual_idx)),
      rep("Seasonal", length(season_idx)),
      rep("Monthly", length(monthly_idx))
    )
  )
  return(resp)
}

# Tiles source
climr_tif <- url_process(Sys.getenv("CLIMR_TIF_URL"))

# Map tiles provider for BGC + vector tiles ----

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

add_climate_na <- function(map) {

}

add_custom_render <- function(map) {
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

      var map = this;

      var updateOpacity=function(message) {
        var prefixedLayerId = map.layerManager._layerIdKey(message.category, message.layerId);
        var layer = map.layerManager._byLayerId[prefixedLayerId];
        if (layer.setOpacity) {
          layer.setOpacity(message.opacity);
        }
      }

      var updateResolution=function(message) {
        var prefixedLayerId = map.layerManager._layerIdKey(message.category, message.layerId);
        var layer = map.layerManager._byLayerId[prefixedLayerId];
        const resolution = message.resolution;
        layer.options.resolution = resolution;
        layer.redraw();
      }

      var updateClimatePalette=function(message) {
        var prefixedLayerId = map.layerManager._layerIdKey(message.category, message.layerId);
        var colorOptions = message.colorOptions;
        var layer = map.layerManager._byLayerId[prefixedLayerId];
        var georaster = layer.options.georaster;

        const cols = colorOptions.palette;
        let scale = chroma.scale(cols);
        let domain = [georaster.mins[0], georaster.maxs[0]];
        let nacol = colorOptions["na.color"];
        pixelValuesToColorFn = values => {
          let vals;
          vals = values[0];
          let clr = scale.domain(domain);
          if (isNaN(vals) || vals === georaster.noDataValue) return nacol;
          return clr(vals).hex();
        };
        layer.options.pixelValuesToColorFn = pixelValuesToColorFn;
        layer.redraw();
      }

      Shiny.addCustomMessageHandler(\'updateOpacity\', updateOpacity);
      Shiny.addCustomMessageHandler(\'updateResolution\', updateResolution);
      Shiny.addCustomMessageHandler(\'updateClimatePalette\', updateClimatePalette);

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

report_msg <- function(msgs, type = c("info", "danger")) {

  type <- match.arg(type)

  hd <- c(
    "info" = "Climr info:",
    "danger" = "Climr encountered problems:"
  )

  msgs_html <- tags$div(
    class = "alert alert-%s" |> sprintf(type),
    tags$h4(class = "alert-heading", hd[type]),
    tags$ul(
      lapply(msgs, function(msg) {
        tags$li(msg)
      })
    )
  )  
  # Show modal with problems
  shiny::showModal(
    shiny::modalDialog(
      msgs_html,
      size = "xl",
      fade = FALSE,
      easyClose = TRUE
  ))

}
