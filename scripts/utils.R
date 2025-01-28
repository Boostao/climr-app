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

# Map tiles provider for BGC + vector tiles ----

climatena_tileserver <- "http://143.198.35.16:8080"
indexcna <- jsonlite::fromJSON("%s/index.json" |> sprintf(climatena_tileserver), simplifyDataFrame = FALSE)
climatena <- climatelabels <- vapply(indexcna, `[[`, character(1), "name")
names(indexcna) <- climatena
season <- c("wt" = " - Winter", "sp" = " - Spring", "sm" = " - Summer", "at" = " - Autumn")
month <- setNames(paste0(" - ", month.name[1:12]), sprintf("%02d", 1:12))
climatevar <- c(
  "AHM" = "annual heat-moisture index (MAT+10)/(MAP/1000))",
  "bFFP" = "Day of the year on which the Frost-Free Period begins",
  "CMD" = "Hargreaves climatic moisture deficit (mm)",
  "CMI" = "Hogg’s climate moisture index (mm)",
  "DD_0" = "degree-days below 0°C, chilling degree-days",
  "DD_18" = "degree-days below 18°C, heating degree-days",
  "DD18" = "degree-days above 18°C, cooling degree-days",
  "DD1040" = "degree-days above 10°C and below 40°C",
  "DD5" = "degree-days above 5°C, growing degree-days",
  "eFFP" = "Day of the year on which the Frost-Free Period ends",
  "EMT" = "extreme minimum temperature over 30 years (°C)",
  "Eref" = "Hargreaves reference evaporation (mm)",
  "FFP" = "frost-free period",
  "MAP" = "mean annual precipitation (mm)",
  "MAT" = "mean annual temperature (°C)",
  "MCMT" = "mean coldest month temperature (°C)",
  "MSP" = "mean annual summer (May to Sept.) precipitation (mm)",
  "MWMT" = "mean warmest month temperature (°C)",
  "NFFD" = "the number of frost-free days",
  "PAS" = "precipitation as snow (mm).",
  "PPT" = "precipitation (mm)",
  "Rad" = "solar radiation (MJ m-2 d-1)",
  "RH" = "mean relative humidity (%)",
  "SHM" = "summer heat-moisture index ((MWMT)/(MSP/1000))",
  "Tave" = "mean temperatures (°C)",
  "TD" = "temperature difference between MWMT and MCMT, or continentality (°C)",
  "Tmax" = "maximum mean temperatures (°C)",
  "Tmin" = "minimum mean temperatures (°C)"
)
for (l in names(climatevar)) { climatelabels <- gsub("^%s" |> sprintf(l), sprintf("Annual - %s", climatevar[l]), climatelabels)}
for (l in names(season)) { climatelabels <- gsub("^Annual([^_]+)_%s$" |> sprintf(l), sprintf("Seasonal\\1%s", season[l]), climatelabels)}
for (l in names(month)) { climatelabels <- gsub("^Annual([^_]+)_?%s$" |> sprintf(l), sprintf("Monthly\\1%s", month[l]), climatelabels)}
climatena <- setNames(climatena, climatelabels)

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