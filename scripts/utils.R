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
      fcontent$name[monthly_idx],
      fcontent$name[season_idx],
      fcontent$name[annual_idx]
    ),
    url = c(
      fcontent$url[monthly_idx],
      fcontent$url[season_idx],
      fcontent$url[annual_idx]
    ),
    label = c(
      {
        s1 <- strsplit(
          lbl[monthly_idx],
          paste0("_?", names(months), "$", collapse = "|")
        ) |> unlist()
        climatevars[s1]
      },
      {
        s1 <- strsplit(
          lbl[season_idx],
          paste0("_", names(seasons), "$", collapse = "|")
        ) |> unlist()
        climatevars[s1]
      },
      climatevars[lbl[annual_idx]]
    ),
    element = c(
      strsplit(
        lbl[monthly_idx],
        paste0("_?", names(months), "$", collapse = "|")
      ) |> unlist(),
      strsplit(
        lbl[season_idx],
        paste0("_", names(seasons), "$", collapse = "|")
      ) |> unlist(),
      lbl[annual_idx]
    ),
    time_code = c(
      substr(lbl[monthly_idx], nchar(lbl[monthly_idx]) - 1, nchar(lbl[monthly_idx])),
      substr(lbl[season_idx], nchar(lbl[season_idx]) - 1, nchar(lbl[season_idx])),
      rep("aa", length(annual_idx))
    ),
    category = c(
      c("Derived elements","Basic elements")[grepl("^PPT|^Tmin|^Tmax", lbl[monthly_idx])+1],
      c("Derived elements","Basic elements")[grepl("^PPT|^Tmin|^Tmax", lbl[season_idx])+1],
      c("Annual elements","Basic elements")[grepl("^PPT|^Tmin|^Tmax", lbl[annual_idx])+1]
    )
  )
  data.table::set(resp, j = "label", value = resp[, "(%s) %s" |> sprintf(element, label)])
  return(resp)
}

time_labels_season <- c(
  "Annual" = "",
  "Winter" = "wt",
  "Spring" = "sp",
  "Summer" = "sm",
  "Autumn" = "at"
)
time_labels_month <- c(
  "January" = "01",
  "February" = "02",
  "March" = "03",
  "April" = "04",
  "May" = "05",
  "June" = "06",
  "July" = "07",
  "August" = "08",
  "September" = "09",
  "October" = "10",
  "November" = "11",
  "December" = "12"
)

# Tiles source
climr_tif <- url_process(Sys.getenv("CLIMR_TIF_URL"))
climr_ratios <- climr::variables[Type %in% "ratio", c(Code, Code_ClimateNA) |> unique() |> sort()]

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
          maxZoom : 25,
          maxNativeZoom : 17,
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
        if (layer !== undefined) {
          layer.setOpacity(message.opacity);
        }
      }

      var updateResolution=function(message) {
        var prefixedLayerId = map.layerManager._layerIdKey(message.category, message.layerId);
        var layer = map.layerManager._byLayerId[prefixedLayerId];
        if (layer !== undefined) {
          const resolution = message.resolution;
          layer.options.resolution = resolution;
          layer.redraw();
        }
      }

      var updateClimatePalette=function(message) {
        var prefixedLayerId = map.layerManager._layerIdKey(message.category, message.layerId);
        var layer = map.layerManager._byLayerId[prefixedLayerId];
        if (layer !== undefined) {
            var georaster = layer.options.georaster;
            var colorOptions = message.colorOptions;
            var scaleFunc = ({log: Math.log, log10: Math.log10, log1p: Math.log1p, log2: Math.log2}[message.vscale] || (x => x));
            const cols = colorOptions.palette;
            let scale = chroma.scale(cols);
            let dmin = scaleFunc(georaster.mins[0]);
            if (dmin === -Infinity || isNaN(dmin)) {
              console.log(message.vscale);
              console.log(georaster.mins[0]);
              console.log(dmin);
            }
            let dmax = scaleFunc(georaster.maxs[0]);
            let domain = [dmin, dmax];
            let nacol = colorOptions["na.color"];
            let clr = scale.domain(domain);
            pixelValuesToColorFn = values => {
                let vals = values[0];
                if (isNaN(vals) || vals === georaster.noDataValue) return nacol;
                let processedVals = scaleFunc(vals);
                return clr(processedVals).hex();
            };
            layer.updateColors(pixelValuesToColorFn);
        }
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

  if (!shiny::devmode() & type %in% "danger") {
    msgs <- "Error with climr: file issue at http://www.github.com/bcgov/climr-app"
  }

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

# Function to generate random run number with compressed timestamp
generate_run_id <- function() {
  # Define character set (A-Z, 0-9)
  chars <- c(65:90, 48:57) # ASCII codes for A-Z and 0-9
  # Sample 8 random characters and convert to string
  run_num <- rawToChar(as.raw(sample(chars, 8, replace = TRUE)))
  # Get compressed timestamp (YYYYMMDDHHMM)
  timestamp <- format(Sys.time(), "%Y%m%d%H%M")
  # Combine with underscore
  paste0(run_num, "_", timestamp)
}

# Albers Equal Area CRS (meters-based)
albers_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m"

create_points_dt <- function(sg, cec, resolution) {

  # Initialize output data.table
  out_dt <- data.table(sg_id = integer(), id = integer(), lon = numeric(), lat = numeric(), elev = numeric())

  # Split indices by group
  marker_idx <- which(sg$group == "marker")
  shape_idx <- which(sg$group == "shape")
  hull <- NULL
  
  # Process markers
  if (length(marker_idx) > 0) {
    marker_geoms <- terra::vect(sg$wkt[marker_idx], crs = "EPSG:4326")
    coords <- terra::crds(marker_geoms)
    elevs <- terra::extract(cec, marker_geoms, method = "bilinear", ID = FALSE, raw = TRUE)[,1]
    
    marker_dt <- data.table(
      sg_id = sg$id[marker_idx],
      id = 1,
      lon = coords[, 1],
      lat = coords[, 2],
      elev = elevs
    )
    hull <- terra::convHull(marker_geoms)
  } else {
    marker_dt <- data.table::data.table()
  }
  
  # Process shapes
  if (length(shape_idx) > 0) {
    shape_geoms <- terra::vect(sg$wkt[shape_idx], crs = "EPSG:4326")
    shape_geoms_albers <- terra::project(shape_geoms, albers_crs)
    
    # Generate grid for each shape in Albers (meters)
    grid_list <- lapply(seq_along(shape_idx), function(i) {
      extents_albers <- terra::ext(shape_geoms_albers[i])
      xmin <- extents_albers$xmin
      xmax <- extents_albers$xmax
      ymin <- extents_albers$ymin
      ymax <- extents_albers$ymax
      x_seq <- seq(xmin, xmax, by = c(1,-1)[(xmax < xmin) + 1] * resolution)
      y_seq <- seq(ymin, ymax, by = c(1,-1)[(ymax < ymin) + 1] * resolution)
      grid_dt <- expand.grid(x = x_seq, y = y_seq)
      # Convert to SpatVector in Albers
      grid_vect_albers <- terra::vect(as.matrix(grid_dt[, c("x", "y")]), crs = albers_crs)
      # Check which points are within their respective polygons
      pts_within <- terra::is.related(grid_vect_albers, shape_geoms_albers[i], "within")
      grid_valid_albers <- grid_vect_albers[which(pts_within)]
      if (length(grid_valid_albers) > 0) {
        # Convert back to WGS84 (EPSG:4326)
        grid_vect_wgs84 <- terra::project(grid_valid_albers, "EPSG:4326")
        coords_wgs84 <- terra::crds(grid_vect_wgs84)
        # Extract elevations
        elevs <- terra::extract(cec, grid_vect_wgs84, method = "bilinear", ID = FALSE, raw = TRUE)[,1]
        # Create output for shapes
        shape_dt <- data.table(
          sg_id = sg$id[shape_idx[i]],
          id = 1,
          lon = coords_wgs84[, 1],
          lat = coords_wgs84[, 2],
          elev = elevs
        )
        return(shape_dt)
      }
      return(data.table::data.table())
    })

    hull <- if (is.null(hull)) {
      terra::convHull(shape_geoms)
    } else {
      if (length(marker_idx) == 1) {
        hull <- terra::buffer(hull, 0.001, quadsegs = 1, capstyle = "square")
      }
      terra::union(hull, terra::convHull(shape_geoms)) |> terra::convHull()
    }

  } else {
    grid_list <- list(data.table::data.table())
  }

  # Combine all grids and track original shape index
  out_dt <- data.table::rbindlist(c(list(out_dt, marker_dt), grid_list), use.names = TRUE)
  data.table::set(out_dt, j = "id", value = seq_len(nrow(out_dt)))
  if (!is.null(hull)) {
    attr(out_dt, "hull") <- hull |> terra::geom(wkt = TRUE)
  }
  cat(attr(out_dt, "hull"), sep = "\n")
  return(out_dt)
}
