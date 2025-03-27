# Geometry input logic ----
session_geometry <- function() {
  
  sg <- data.table::data.table(
    id = integer(),
    wkt = character(),
    group = character(),
    source = character(),
    datapath = character()
  )

  fg <- list()

  mp <- leaflet::leafletProxy("climr")
  
  rem_popup <- function(id) {
    shiny::actionButton(
      "sg_remove_%s" |> sprintf(id),
      "Remove:%s" |> sprintf(id),
      class = "btn btn-sm btn-danger action-button",
      onclick = 'Shiny.setInputValue(\"sg_remove\", %s, {priority: \"event\"})' |> sprintf(id)
    ) |> 
      as.character()
  }

  refresh_DT <- function() {
    output$geom_dt <<- DT::renderDT(server = TRUE, {
      gdt <- data.table::copy(sg[,1:4])
      gdt$wkt[nchar(gdt$wkt) > 90] <- paste0(substr(gdt$wkt[nchar(gdt$wkt) > 90], 1, 87), "...")
      gdt$action <- vapply(gdt$id, \(id) {
        shiny::tagList(
          shiny::actionLink(
            "sg_view_%s" |> sprintf(id),
            "View [\U1F5FA\UFE0F]",
            onclick = 'Shiny.setInputValue(\"sg_view\", %s, {priority: \"event\"})' |> sprintf(id)
          ),
          shiny::actionLink(
            "sg_remove_%s" |> sprintf(id),
            "Remove [\U274C]",
            onclick = 'Shiny.setInputValue(\"sg_remove\", %s, {priority: \"event\"})' |> sprintf(id)
          ),
        ) |> as.character()
      }, character(1))
      data.table::setnames(gdt, "wkt", "well-known text")
      data.table::setnames(gdt, tools::toTitleCase(names(gdt)))
      DT::datatable(gdt, rownames = FALSE, escape = FALSE, options = list(
        rowCallback = DT::JS("
          function(row, data, index) {
            if (data[3] === \"map_click\") {
              $(row).addClass(\"table-primary\");
            } else if (data[3] === \"map_draw\") {
              $(row).addClass(\"table-warning\");
            }
          }
        ")
      ))
    })
  }
  refresh_DT()

  update_map_marker <- function() {
    mg <- sg[group == "marker"]
    mp |> leaflet::clearGroup("sg_marker")
    if (nrow(mg)) {
      mp |> leaflet::addAwesomeMarkers(
        data = terra::vect(mg$wkt),
        group = "sg_marker",
        popup = lapply(mg$id, rem_popup),
        icon = default_icon
      )
    }
  }

  update_map_shape <- function() {
    mg <- sg[group == "shape"]
    mp |> leaflet::clearGroup("sg_shape") |>
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE) |>
      default_draw_tool()
    if (nrow(mg)) {
      mp |> leaflet::addPolygons(
        data = terra::vect(mg$wkt),
        group = "sg_shape",
        popup = lapply(mg$id, rem_popup),
        fillColor = "#fcba19",
        color = "#036",
        opacity = 0.8,
        weight = 2
      )
    }
  }

  modal_map <- function(wkt, g) {
    if ("marker" %in% g) {
      m <- mview |> leaflet::addAwesomeMarkers(
        data = terra::vect(wkt),
        group = "sg_marker",
        icon = default_icon
      )
    } else if ("shape" %in% g ) {
      m <- mview |> leaflet::addPolygons(
        data = terra::vect(wkt),
        fillColor = "#fcba19",
        color = "#036",
        opacity = 0.8,
        weight = 2
      )
    }
    shiny::showModal( 
      shiny::modalDialog( 
        title = NULL, 
        easy_close = TRUE, 
        leaflet::renderLeaflet(m)
      )
    )
  }

  refresh <- function(g) {
    refresh_DT()
    shiny::updateActionButton(inputId = "downscale_process", disabled = {nrow(sg) <= 0})
    if ("marker" %in% g) update_map_marker()
    if ("shape" %in% g) update_map_shape()
  }

  push <- function(new, g, s, d = NA_character_) {
    id <- max(c(0L,sg$id))+1L
    sg <<- rbind(sg, data.table::data.table(id = id, wkt = new, group = g, source = s, datapath = d))
    refresh(g)
    session$sendCustomMessage(type="jsCode", list(code = "$('.input-control-body a.shiny-download-link').removeClass('btn-success');"))
  }

  rem <- function(rid) {

    t <- sg[id %in% rid, list(group, source, datapath)]
    
    # Drop datapath from fileuploads if any
    d <- unique(t$datapath)
    d <- d[!is.na(d)]
    if (length(d)) {
      fg[d] <<- NULL
      unlink(d, recursive = TRUE)
    }

    # Refresh geometries
    g <- unique(t$group)
    sg <<- sg[!id %in% rid]
    refresh(g)

  }

  view_map <- function(rid) {
    g <- sg[id == rid, unique(group)]
    modal_map(sg[id == rid][["wkt"]], g)
  }

  click_enabled <- TRUE
  click_ignore_next <- FALSE

  sg_methods <- list(
    add_point = function(lat,lng) {
       if (!click_enabled) return()
       if (click_ignore_next) {click_ignore_next <<- FALSE; return()}
       new_p <- "POINT (%s %s)" |> sprintf(lng, lat)
       push(new_p, "marker", "map_click")
    },
    add_draw_poly = function(poly) {
      ft <- poly$properties$feature_type
      if (ft %in% c("polygon","rectangle")) {
        new_p <- paste0(
          "POLYGON ((",
            paste(
              lapply(
                poly$geometry$coordinates[[1]],
                \(x) unlist(x) |> paste(collapse = " ")
              ),
            collapse = ","),
          "))"
        )
      } else if (ft == "circle") {
        new_p <- do.call(sprintf, c("POINT (%s %s)", poly$geometry$coordinates)) |>
          terra::vect(crs = "EPSG:4326") |>
          terra::buffer(poly$properties$radius) |>
          terra::geom(wkt = TRUE)
      }
      push(new_p, "shape", "map_draw")
      click_ignore_next <<- TRUE
    },
    add_file = function(f) {
      f <- as.list(f)
      f0 <- f$datapath
      d0 <- dirname(f$datapath)
      # does it need unzipping before continuing processing?
      if (tolower(tools::file_ext(f$name)) %in% c("zip","tar","gz","xz","7z","bz2")) {
        farch <- try(archive::archive_extract(f0, d0), silent = TRUE)
        if (inherits(farch, "try-error")) {
          shiny::showNotification("Unable to read archive.", type = "error")
          unlink(d0, recursive = TRUE)
          return()
        }
        f0 <- file.path(d0, farch)
        # check if it's multifile archive (bin for raster, shp for polygons)
        if (length(f0) > 1) {
          f0 <- grep("bin$|shp$", f0, value = TRUE, ignore.case = TRUE) |> head(1)
        }
      }

      # Text file upload logic bloc
      if (tolower(tools::file_ext(f0)) %in% c("csv", "txt")) {

        res <- try(data.table::fread(f0), silent = TRUE)
        if (inherits(res, "try-error")) {
          shiny::showNotification("Unable to read input file (csv/txt). [data.table::fread(\"%s\")]" |> sprintf(f$name), type = "error")
          unlink(d0, recursive = TRUE)
          return()
        }

        nm <- names(res)
        geom_j <- head(grep("^geom|geometry", nm, ignore.case = TRUE), 1)
        elev_j <- head(grep("^elev|elevation", nm, ignore.case = TRUE), 1)
        lon_j <- head(grep("^lng|^long|^lon|longitude", nm, ignore.case = TRUE), 1)
        lat_j <- head(grep("^lat|latitude", nm, ignore.case = TRUE), 1)
        id_j <- head(grep("^id|id$|^site", nm, ignore.case = TRUE), 1)

        if (length(geom_j)) {

          shiny::showNotification("Found columns in file. [%s : %s]" |> sprintf(f$name, paste(nm[id_j], nm[geom_j], sep = ", ")), type = "message")

          shape <- try(terra::vect(res[[geom_j]], crs = "EPSG:4326"), silent = TRUE)
          if (inherits(shape, "try-error")) {
            shiny::showNotification("Unable to read input file geometry column. [pos: %s]" |> sprintf(geom_j), type = "error")
            unlink(d0, recursive = TRUE)
            return()
          }

          # Add to file geometries
          fg[[d0]] <<- list(
            "datapath" = f0,
            "type" = "text",
            "id" = id_j,
            "geom" = geom_j,
            "elev" = elev_j,
            "table" = res[,-geom_j],
            "shape" = shape
          )

          new_p <- shape |>
            terra::aggregate() |>
            terra::geom(wkt = TRUE)

          if (terra::is.points(shape)) {
            push(new_p, "marker", "file_upload", d0)
          } else {
            push(new_p, "shape", "file_upload", d0)
          }

          return()

        } else if (length(lat_j) && length(lon_j)) {
          shiny::showNotification("Found columns in file. [%s : %s]" |> sprintf(f$name, paste(nm[id_j], nm[lat_j], nm[lon_j], nm[elev_j], sep = ", ")), type = "message")
        } else {
          shiny::showNotification("Column detection could not find latitude and longitude pair in file. [%s]" |> sprintf(f$name), type = "error")
          return()
        }

        # Add to file geometries
        fg[[d0]] <<- list(
          "datapath" = f0,
          "type" = "text",
          "id" = id_j,
          "lon" = lon_j,
          "lat" = lat_j,
          "elev" = elev_j,
          "table" = res
        )

        new_p <- "MULTIPOINT (%s)" |> sprintf(paste(sprintf("(%s %s)", res[[lon_j]], res[[lat_j]]), collapse = ","))
        push(new_p, "marker", "file_upload", d0)
        return()

      }
      
      # raster upload logic
      res <- try(terra::rast(f0), silent = TRUE)
      if (!inherits(res, "try-error")) {

        if ("" %in% terra::crs(res)) {
          shiny::showNotification("Could not determine the CRS of the raster. [%s]" |> sprintf(f$name), type = "error")
          return()
        }

        if (!terra::is.lonlat(res)) {
          res <- terra::project(res, from = terra::crs(res), to = "EPSG:4326")
        }

        # Add to file geometries
        fg[[d0]] <<- list(
          "datapath" = f0,
          "type" = "raster",
          "raster" = res
        )
        
        new_p <- res |> 
          terra::ext() |>
          terra::vect() |>
          terra::geom(wkt = TRUE)

        push(new_p, "shape", "raster_upload", d0)
        return()
      
      }

      # shape upload logic
      res <- try(terra::vect(f0), silent = TRUE)
      if (!inherits(res, "try-error")) {

        if ("" %in% terra::crs(res)) {
          shiny::showNotification("Could not determine the CRS of the vector. [%s]" |> sprintf(f$name), type = "error")
          return()
        }

        if (!terra::is.lonlat(res)) {
          res <- terra::project(res, from = terra::crs(res), to = "EPSG:4326")
        }

        # Add to file geometries
        fg[[d0]] <<- list(
          "datapath" = f0,
          "type" = "shape",
          "shape" = res
        )

        new_p <- res |>
          terra::aggregate() |>
          terra::geom(wkt = TRUE)

        push(new_p, "shape", "file_upload", d0)
        return()
      
      }
      
      shiny::showNotification("Unable to ingest uploaded file. [%s]" |> sprintf(f$name), type = "error")
      return()

    },
    process = function() {
      vstore[["processing"]] <- TRUE
      shiny::updateActionButton(inputId = "downscale_process", disabled = TRUE)
      withCallingHandlers(
        message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
        warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
        error = function(e) {shiny::showNotification(ui = shiny::span(conditionMessage(e)), type = "error")},
        {

          run_id <- generate_run_id()

          output_files <- process_downscale(sg, cec, vstore, fg, run_id)

          output$downscale_download <- shiny::downloadHandler(
            filename = function() {
              paste0("downscale_", run_id, ".zip")
            },
            content = function(file) {            
              on.exit(unlink(output_files), add = TRUE)
              zip::zipr(file, output_files)
            },
            contentType = "application/zip"
          )

          session$sendCustomMessage(type="jsCode", list(code = "$('.input-control-body a.shiny-download-link').addClass('btn-success');"))
          shiny::showNotification("Downscale process completed. You can now download the results.", type = "message")
        }
      )
      vstore[["processing"]] <- FALSE
      shiny::updateActionButton(inputId = "downscale_process", disabled = FALSE)
    },
    get = function() {
      return(sg)
    },
    rm = function(rid) {
      rem(rid)
    },
    view = function(rid) {
      view_map(rid)
    },
    add_point_enabled = function(val) {
      if (missing(val)) return(click_enabled)
      else click_enabled <<- val
    },
    approx_count = function(resolution = 2500) {
      marker_idx <- which(sg$group == "marker")
      shape_idx <- which(sg$group == "shape")
    
      approx_pts_shape <- vapply(
        sg[group == "shape", wkt],
        \(x) {
          area <- terra::vect(x, crs = "EPSG:4326") |>
            terra::expanse("m")
          floor(area / (resolution ^ 2))
        },
        FUN.VALUE = numeric(1),
        USE.NAMES = FALSE
      ) |> sum(na.rm = TRUE)
      
      return(
        list(
          marker = length(marker_idx),
          marker_count = length(marker_idx),
          shape = approx_pts_shape,
          shape_count = length(shape_idx)
        )
      )
    }
  )
  return(sg_methods)
}