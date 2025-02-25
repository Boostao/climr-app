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
    m <- leaflet::leaflet() |> leaflet::addProviderTiles(provider = leaflet::providers$CartoDB.PositronNoLabels)
    if (g == "marker") {
      m <- m |> leaflet::addAwesomeMarkers(
        data = terra::vect(wkt),
        group = "sg_marker",
        icon = default_icon
      )
    } else if (g == "shape") {
      m <- m |> leaflet::addPolygons(
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
        f0 <- file.path(d0, archive::archive_extract(f0, d0))
        # check if it's multifile archive (bin for raster, shp for polygons)
        if (length(f0) > 1) {
          f0 <- grep("bin$|shp$", f0, value = TRUE, ignore.case = TRUE) |> head(1)
        }
      }

      # Text file upload logic bloc
      if (tolower(tools::file_ext(f0)) %in% c("csv", "txt")) {

        res <- try(data.table::fread(f0), silent = TRUE)
        if (inherits(res, "try-error")) {
          report_msg("Unable to read input file (csv/txt). [data.table::fread(\"%s\")]" |> sprintf(f$name), "danger")
          unlink(d0, recursive = TRUE)
          return()
        }

        nm <- names(res)
        id_j <- head(grep("^id|id$|^site", nm, ignore.case = TRUE), 1)
        lat_j <- head(grep("^lat|latitude", nm, ignore.case = TRUE), 1)
        lng_j <- head(grep("^lng|^long|^lon|longitude", nm, ignore.case = TRUE), 1)
        ele_j <- head(grep("^elev|elevation", nm, ignore.case = TRUE), 1)
        geom_j <- head(grep("^geom|geometry", nm, ignore.case = TRUE), 1)
        
        if (length(lat_j) && length(lng_j)) {
          report_msg("Found columns in file. [%s : %s]" |> sprintf(f$name, paste(nm[id_j], nm[lat_j], nm[lng_j], nm[ele_j], sep = ", ")))
        } else if (length(geom_j)) {
          
        } else {
          report_msg("Column detection could not find latitude and longitude pair in file. [%s]" |> sprintf(f$name), "danger")
          return()
        }

        # Add to file geometries
        fg[[d0]] <<- list(
          "datapath" = f0,
          "type" = "text",
          "object" = res,
          "id_j" = id_j,
          "lat_j" = lat_j,
          "lng_j" = lng_j,
          "ele_j" = ele_j
        )

        new_p <- "MULTIPOINT (%s)" |> sprintf(paste(sprintf("(%s %s)", res[lng_j], res[lat_j]), collapse = ","))
        push(new_p, "marker", "file_upload")
        return()

      }
      
      # raster upload logic
      res <- try(terra::rast(f0), silent = TRUE)
      if (!inherits(res, "try-error")) {

        # Add to file geometries
        fg[[d0]] <<- list(
          "datapath" = f0,
          "type" = "raster",
          "object" = res
        )

        new_p <- terra::ext(res) |>
          terra::project(from = terra::crs(res), to = "EPSG:4326") |> 
          terra::vect("EPSG:4326") |>
          terra::geom(wkt = TRUE)
        push(new_p, "shape", "file_upload")
        return()
      
      }

      # shape upload logic
      res <- try(terra::vect(f0), silent = TRUE)
      if (!inherits(res, "try-error")) {

        # Add to file geometries
        fg[[d0]] <<- list(
          "datapath" = f0,
          "type" = "shape",
          "object" = res
        )

        new_p <- res |>
          terra::project(from = terra::crs(res), to = "EPSG:4326") |> 
          terra::geom(wkt = TRUE)
        push(new_p, "shape", "file_upload")
        return()
      
      }
      
      report_msg("Unable to ingest uploaded file. [%s]" |> sprintf(f$name), "danger")
      return()

    },
    process = function() {
      withCallingHandlers(
        message = function(m) {shiny::showNotification(ui = shiny::span(conditionMessage(m)), type = "message")},
        warning = function(w) {shiny::showNotification(ui = shiny::span(conditionMessage(w)), type = "warning")},
        tryCatch(
          {
            n <- \(x) if (length(x) && !"NULL" %in% x) x
            res <- climr::downscale(
              xyz = xyz,
              which_refmap = vstore[["downscale_which_refmap"]],
              obs_periods = vstore[["downscale_obs_periods"]] |> n(),
              obs_years  = vstore[["downscale_obs_years "]] |> n(),
              obs_ts_dataset = vstore[["downscale_obs_ts_dataset"]] |> n(),
              gcsm = vstore[["downscale_gcsm"]] |> n(),
              ssps = vstore[["downscale_ssps"]] |> n(),
              gcm_periods = vstore[["downscale_gcm_periods"]] |> n(),
              gcm_ssp_years = vstore[["downscale_gcm_ssp_years"]] |> n(),
              gcm_hist_years = vstore[["downscale_gcm_hist_years"]] |> n(),
              max_run = vstore[["downscale_max_run"]] |> n(),
              run_nm = vstore[["downscale_run_nm"]] |> n(),
              core_vars = vstore[["downscale_core_vars"]] |> n(),
              core_ppt_lr = vstore[["downscale_core_ppt_lr"]],
              cache = FALSE
            )
            output$downscale_download <<- shiny::downloadHandler(
              filename = function() tempfile("downscale_", fileext = ".zip"),
              content = function(file) {},
              contentType = "application/zip"
            )
          },
          error = function(e) {
            report_msg(conditionMessage(e), type = "danger")
          }
        )
      )
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
    }
  )
  return(sg_methods)
}