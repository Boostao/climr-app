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
      gdt$action <- vapply(gdt$id, \(i) {
        shiny::tagList(
          shiny::actionLink(
            "sg_view_%s" |> sprintf(i),
            "View [\U1F5FA\UFE0F]",
            onclick = 'Shiny.setInputValue(\"sg_view\", %s, {priority: \"event\"})' |> sprintf(i)
          ),
          if (sg[id == i, group == "marker" & source == "map_click"]) {
            shiny::actionLink(
              "sg_bivariate_%s" |> sprintf(i),
              "Bivariate [\U1F4CA]",
              onclick = 'Shiny.setInputValue(\"sg_bivariate\", %s, {priority: \"event\"})' |> sprintf(i)
            )
          },
          if (sg[id == i, group == "marker" & source == "map_click"]) {
            shiny::actionLink(
              "sg_timeseries_%s" |> sprintf(i),
              "Timeseries [\U1F4C8]",
              onclick = 'Shiny.setInputValue(\"sg_timeseries\", %s, {priority: \"event\"})' |> sprintf(i)
            )
          },
          shiny::actionLink(
            "sg_remove_%s" |> sprintf(i),
            "Remove [\U274C]",
            onclick = 'Shiny.setInputValue(\"sg_remove\", %s, {priority: \"event\"})' |> sprintf(i)
          )
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
        easyClose =  TRUE, 
        leaflet::renderLeaflet(m)
      )
    )
  }

  modal_bivariate <- function(wkt) {
    shiny::showModal(
      shiny::modalDialog(size = "xl",
        shiny::tabsetPanel(
          shiny::tabPanel("Parameters",
            shiny::div(
              title = "Climate variables for x axis.",
              shiny::selectizeInput(
                inputId = "bivariate_xvars",
                label = "Climate variables X Axis",
                width = "100%",
                choices = c(downscale_extra_vars, list("Core" = downscale_core_vars)),
                multiple = FALSE,
                selected = "Tave_sm"
              ),
              shiny::selectizeInput(
                inputId = "bivariate_yvars",
                label = "Climate variables Y Axis",
                width = "100%",
                choices = c(downscale_extra_vars, list("Core" = downscale_core_vars)),
                multiple = FALSE,
                selected = "PPT_sm"
              ),
              shiny::div(
                title = "20-year reference periods for GCM simulations.",
                shiny::selectInput(
                  inputId = "bivariate_gcm_periods",
                  label = "General Circulation Model (GCM) Periods",
                  width = "100%",
                  choices = climr::list_gcm_periods() |> sn(),
                  multiple = TRUE,
                  selected = climr::list_gcm_periods()[1]
                )
              ),
              shiny::div(
                title = "Global climate models to downscale. Select multiple GCMs for ensemble outputs.",
                shiny::selectInput(
                  inputId = "bivariate_gcms",
                  label = "Global climate model",
                  width = "100%",
                  choices = climr::list_gcms() |> sn(),
                  multiple = TRUE,
                  selected = climr::list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)]
                )
              ),
              shiny::div(
                title = "SSP-RCP scenarios pairing shared socioeconomic pathways with representative concentration pathways.",
                shiny::selectInput(
                  inputId = "bivariate_ssps",
                  label = "Shared Socio-economic Pathways (SSP) - Representative Concentration Pathways (RCP) Scenarios",
                  width = "100%",
                  choices = climr::list_ssps() |> sn(),
                  multiple = TRUE,
                  selected = climr::list_ssps()[2]
                )
              ),
            )
          ),
          shiny::tabPanel("Bivariate Plot",
            plotly::plotlyOutput("bivariate_plot", height = "600px")
          ),
          shiny::tabPanel("Description",
            shiny::div(
              style = "margin-top: 20px;",
              shiny::p("Bivariate plots showing 21st century climate change for user-selected locations and climate variables."),
              shiny::p("Purposes of the plot:"),
              shiny::tags$ol(
                shiny::tags$li("Show differences in climate change trends among global climate models (GCMs)"),
                shiny::tags$li("Show the differences between multiple simulations of each model"),
                shiny::tags$li("Compare simulated climate change to observed climate change in the 2001-2020 period")
              ),
              shiny::p("All climate changes are relative to the 1961-1990 reference period normals.")
            )
          )
        )
      )
    )
    output$bivariate_plot <- plotly::renderPlotly({
      g <- terra::vect(wkt, crs = "EPSG:4326")
      coords <- terra::crds(g)
      elevs <- terra::extract(cec, g, method = "bilinear", ID = FALSE, raw = TRUE)[,1]
      xyz <- data.table::data.table(
        id = 1,
        lon = coords[, 1],
        lat = coords[, 2],
        elev = elevs
      )
      climr::plot_bivariate_db(
        xyz = xyz,
        xvar = input$bivariate_xvars,
        yvar = input$bivariate_yvars,
        period_focal = input$bivariate_gcm_periods,
        gcms = input$bivariate_gcms,
        ssp = input$bivariate_ssps,
        interactive = TRUE
      )
    })
  }

  modal_timeseries <- function(wkt) {
    shiny::showModal(
      shiny::modalDialog(size = "xl",
        shiny::tabsetPanel(
          shiny::tabPanel("Parameters",
            shiny::div(
              title = "Climate variables.",
              shiny::selectizeInput(
                inputId = "timeseries_vars",
                label = "Climate variables",
                width = "100%",
                choices = c(downscale_extra_vars, list("Core" = downscale_core_vars)),
                multiple = FALSE,
                selected = "Tmin_sm"
              ),
              shiny::div(
                title = "Dataset for observational time series data. Options: 'climatena' for ClimateNA gridded time series, 'cru.gpcc' for CRU TS (temperature) and GPCC (precipitation), or 'Null' for none.",
                shiny::selectInput(
                  inputId = "timeseries_obs_ts_dataset",
                  label = "Observation time-series data",
                  width = "100%",
                  choices = c("ClimateNA" = "climatena", "Climatic Research Unit / Global Precipitation Climatology Centre" = "cru.gpcc"),
                  selected = "climatena",
                )
              ),
              shiny::div(
                title = "Global climate models to downscale. Select multiple GCMs for ensemble outputs.",
                shiny::selectInput(
                  inputId = "timeseries_gcms",
                  label = "Global climate model",
                  width = "100%",
                  choices = climr::list_gcms() |> sn(),
                  multiple = TRUE,
                  selected = list_gcms()[c(1)]
                )
              ),
              shiny::div(
                title = "SSP-RCP scenarios pairing shared socioeconomic pathways with representative concentration pathways.",
                shiny::selectInput(
                  inputId = "timeseries_ssps",
                  label = "Shared Socio-economic Pathways (SSP) - Representative Concentration Pathways (RCP) Scenarios",
                  width = "100%",
                  choices = climr::list_ssps() |> sn(),
                  multiple = TRUE,
                  selected = list_ssps()[1]
                )
              ),
            )
          ),
          shiny::tabPanel("Timeseries Plot",
            shiny::plotOutput("timeseries_plot", height = "600px")
          ),
          shiny::tabPanel("Description",
            shiny::div(
              style = "margin-top: 20px;",
              shiny::p("Time series plots of 20th and 21st century climate change for user-selected locations and climate variables."),
              shiny::p("Purposes of the plot:"),
              shiny::tags$ul(
                shiny::tags$li("View differences in interannual variability and climate change trends among global climate models (GCMs)"),
                shiny::tags$li("View the differences between multiple simulations of each model"),
                shiny::tags$li("Compare simulated and observed climate change from 1901 to present"),
                shiny::tags$li("Compare time series of two different variables")
              ),
              shiny::p("All global climate model anomalies are bias-corrected to the 1961-1990 reference period normals.")
            )
          )
        )
      )
    )
    output$timeseries_plot <- shiny::renderPlot({
      g <- terra::vect(wkt, crs = "EPSG:4326")
      coords <- terra::crds(g)
      elevs <- terra::extract(cec, g, method = "bilinear", ID = FALSE, raw = TRUE)[,1]
      xyz <- data.table::data.table(
        id = 1,
        lon = coords[, 1],
        lat = coords[, 2],
        elev = elevs
      )
      data <- climr::plot_timeSeries_input_db(
        xyz = xyz,
        gcms = input$timeseries_gcms,
        ssps = input$timeseries_ssps,
        obs_ts_dataset = input$timeseries_obs_ts_dataset,
        vars = input$timeseries_vars,
      )
      climr::plot_timeSeries(
        X = data,
        var1 = input$timeseries_vars,
        obs_ts_dataset = input$timeseries_obs_ts_dataset,
        gcms = input$bivariate_gcms,
        ssps = input$bivariate_ssps        
      )
    })    
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

  plot_bivariate <- function(rid) {
    modal_bivariate(sg[id == rid][["wkt"]])
  }

  plot_timeseries <- function(rid) {
    modal_timeseries(sg[id == rid][["wkt"]])
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

          if (!length(output_files)) {
            vstore[["processing"]] <- FALSE
            shiny::updateActionButton(inputId = "downscale_process", disabled = FALSE)
            shiny::removeModal()
            shiny::showNotification("No output generated.", type = "warning")
            return()
          }

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
      shiny::removeModal()
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
    bivariate = function(rid) {
      plot_bivariate(rid)
    },
    timeseries = function(rid) {
      plot_timeseries(rid)
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