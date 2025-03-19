
# Setup ----
suppressPackageStartupMessages({
  library(archive)
  library(bslib)
  library(data.table)
  library(DT)
  library(htmltools)
  library(htmlwidgets)
  library(jsonlite)
  library(leafem)
  library(leaflet.extras)
  library(leaflet)
  library(shiny)
  library(terra)
  library(climr)
  library(zip)
  source("scripts/utils.R", local = TRUE)
})

# Shiny options
options(shiny.autoreload = TRUE)
options(shiny.maxRequestSize = 1000 * 1024^2)

# MapBox values
mbtk <- Sys.getenv("BCGOV_MAPBOX_TOKEN")
mblbstyle <- Sys.getenv("BCGOV_MAPBOX_LABELS_STYLE")
mbhsstyle <- Sys.getenv("BCGOV_MAPBOX_HILLSHADE_STYLE")

pals <- readRDS("scripts/pals.rds")

# Elevation raster for missing values
elevtif <- c(Sys.getenv("ELEV_RASTER"), "northamerica_elevation_cec_2023.tif")
if (!length(felev <- which(file.exists(elevtif)))) {
  curl::curl_download("http://www.cec.org/files/atlas_layers/0_reference/0_03_elevation/elevation_tif.zip", "elevation_tif.zip")
  unzip("elevation_tif.zip", files = "Elevation_TIF/NA_Elevation/data/northamerica/northamerica_elevation_cec_2023.tif", junkpaths = TRUE)
  unlink("elevation_tif.zip")
  cec <- terra::rast("northamerica_elevation_cec_2023.tif")
} else {
  cec <- terra::rast(elevtif[felev])
}


# Base map ----
l <- leaflet::leaflet(
    options = leaflet::leafletOptions(maxZoom = 25)
  ) |>
  # base layer
  leaflet::addProviderTiles(
    provider = leaflet::providers$CartoDB.PositronNoLabels,
    options = leaflet::pathOptions(pane = "mapPane", maxZoom = 25, maxNativeZoom = 20),
    group = "Light"
  ) |>
  leaflet::addProviderTiles(
    provider = leaflet::providers$CartoDB.DarkMatterNoLabels, 
    options = leaflet::pathOptions(pane = "mapPane", maxZoom = 25, maxNativeZoom = 20),
    group = "Dark"
  ) |>
  leaflet::addProviderTiles(
    provider = leaflet::providers$Esri.WorldImagery, 
    options = leaflet::pathOptions(pane = "mapPane", maxZoom = 25, maxNativeZoom = 18),
    group = "Satellite"
  ) |>
  leaflet::addProviderTiles(
    provider = leaflet::providers$OpenStreetMap, 
    options = leaflet::pathOptions(pane = "mapPane", maxZoom = 25, maxNativeZoom = 20),
    group = "OpenStreetMap"
  ) |>
  leaflet::addTiles(
    urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbhsstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
    attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
    options = leaflet::pathOptions(pane = "mapPane", maxZoom = 25, maxNativeZoom = 22),
    group = "Hillshade",

  ) |>
  # overlay layer
  leaflet::addTiles(
    urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
    attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
    options = leaflet::pathOptions(pane = "overlayPane", maxZoom = 25, maxNativeZoom = 22),
    group = "Labels"
  ) |>
  add_custom_render() |>
  # extensions
  leaflet.extras::addSearchOSM(
    options = leaflet.extras::searchOptions(
      collapsed = TRUE,
      hideMarkerOnCollapse = TRUE,
      autoCollapse = TRUE,
      zoom = 11
    )
  ) |>
  leaflet::addLayersControl(
    baseGroups = c("Light", "Dark", "Satellite", "OpenStreetMap", "Hillshade"),
    overlayGroups = c("Labels", "WNA BEC", "Climate"),
    position = "topright"
  ) |>
  leaflet::setView(lng = -100, lat = 50, zoom = 5) |>
  leaflet::addMiniMap(toggleDisplay = TRUE, minimized = TRUE) |>
  default_draw_tool() |>
  leaflet::hideGroup(c("WNA BEC", "Climate")) |>
  leaflet::showGroup("Hillshade")

# Shiny App ----

shiny::shinyApp(

# Shiny UI ----
    
  ui = shiny::tagList(
    # Favicon
    tags$head(
      tags$link(rel="apple-touch-icon", href="images/bcid-apple-touch-icon.png", sizes="180x180"),
      tags$link(rel="icon", href="images/bcid-favicon-32x32.png", sizes="32x32", type="image/png"),
      tags$link(rel="icon", href="images/bcid-favicon-16x16.png", sizes="16x16", type="image/png"),
      tags$link(rel="mask-icon", href="images/bcid-apple-icon.svg", color="#036"),
      tags$link(rel="icon", href="images/bcid-favicon-32x32.png")
    ),
    shiny::navbarPage(collapsible = TRUE, 
      theme = bslib::bs_theme(
        preset = "bcgov",
        "navbar-brand-padding-y" = "0rem",
        "navbar-brand-margin-end" = "4rem"
      ), 
      title = shiny::tagList(
        shiny::tags$image(
          src = "images/bcid-logo-rev-en.svg",
          style = "display: inline-block",
          height = "35px",
          alt = "British Columbia"
        ),
        "ClimR"
      ),
      shiny::tabPanel(title = "Map",
        shiny::div(class="outer",
          leaflet::leafletOutput("climr", width = "100%", height = "100%"),
          shiny::absolutePanel(
            class = "input-control",
            shiny::div(class = "input-control-header", shiny::h4("Controls")),
            shiny::div(class = "input-control-body",
              shiny::div(
                title = "Upload a csv, a raster or a shape file to add geographies",
                shiny::fileInput(
                  inputId = "upload",
                  label = "Upload geometry or raster file"
                )
              ),
              # Downscale parameters
              shiny::actionButton(
                inputId = "downscale_parameters",
                label = "Downscale Parameters",
                title = "Open advanced downscale parameters selection",
                class = "btn btn-primary btn-sm",
                width = "62%"
              ),  
              shiny::actionButton(
                inputId = "downscale_process",
                label = "",
                title = "Open downscale process launch window with currently active geographies",
                class = "btn btn-secondary btn-sm", icon = shiny::icon("gear"), width = "17%", disabled = TRUE),
              shiny::downloadButton(
                outputId = "downscale_download",
                label = "",
                title = "Download downscaled geographies archive",
                class = "btn btn-secondary btn-sm"
              ),
              # Overlay parameters
              shiny::hr(),
              shiny::actionButton(
                inputId = "selectoverlay",
                label = "Select Climate Overlay",
                title = "Open map climate overlay selection",
                class = "btn btn-primary btn-sm",
                width = "62%"
              ),
              shiny::actionButton(
                inputId = "downloadoverlay",
                label = "Download",
                title = "Download currently active overlay raster (tif)",
                class = "btn btn-secondary btn-sm",
                disabled = TRUE,
                width = "36%",
                icon = shiny::icon("map")
              ),
              shiny::div(
                title = "Adjust the opacity of the currently active overlay",
                shiny::sliderInput(
                  inputId = "opacity",
                  label = "Overlay opacity",
                  value = 80,
                  min = 0,
                  max = 100,
                  step = 1,
                  post = "%",
                  ticks = FALSE
                ),
              ),
              shiny::tags$div(
                title = "Adjust the resolution of the currently active overlay",
                shiny::sliderInput(
                  inputId = "resolution",
                  label = "Overlay resolution",
                  value = 96,
                  min = 24,
                  max = 384,
                  step = 12,
                  post = "px",
                  ticks = FALSE
                )
              ),
              shiny::div(
                style = "display: inline-flex; gap: 8px",
                shiny::div(
                  title = "Adjust color palette of the currently active overlay",
                  shiny::selectizeInput(
                    inputId = "palette",
                    label = NULL,
                    choices = pals$select,
                    selected = "Roma",
                    width = "225px",
                    options = list(render = I('{option: function(item, escape) {return item.label;},item: function(item, escape) {return item.label;}}'))
                  )
                ),
                shiny::div(
                  title = "Invert color palette value association of the currently active overlay",
                  shiny::checkboxInput("inverse", "Invert", width = "72px")
                )
              )
            )           
          )
        )
      ),
      shiny::navbarMenu(
        "Data",
        "Locations",
        shiny::tabPanel(title = "Geometry",
          shiny::div(class="outer2",
            DT::DTOutput(outputId = "geom_dt")
          )
        )
      ),
      shiny::navbarMenu(
        "About",
        "How to use",
        shiny::tabPanel("Map"),
        shiny::tabPanel("Date")
      ),
      header = list(
        shiny::includeCSS("www/style.css"),
        shiny::includeScript("www/script.js")
      )
    ),
    # Footer
    tags$footer(class = "footer mt-5",
      tags$nav(class = "navbar navbar-expand-lg bottom-static navbar-dark bg-primary-nav",
        tags$div(class = "container",
          tags$ul(class = "navbar-nav",
            tags$li(class = "nav-item",
              tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content/home", "Home", target = "_blank")
            ),
            tags$li(class = "nav-item",
              tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=79F93E018712422FBC8E674A67A70535", "Disclaimer", target = "_blank")
            ),
            tags$li(class = "nav-item",
              tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=9E890E16955E4FF4BF3B0E07B4722932", "Privacy", target = "_blank")
            ),
            tags$li(class = "nav-item",
              tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=E08E79740F9C41B9B0C484685CC5E412", "Accessibility", target = "_blank")
            ),
            tags$li(class = "nav-item",
              tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=1AAACC9C65754E4D89A118B875E0FBDA", "Copyright", target = "_blank")
            ),
            tags$li(class = "nav-item",
              tags$a(class = "nav-link", href = "https://www2.gov.bc.ca/gov/content?id=6A77C17D0CCB48F897F8598CCC019111", "Contact Us", target = "_blank")
            ),
          )
        )
      )
    )
  )

  ,

# Shiny server ----

  server = function(input, output, session) {
    
    session$allowReconnect("force")

    # ---- Modal input storage
    output$climr <- leaflet::renderLeaflet(l)
    vstore <- reactiveValues(
      temporality = "Annual",
      tifsource = names(climr_tif) |> head(1),
      climatevar = "NONE",
      downscale_which_refmap = "auto",
      downscale_obs_periods = "NULL",
      downscale_obs_years = c(),
      downscale_obs_ts_dataset = "NULL",
      downscale_gcms = c(),
      downscale_ssps = c(),
      downscale_gcm_periods = c(),
      downscale_gcm_ssp_years = c(),
      downscale_gcm_hist_years = c(),
      downscale_max_run = 0,
      downscale_run_nm = c(),
      downscale_core_vars = sort(sprintf(c("PPT_%02d", "Tmax_%02d", "Tmin_%02d"), sort(rep(1:12, 3)))),
      downscale_core_ppt_lr = FALSE,
      downscale_output = "csv",
      downscale_resolution = 2500,
      vscale = ""
    )
    
    # ---- Geometry
    source("scripts/geometry.R", local = TRUE)
    sg <- session_geometry()

    # ---- Map events
    shiny::observeEvent(input$climr_draw_start,         sg$add_point_enabled(FALSE))
    shiny::observeEvent(input$climr_draw_stop,          sg$add_point_enabled(TRUE))
    shiny::observeEvent(input$climr_draw_new_feature,   sg$add_draw_poly(input$climr_draw_new_feature))
    shiny::observeEvent(input$climr_click,              sg$add_point(input$climr_click$lat, input$climr_click$lng))
    shiny::observeEvent(input$upload,                   sg$add_file(input$upload))
    shiny::observeEvent(input$sg_remove,                sg$rm(input$sg_remove))
    shiny::observeEvent(input$sg_view,                  sg$view(input$sg_view))
    shiny::observeEvent(input$downscale_process_launch, sg$process())

    # ---- Downscale events
    shiny::observeEvent(input$downscale_parameters, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Downscale Parameters", size = "xl", easyClose = TRUE, fade = FALSE, class = "modal-dialog-scrollable",
          shiny::selectInput(
            inputId = "downscale_which_refmap",
            label =  "Reference Map",
            width = "100%",
            choices = c(
              list("Auto" = "auto"),
              local({z <- climr::list_refmaps(); substr(z, 8L, z |> nchar()) |> tools::toTitleCase() |> setNames(object = z, nm = _)})
            ),
            selected = vstore[["downscale_which_refmap"]]
          ),
          shiny::selectInput(
            inputId = "downscale_obs_periods",
            label =  "Observation periods",
            width = "100%",
            choices = c("Null" = "NULL", climr::list_obs_periods()),
            selected = vstore[["downscale_obs_periods"]]
          ),
          shiny::selectInput(
            inputId = "downscale_obs_years",
            label = "Observation years",
            width = "100%",
            choices = climr::list_obs_years(),
            multiple = TRUE,
            selected = c()
          ),
          shiny::selectInput(
            inputId = "downscale_obs_ts_dataset",
            label = "Observation time-series data",
            width = "100%",
            selected = vstore[["downscale_obs_ts_dataset"]],
            choices = c("Null" = "NULL", "ClimateNA" = "climatena", "Climatic Research Unit / Global Precipitation Climatology Centre" = "cru.gpcc")
          ),
          shiny::selectInput(
            inputId = "downscale_gcms",
            label = "Global climate model",
            width = "100%",
            choices = climr::list_gcms(),
            multiple = TRUE,
            selected = vstore[["downscale_gcms"]]
          ),
          shiny::selectInput(
            inputId = "downscale_ssps",
            label = "Shared Socio-economic Pathways (SSP) - Representative Concentration Pathways (RCP) Scenarios",
            width = "100%",
            choices = climr::list_ssps(),
            multiple = TRUE,
            selected = vstore[["downscale_ssps"]]
          ),          
          shiny::selectInput(
            inputId = "downscale_gcm_periods",
            label = "General Circulation Model (GCM) Periods",
            width = "100%",
            choices = climr::list_gcm_periods(),
            multiple = TRUE,
            selected = vstore[["downscale_gcm_periods"]]
          ),
          shiny::selectInput(
            inputId = "downscale_gcm_ssp_years",
            label = "General circulation model (GCM) Shared Socio-economic Pathways (SSP) Years",
            width = "100%",
            choices = climr::list_gcm_ssp_years(),
            multiple = TRUE,
            selected = vstore[["downscale_gcm_ssp_years"]]
          ),
          shiny::selectInput(
            inputId = "downscale_gcm_hist_years",
            label = "General circulation model (GCM) Historical Years",
            width = "100%",
            choices = climr::list_gcm_hist_years(),
            multiple = TRUE,
            selected = vstore[["downscale_gcm_hist_years"]]
          ),
          shiny::selectInput(
            inputId = "downscale_max_run",
            label = "Maximum number of model runs",
            width = "100%",
            choices = c("ensembleMean" = 0, 1:10),
            multiple = FALSE,
            selected = vstore[["downscale_max_run"]]
          ),
          shiny::selectInput(
            inputId = "downscale_run_nm",
            label = "Name of specified runs",
            width = "100%",
            choices = c(),
            multiple = TRUE,
            selected = vstore[["downscale_run_nm"]]
          ),
          shiny::selectizeInput(
            inputId = "downscale_core_vars",
            label = "Climate variables",
            width = "100%",
            choices = climr::list_vars(),
            multiple = TRUE,
            selected = vstore[["downscale_core_vars"]]
          ),
          shiny::checkboxInput(
            inputId = "downscale_core_ppt_lr",
            label = "Precipitaion elevation adjustment"
          ),
        )
      )
    })
    shiny::observeEvent(input$downscale_which_refmap, {vstore[["downscale_which_refmap"]] <- input$downscale_which_refmap})
    shiny::observeEvent(input$downscale_obs_periods, {vstore[["downscale_obs_periods"]] <- input$downscale_obs_periods})
    shiny::observeEvent(input$downscale_obs_years, {vstore[["downscale_obs_years"]] <- input$downscale_obs_years})
    shiny::observeEvent(input$downscale_obs_ts_dataset, {vstore[["downscale_obs_ts_dataset"]] <- input$downscale_obs_ts_dataset})
    shiny::observeEvent(input$downscale_gcms, {vstore[["downscale_gcms"]] <- input$downscale_gcms})
    shiny::observeEvent(input$downscale_ssps, {vstore[["downscale_ssps"]] <- input$downscale_ssps})
    shiny::observeEvent(input$downscale_gcm_periods, {vstore[["downscale_gcm_periods"]] <- input$downscale_gcm_periods})
    shiny::observeEvent(input$downscale_gcm_ssp_years, {vstore[["downscale_gcm_ssp_years"]] <- input$downscale_gcm_ssp_years})
    shiny::observeEvent(input$downscale_gcm_hist_years, {vstore[["downscale_gcm_hist_years"]] <- input$downscale_gcm_hist_years})
    shiny::observeEvent(input$downscale_max_run, {vstore[["downscale_max_run"]] <- input$downscale_max_run})
    shiny::observeEvent(input$downscale_run_nm, {vstore[["downscale_run_nm"]] <- input$downscale_run_nm})
    shiny::observeEvent(input$downscale_core_vars, {vstore[["downscale_core_vars"]] <- input$downscale_core_vars})
    shiny::observeEvent(input$downscale_core_ppt_lr, {vstore[["downscale_core_ppt_lr"]] <- input$downscale_core_ppt_lr})
    shiny::observe({
      gcms <- vstore[["downscale_gcms"]]
      ssps <- vstore[["downscale_ssps"]]
      if (!length(gcms) && !length(ssps)) {
        shiny::updateSelectInput(inputId = "downscale_run_nm", choices = c())
      } else if (length(gcms) && !length(ssps)) {
        shiny::updateSelectInput(inputId = "downscale_run_nm", choices = climr::list_runs_historic(gcm = gcms))
      } else if (length(gcms) && length(ssps)) {
        shiny::updateSelectInput(inputId = "downscale_run_nm", choices = climr::list_runs_ssp(gcm = gcms, ssp = ssps))
      }
    })
    shiny::observeEvent(input$downscale_process, {
      output$downscale_points_count_estimate <- shiny::renderUI({
        pce <- sg$approx_count(vstore[["downscale_resolution"]])
        bslib::card(
          full_screen = FALSE,
          height = "auto",
          bslib::card_header("Load estimation"),
          class = "bg-warning",
          fill = TRUE,
          bslib::card_body(
            shiny::tags$span(
              if (pce$marker_count > 0) "[%s] point geometries from [%s] markers." |> sprintf(format(pce$marker, big.mark = ","), format(pce$marker_count, big.mark = ",")),
              shiny::br(),
              if (pce$shape_count > 0) "[%s] point geometries from [%s] shapes (approximate using resolution)." |> sprintf(format(pce$shape, big.mark = ","), format(pce$shape_count, big.mark = ","))
            )
          )
        )
      })
      shiny::showModal(
        shiny::modalDialog(
          title = "Preferences for Downscale Processing", size = "l", easyClose = TRUE, fade = FALSE,
          shiny::div(
            title = "csv: all points are returned in csv. tif: Only shapes/rasters are returned as GeoTIFF if selected.",
            shiny::radioButtons(
              inputId = "downscale_output",
              label =  "Downscale Output Format", c("Comma Separated Value (csv)" = "csv", "Geographic Tag Image File Format (GeoTIFF)" = "tif"),
              inline = TRUE,
              selected = vstore[["downscale_output"]]
            )
          ),
          div(
            title = "Target resolution for shapes drawn on map or added using file upload. Does not apply to points, raster or csv files.",
            shiny::sliderInput(
              inputId = "downscale_resolution",
              label = "Downscale Resolution (m)",
              value = vstore[["downscale_resolution"]],
              width = "100%",
              min = 250,
              max = 50000,
              step = 250,
              post = "m",
              ticks = FALSE
            )
          ),
          shiny::uiOutput("downscale_points_count_estimate"),
          shiny::actionButton(
            inputId = "downscale_process_launch",
            label = "Launch Downscale Process",
            title = "Trigger a downscale processing run. At the end of the run, the download button on the main control panel will be enabled.",
            class = "btn btn-primary btn-lg",
            icon = shiny::icon("robot"),
            width = "100%"
          ),
        )
      )
    })
    shiny::observeEvent(input$downscale_output, {vstore[["downscale_output"]] <- input$downscale_output})
    shiny::observeEvent(input$downscale_resolution, {vstore[["downscale_resolution"]] <- input$downscale_resolution})

    # ---- Overlay events
    shiny::observeEvent(input$selectoverlay, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Climate Overlay Selection", size = "xl", easyClose = TRUE, fade = FALSE,
          shiny::selectInput(
            inputId = "tifsource",
            label =  "Source",
            width = "100%",
            choices = names(climr_tif),
            selected = vstore[["tifsource"]]
          ),
          shiny::radioButtons(
            inputId = "temporality", "Temporality", c("Annual", "Seasonal", "Monthly"),
            inline = TRUE,
            selected = vstore[["temporality"]]
          ),
          shiny::selectInput(
            inputId = "climatevar", 
            label = "Variables (%s)" |> sprintf(vstore[["tifsource"]]),
            selectize = TRUE,
            width = "100%",
            choices = c("None" = "NONE", climr_tif[[vstore[["tifsource"]]]][temporality %in% vstore[["temporality"]], setNames(url, label)]),
            selected = vstore[["climatevar"]]
          )
        )
      )
    })
    shiny::observeEvent(input$tifsource, {
      vstore[["tifsource"]] <- input$tifsource
      shiny::updateSelectInput(
        inputId = "climatevar", label = "Variables (%s)" |> sprintf(vstore[["tifsource"]]),
        choices = c("None" = "NONE", climr_tif[[vstore[["tifsource"]]]][temporality %in% vstore[["temporality"]], setNames(url, label)])
      )
    })
    shiny::observeEvent(input$temporality, {
      vstore[["temporality"]] <- input$temporality
      shiny::updateSelectInput(
        inputId = "climatevar",
        choices = c("None" = "NONE", climr_tif[[vstore[["tifsource"]]]][temporality %in% vstore[["temporality"]], setNames(url, label)])
      )
    })
    shiny::observeEvent(input$climatevar, {
      vstore[["climatevar"]] <- input$climatevar
      mp <- leaflet::leafletProxy("climr")
      mp |> leaflet::clearGroup("Climate") |> leaflet::hideGroup("Climate")
      session$sendCustomMessage(type="jsCode", list(code= "$('#rasterValues-val').remove();"))
      shiny::updateActionButton(inputId = "downloadoverlay", disabled = TRUE)
      if ("NONE" %in% vstore[["climatevar"]] | 0 == input$opacity) return()
      shiny::updateActionButton(inputId = "downloadoverlay", disabled = FALSE)
      prefix <- vstore[["climatevar"]] |> basename() |> tools::file_path_sans_ext()
      fpal <- if (isTRUE(input$inverse)) rev else identity
      mp |> leafem::addGeotiff(
        url = vstore[["climatevar"]],
        group = "Climate",
        layerId = "val",
        project = FALSE,
        opacity = input$opacity / 100,
        colorOptions = leafem::colorOptions(
          palette = pals$colors[[input$palette]] |> fpal(),
          na.color = "transparent"
        ),
        imagequery = TRUE,
        imagequeryOptions = leafem::imagequeryOptions(
          prefix = prefix
        ),
        autozoom = FALSE,
        options = leaflet::tileOptions(maxZoom = 25, maxNativeZoom = 20)
      ) |> leaflet::showGroup("Climate")
      if (prefix %in% climr_ratios) {
        vstore[["vscale"]] <- "log"
      } else {
        vstore[["vscale"]] <- ""
      }
      shiny::showNotification("Rendering %s values" |> sprintf(prefix), duration = 5)
    })
    shiny::observeEvent(input$opacity, {
      session$sendCustomMessage(type="updateOpacity", list(category = "image", layerId = "val", opacity = input$opacity /100))
    })
    shiny::observeEvent(shiny::debounce(input$resolution, 500), {
      session$sendCustomMessage(type="updateResolution", list(category = "image", layerId = "val", resolution = input$resolution))
    })
    shiny::observeEvent(input$inverse, {
      if (isTRUE(input$inverse)) {
        session$sendCustomMessage(type="jsCode", list(code= "$('.palselect').addClass('palselect-invert');"))
      } else {
        session$sendCustomMessage(type="jsCode", list(code= "$('.palselect').removeClass('palselect-invert');"))
      }      
    })
    shiny::observe({
      fpal <- if (isTRUE(input$inverse)) rev else identity
      session$sendCustomMessage(type="updateClimatePalette", list(
        category = "image", layerId = "val", vscale = vstore[["vscale"]], colorOptions = leafem::colorOptions(
          palette = pals$colors[[input$palette]] |> fpal(),
          na.color = "transparent"
        )
      ))
    })
    shiny::observeEvent(input$downloadoverlay, {
      session$sendCustomMessage(type="jsCode", list(code = "window.location.assign('%s');" |> sprintf(vstore[["climatevar"]])))
    })
    
  }
)
