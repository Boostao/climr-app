
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
  source("scripts/utils.R", local = TRUE)
})

# Shiny options
options(shiny.maxRequestSize = 1000 * 1024^2)

# MapBox values
mbtk <- Sys.getenv("BCGOV_MAPBOX_TOKEN")
mblbstyle <- Sys.getenv("BCGOV_MAPBOX_LABELS_STYLE")
mbhsstyle <- Sys.getenv("BCGOV_MAPBOX_HILLSHADE_STYLE")

pals <- readRDS("scripts/pals.rds")

# Base map ----
l <- leaflet::leaflet() |>
  # base layer
  leaflet::addProviderTiles(
    provider = leaflet::providers$CartoDB.PositronNoLabels,
    options = leaflet::pathOptions(pane = "mapPane"),
    group = "Light"
  ) |>
  leaflet::addProviderTiles(
    provider = leaflet::providers$CartoDB.DarkMatterNoLabels, 
    options = leaflet::pathOptions(pane = "mapPane"),
    group = "Dark"
  ) |>
  leaflet::addProviderTiles(
    provider = leaflet::providers$Esri.WorldImagery, 
    options = leaflet::pathOptions(pane = "mapPane"),
    group = "Satellite"
  ) |>
  leaflet::addProviderTiles(
    provider = leaflet::providers$OpenStreetMap, 
    options = leaflet::pathOptions(pane = "mapPane"),
    group = "OpenStreetMap"
  ) |>
  leaflet::addTiles(
    urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbhsstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
    attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
    options = leaflet::pathOptions(pane = "mapPane"),
    group = "Hillshade",

  ) |>
  # overlay layer
  leaflet::addTiles(
    urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
    attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
    options = leaflet::pathOptions(pane = "overlayPane"),
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
            shiny::fileInput("upload", "Upload geometry or raster file"),
            shiny::actionButton("toggleOverlayInputs", "Climate Overlay", class = "btn btn-info btn-sm", `data-bs-toggle` = "collapse", `data-bs-target` = "#collapseOverlayInputs"),
            shiny::div(class = "collapse", id = "collapseOverlayInputs",
              shiny::radioButtons("temporality", "Temporality", c("Annual", "Seasonal", "Monthly"), inline = TRUE),
              shiny::selectInput("climatevar", "Measurement (1981-2010 Hist. norm.)", choices = c("None" = "NONE", climr_tif[["Annual"]])),
              shiny::selectizeInput("palette", "Overlay color palette", choices = pals$select, selected = "Roma",
                options = list(render = I('{option: function(item, escape) {return item.label;},item: function(item, escape) {return item.label;}}'))
              ),
              shiny::sliderInput("opacity", "Overlay opacity", value = 80, min = 0, max = 100, step = 1, ticks = FALSE),
              shiny::actionButton("downloadoverlay", "Download Overlay", class = "btn btn-secondary btn-sm", disabled = TRUE)
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

    output$climr <- leaflet::renderLeaflet(l)

    source("scripts/geometry.R", local = TRUE)
    sg <- session_geometry()

    ## Map events
    observeEvent(input$climr_draw_start, sg$add_point_enabled(FALSE))
    observeEvent(input$climr_draw_stop, sg$add_point_enabled(TRUE))
    observeEvent(input$climr_draw_new_feature, sg$add_draw_poly(input$climr_draw_new_feature))
    observeEvent(input$climr_click, sg$add_point(input$climr_click$lat, input$climr_click$lng))
    observeEvent(input$upload, sg$add_file(input$upload))
    observeEvent(input$sg_remove, sg$rm(input$sg_remove))
    observeEvent(input$sg_view, sg$view(input$sg_view))
    observeEvent(input$temporality, {
      shiny::updateSelectInput(inputId = "climatevar", choices = c("None" = "NONE", climr_tif[[input$temporality]]))
    })
    observeEvent(input$climatevar, {
      mp <- leaflet::leafletProxy("climr")
      mp |> removeTiles("val")
      session$sendCustomMessage(type="jsCode", list(code= "$('#rasterValues-val').remove();"))
      shiny::updateActionButton(inputId = "downloadoverlay", disabled = TRUE)
      if ("NONE" %in% input$climatevar | 0 == input$opacity) return()
      shiny::updateActionButton(inputId = "downloadoverlay", disabled = FALSE)
      mp |> leafem::addGeotiff(      
        url = input$climatevar,
        group = "Climate",
        layerId = "val",
        project = FALSE,
        opacity = input$opacity / 100,
        colorOptions = leafem::colorOptions(
          palette = pals$colors[[input$palette]],
          na.color = "transparent"
        ),
        imagequery = TRUE,
        imagequeryOptions = leafem::imagequeryOptions(
          prefix = input$climatevar |> basename() |> tools::file_path_sans_ext()
        ),
        autozoom = FALSE
      ) |> leaflet::showGroup("Climate")
    })
    observeEvent(input$opacity, {
      session$sendCustomMessage(type="updateOpacity", list(category = "image", layerId = "val", opacity = input$opacity /100))
    })
    observeEvent(input$palette, {
      session$sendCustomMessage(type="updateClimatePalette", list(
        category = "image", layerId = "val", colorOptions = leafem::colorOptions(
          palette = pals$colors[[input$palette]],
          na.color = "transparent"
        )
      ))
    })
    observeEvent(input$downloadoverlay, {
      session$sendCustomMessage(type="jsCode", list(code = "window.location.assign('%s');" |> sprintf(input$climatevar)))
    })
    
  }
)
