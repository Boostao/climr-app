
# Setup ----
suppressPackageStartupMessages({
  library(archive)
  library(bslib)
  library(data.table)
  library(DT)
  library(htmltools)
  library(leaflet.extras)
  library(leaflet)
  library(shiny)
  library(terra)
  library(jsonlite)
  source("scripts/utils.R", local = TRUE)
})

# Shiny options
options(shiny.maxRequestSize = 1000 * 1024^2)

# MapBox values
mbtk <- Sys.getenv("BCGOV_MAPBOX_TOKEN")
mblbstyle <- Sys.getenv("BCGOV_MAPBOX_LABELS_STYLE")
mbhsstyle <- Sys.getenv("BCGOV_MAPBOX_HILLSHADE_STYLE")

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
    group = "Hillshade"
  ) |>
  # overlay layer
  leaflet::addTiles(
    urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
    attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
    options = leaflet::pathOptions(pane = "overlayPane"),
    group = "Labels"
  ) |>
  add_wna() |>
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
  default_draw_tool()

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
            shiny::selectInput("climatevar", "Climate overlay (1981-2010 Hist. norm.)", choices = c("None" = "NONE", climatena)),
            shiny::sliderInput("opacity", "Overlay opacity", value = 80, min = 0, max = 100, step = 1, ticks = FALSE),
            shiny::fileInput("upload", "Upload geometry or raster file")
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
    observe({
      mp <- leaflet::leafletProxy("climr")
      mp |> leaflet::removeTiles("climate")
      if ("NONE" %in% input$climatevar | 0 == input$opacity) return()
      t <- indexcna[[input$climatevar]]
      mp |> leaflet::addTiles(
        urlTemplate = t$tiles,
        layerId = "climate",
        group = "Climate",
        options = leaflet::tileOptions(
          maxNativeZoom = t$maxzoom,
          minZoom = t$minzoom,
          opacity = input$opacity / 100
        )
      )
    })
    
  }
)
