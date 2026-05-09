# =========================
# LIBRARIES
# =========================
library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(ggplot2)
library(dplyr)
library(DT)
library(terra)
library(lidR)
library(sf)
library(RCSF)

# =========================
# HELPER FUNCTION
# =========================
estimate_gbh <- function(height_m, a = 3.5, b = 1.8) {
  a * (height_m ^ b)
}

# =========================
# UI
# =========================
ui <- dashboardPage(
  dashboardHeader(title = "LiDAR Forest Dashboard"),
  
  dashboardSidebar(
    sliderInput("z_filter", "Min Height", 0, 60, 0),
    sliderInput("sample_n", "3D sample size", 1000, 50000, 15000),
    checkboxInput("show_trees", "Show Trees", TRUE)
  ),
  
  dashboardBody(
    fluidRow(
      valueBoxOutput("trees"),
      valueBoxOutput("mean_h"),
      valueBoxOutput("max_h")
    ),
    
    fluidRow(
      box(width = 6, plotOutput("hist")),
      box(width = 6, plotOutput("gbh_plot"))
    ),
    
    fluidRow(
      box(width = 12, plotlyOutput("plot3d", height = 600))
    ),
    
    fluidRow(
      box(width = 12, leafletOutput("chm_map", height = 500))
    ),
    
    fluidRow(
      box(width = 12, DTOutput("table"))
    ),
    
    # ✅ 3D VIDEO - Updated Link
    fluidRow(
      box(
        width = 12,
        title = "3D LiDAR Video",
        status = "success",
        solidHeader = TRUE,
        tags$iframe(
          src = "https://drive.google.com/file/d/14n9kHZyQnLvRv5JK-XkrV4owBSZZPxbO/preview",
          width = "100%",
          height = "500px",
          style = "border:none;",
          allowfullscreen = "true"
        )
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output) {
  
  data <- reactive({
    
    # ✅ Fixed path + filter
    las <- readLAS("Data/2026-04-11_11-46-34_9pct_time.laz",
                   filter = "-drop_return_number 0")
    
    validate(need(!is.empty(las), "LAS file empty"))
    
    # ✅ Fix CRS for GeoSLAM scanner
    projection(las) <- sp::CRS("+proj=tmerc +datum=WGS84")
    
    # Ground + normalize
    las <- classify_ground(las, csf())
    las <- normalize_height(las, tin())
    
    # CHM
    chm <- rasterize_canopy(las, res = 1, p2r())
    
    # Trees
    ttops <- locate_trees(las, lmf(ws = 5))
    coords <- st_coordinates(ttops)
    
    trees <- data.frame(
      X = coords[,1],
      Y = coords[,2],
      Z = ttops$Z
    )
    
    trees$GBH <- estimate_gbh(trees$Z)
    
    list(las = las, trees = trees, chm = chm)
  })
  
  lidar_sample <- reactive({
    df <- as.data.frame(data()$las@data)
    df <- df %>% filter(Z >= input$z_filter)
    n <- min(input$sample_n, nrow(df))
    df[sample(nrow(df), n), ]
  })
  
  # =====================
  # VALUE BOXES
  # =====================
  output$trees <- renderValueBox({
    valueBox(nrow(data()$trees), "Trees Detected", color = "green")
  })
  
  output$mean_h <- renderValueBox({
    valueBox(round(mean(data()$trees$Z), 2), "Mean Height (m)", color = "blue")
  })
  
  output$max_h <- renderValueBox({
    valueBox(round(max(data()$trees$Z), 2), "Max Height (m)", color = "purple")
  })
  
  # =====================
  # PLOTS
  # =====================
  output$hist <- renderPlot({
    ggplot(data()$trees, aes(Z)) +
      geom_histogram(fill = "forestgreen", bins = 25) +
      labs(title = "Tree Height Distribution",
           x = "Height (m)", y = "Count")
  })
  
  output$gbh_plot <- renderPlot({
    ggplot(data()$trees, aes(Z, GBH)) +
      geom_point(color = "brown") +
      geom_smooth(method = "lm") +
      labs(title = "Height vs GBH",
           x = "Height (m)", y = "GBH (cm)")
  })
  
  # =====================
  # 3D PLOT
  # =====================
  output$plot3d <- renderPlotly({
    pts <- lidar_sample()
    
    p <- plot_ly(
      pts, x=~X, y=~Y, z=~Z,
      type = "scatter3d",
      mode = "markers",
      marker = list(size=1, color=~Z, colorscale="Viridis")
    )
    
    if(input$show_trees) {
      p <- add_trace(p,
                     data = data()$trees,
                     x=~X, y=~Y, z=~Z,
                     type = "scatter3d",
                     mode = "markers",
                     marker = list(size=4, color="red"),
                     name = "Trees")
    }
    p
  })
  
  # =====================
  # CHM MAP
  # =====================
  output$chm_map <- renderLeaflet({
    chm <- data()$chm
    pal <- colorNumeric("YlGn", values(chm), na.color = "transparent")
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addRasterImage(chm, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = values(chm), title = "Height (m)")
  })
  
  # =====================
  # TABLE
  # =====================
  output$table <- renderDT({
    datatable(data()$trees, options = list(pageLength = 10))
  })
}

# =========================
# RUN APP
# =========================
shinyApp(ui, server)
