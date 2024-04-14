library(shiny)
library(leaflet)
library(sf)
library(SpatialAcc)
library(spdep)
library(tmap)
library(ggpubr)
library(cluster)
library(factoextra)
library(heatmaply)
library(ClustGeo)
library(GGally)
library(tidyverse)
library(psych)
library(bslib)
library(shinycssloaders)
library(RColorBrewer)

# UI for App 1
ui_app1 <- fluidPage(
  titlePanel("Geographic Accessibilty"),
  sidebarLayout(
    sidebarPanel(
      selectInput("amenity",
                  "Choose an Amenity:",
                  choices = c("Schools", "MRT", "Mall", "Supermarket", "Hawker"),
                  selected = "Schools"), 
      selectInput("access_method",
                  "Choose an Accessibility Computation Method:",
                  choices = c("Hansen", "KD2SFCA", "SAM"), 
                  selected = "Hansen"), 
      selectInput("map_style", 
                  "Choose a Map Palette Style:", 
                  choices = brewer.pal.info %>% 
                               row.names())
    ),
    mainPanel(
      tmapOutput("accessibilityPlot")
    )
  )
)

# UI definitions for App 2
ui_app2 <- fluidPage(
  navbarPage("IS415 Project",
             tabPanel("HeatMaps",
                      titlePanel("Basic Heat Maps"),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "classes",
                                      label  = "Number of classes",
                                      min = 4,
                                      max = 10,
                                      value = c(6)),
                          selectInput(inputId = "style",
                                      label="Mapping style",
                                      choices = list("Equal Style" = "equal",
                                                     "Quantile Style"="quantile"),
                                      selected = "equal"),
                          
                          selectInput(inputId = "amenities",
                                      label="Amenities",
                                      choices = list("Supermarkets" = "supermarkets_frequencyFinal",
                                                     "All"="combinedfrequencyFinal"),
                                      
                                      selected = "supermarkets_frequencyFinal"),
                          #checkboxInput(inputId="compassToggle", "Toggle Compass", TRUE),
                          
                          selectInput(inputId = "compassToggle",
                                      label="Toggle Compass",
                                      choices = list("Yes" = "black",
                                                     "No"="snow"),
                                      
                                      selected = "snow")
                          
                        ),
                        mainPanel(
                          plotOutput("mapPlot",
                                     width = "100%",
                                     height = 400)
                        )
                        
                      )
             ), ## end of layout for 1st plot 
             
             
             
             tabPanel("Clustering",
                      titlePanel("Local moran plot for clusters"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "amenities2",
                                      label="Amenities",
                                      choices = list("Supermarkets" = "localMoranData2",
                                                     "All"="localMoranData",
                                                     "Mrts" = "localMoranData3",
                                                     "Hawkers" = "localMoranData4",
                                                     "Malls" = "localMoranData5"),
                                      
                                      selected = "supermarkets_frequencyFinal"),
                          sliderInput(inputId = "classes2",
                                      label  = "Number of classes",
                                      min = 3,
                                      max = 8,
                                      value = c(4)),
                          
                          selectInput(inputId = "compassToggle2",
                                      label="Toggle Compass",
                                      choices = list("Yes" = "black",
                                                     "No"="snow"),
                                      
                                      selected = "snow")
                          
                          
                        ),
                        mainPanel(
                          plotOutput("mapPlot2",
                                     width = "100%",
                                     height = 400)
                        )
                        
                      )
             ), ## end of 2nd plot
             tabPanel("Hotspot",
                      titlePanel("Hot & Cold-Spot Analysis"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "zone",
                                      label="Zone Category",
                                      choices = c("Subzone" = "subzone",
                                                     "Area"="area"),
                                      selected = "area"),
                          selectInput(inputId = "amenities3",
                                      label="Select Amenities",
                                      choices = c("All" = "All",
                                                     "Supermarket"="Supermarket",
                                                     "Mall"="Mall",
                                                     "Mrt"="Mrt",
                                                     "Hawker"="Hawker"),
                                      selected = "Supermarket"),
                          
                        ),    
                        
                        
                        mainPanel(
                          tmapOutput("mapPlot3",
                                     width = "100%",
                                     height = 400)
                        )
                        
                      )
             ) ## end of 3rd tab
             
             
             
  ) ## end of navbar page
)

# UI for App 3
ui_app3 <- fluidPage(
  tags$head(
    tags$script(HTML(
      "
      $(document).ready(function() {
        $('#amenities_select').find('input[type=checkbox]').on('change', function() {
          var checked = $('#amenities_select').find('input[type=checkbox]:checked').length;
          if (checked === 0 && !$(this).prop('checked')) {
            alert('Please select at least one option.');
            $(this).prop('checked', true); // Keep the current checkbox checked
          }
        });
      });
      "
    ))
  ), 
  
  titlePanel("Geospatial Clustering by Amenities in Singapore"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(# You can add inputs here if needed
        tabPanel("Data Scope Options",
                 selectizeInput("level_select", "Select Planning Level:",
                                choices = c("Planning Region", 
                                            "Planning Area", 
                                            "Planning Subzone"),
                                selected = "Planning Area"
                 ), 
                 selectInput("zone_select", "Select Planning Zone:", 
                             choices = NULL, 
                             multiple = TRUE), 
                 checkboxGroupInput("amenities_select", "Select Amenities:", 
                                    choiceNames = c("MRT Stations", 
                                                    "Schools", 
                                                    "Supermarkets", 
                                                    "Malls", 
                                                    "Hawker-Centers"), 
                                    choiceValues = c("mrt_count", 
                                                     "school_count", 
                                                     "supermarket_count", 
                                                     "mall_count", 
                                                     "hawker_count"), 
                                    selected = c("mrt_count", 
                                                 "school_count", 
                                                 "supermarket_count", 
                                                 "mall_count", 
                                                 "hawker_count"), 
                                    inline = TRUE
                 )
        ), 
        tabPanel("Scaling Options", 
                 selectInput("scale_plot_style", "Graph Style", 
                             choices = c("Discrete", "Continuous"), 
                             selected = "Discrete"), 
                 selectInput("scaled_amenity", "Amenity to Visualise: ", 
                             choices = c("Mrt", 
                                         "School", 
                                         "Supermarket", 
                                         "Mall", 
                                         "Hawker"), 
                             selected = "Mrt"), 
                 selectInput("scale_method", "Scaling Method", 
                             choices = c("None", 
                                         "Min-Max Standardisation", 
                                         "Z-score Standardisation"), 
                             selected = "None")), 
        tabPanel("Cluster Options",
                 selectInput("diss_method", "Dissimilarity Calculation", 
                             choices = c("euclidean", "maximum", "manhattan", 
                                         "canberra", "binary", "minkowski"), 
                             selected = "euclidean"), 
                 selectInput("cluster_algo", "Clustering Algorithm", 
                             choices = c("Hierarchical", 
                                         "K-Means", 
                                         "Spatially-constrained Ward Hierarchical", 
                                         "SKATER"), 
                             selected = "Hierarchical"), 
                 conditionalPanel(
                   condition = "input.cluster_algo == 'Hierarchical'", 
                   selectInput("hc_method", "Agglomeration Method", 
                               choices = c("ward.D", "ward.D2", "single", "complete", 
                                           "average", "mcquitty", "median", "centroid"), 
                               selected = "ward.D")
                 ),
                 conditionalPanel(
                   condition = "input.cluster_algo == 'K-Means'",
                   selectInput("k_means_algo", "K-Means Algorithm", 
                               choices = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), 
                               selected = "Hartigan-Wong")
                 ),
                 conditionalPanel(
                   condition = "input.cluster_algo == 'Spatially-constrained Ward Hierarchical'", 
                   selectInput("hcgeo_method", "Agglomeration Method",
                               choices = c("ward.D", "ward.D2"), 
                               selected = "ward.D")
                 ), 
                 conditionalPanel(
                   condition = "input.cluster_algo == 'SKATER'", 
                   selectInput("lcost_method", "Spatial Weighting Method",
                               choices = c("Row-standardised", 
                                           "Binary Spatial", 
                                           "Spatial Contiguity", 
                                           "Spatial Distance", 
                                           "Spatial Kernel", 
                                           "Min-Max Standardised"), 
                               selected = "Row-standardised")
                 )
        ), 
        tabPanel("Hyperparameter Tuning",
                 sliderInput("nstart", "Number of Starts:",
                             min = 10, max = 50, value = 25, step = 5),
                 sliderInput("k_max", "Maximum Number of Clusters (k.max):",
                             min = 2, max = 20, value = 10, step = 1),
                 sliderInput("nboot", "Number of Bootstrap Samples (nboot):",
                             min = 10, max = 200, value = 10, step = 10), 
                 selectInput("ncluster_estimation_method", "Cluster Estimation Method", 
                             choices = c("gap_stat", "silhouette", "wss")), 
                 sliderInput("k", HTML("<b style='color:red;'><i>Number of Clusters:</i></b>"), 
                             min = 2, max = 20, value = 5, step = 1), 
                 conditionalPanel(
                   condition = "input.cluster_algo == 'Spatially-constrained Ward Hierarchical'", 
                   sliderInput("alpha", HTML("<b style='color:red;'><i>Alpha Value for Spatially-constrained Ward Hierarchical Method:</i></b>"),
                               min = 0.1, max = 1.0, value = 0.5, step = 0.1)
                 )
        ), 
        tabPanel("Stylistic Options", 
                 selectInput("map_color_palette", "Map Colour Style", 
                             choices = brewer.pal.info %>% 
                               row.names(), 
                             selected =  "Set2"), 
                 sliderInput("map_color_contrast", "Map Colour Contrast", 
                             min = 0.0, max = 1.0, value = 0.5, step = 0.1), 
                 sliderInput("map_color_alpha", "Map Colour Opacity", 
                             min = 0.0, max = 1.0, value = 0.3, step = 0.1), 
                 checkboxInput("map_color_stretch", "Interpolate Colours", 
                               value = FALSE), 
                 conditionalPanel(
                   condition = "($('input[name=\\'amenities_select\\']:checked').length > 1)", 
                   sliderInput("distribution_alpha", "Cluster Distribution Opacity", 
                               min = 0.0, max = 1.0, value = 0.3, step = 0.1), 
                   textInput("shadebox_colour", "Cluster Distribution Background", 
                             value = "lightblue"), 
                   checkboxInput("incl_boxplot", "Include Distribution Boxplots", 
                                 value = TRUE), 
                   checkboxInput("show_distribution_points", "Show Distribution Points", 
                                 value = FALSE)
                 ), 
                 conditionalPanel(
                   condition = "($('input[name=\\'amenities_select\\']:checked').length === 1)", 
                   
                   textInput("boxplot_outline_color", "Boxplot Outline Colour", 
                             value = "lightblue"), 
                   textInput("boxplot_fill_color", "Boxplot Fill ColourI", 
                             value = "lightgrey"),
                   checkboxInput("boxplot_outlier", "Show Outliers", 
                                 value = TRUE),
                   sliderInput("boxplot_outlier_tolerance", "Boxplot Outlier Tolerance", 
                               min = 1.0, max = 5.0, value = 1.5, step = 0.1), 
                   textInput("boxplot_outlier_colour", "Boxplot Outlier Colour", 
                             value = "red"), 
                   sliderInput("boxplot_outlier_alpha", "Boxplot Outlier Opacity", 
                               min = 0.0, max = 1.0, value = 0.5, step = 0.1), 
                   checkboxInput("boxplot_notch", "Include Boxplot Notch", 
                                 value = FALSE), 
                   sliderInput("boxplot_notch_width", "Boxplot Notch Width", 
                               min = 0.1, max = 5.0, value = 0.5, step = 0.1)
                 )
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scaling Distribution", 
                 plotOutput("scale_plot")), 
        
        tabPanel("Hyperparameter Tuning", 
                 uiOutput("n_cluster_plot"), 
                 uiOutput("alpha_plot"), 
                 uiOutput("skater_stree_summary")), 
        
        tabPanel("Cluster Visualisation", 
                 tmapOutput("cluster_map")), 
        
        tabPanel("Cluster Distribution", 
                 plotOutput("cluster_distribution_plot"))
      )
    )
  )
)

# Define the combined UI using navbarPage
ui <- navbarPage(
  "Accessibility of Amenities in Singapore",
  theme = bs_theme(
    version = 4,
    bootswatch = "darkly"  # This is a pre-built Bootstrap dark theme
  ),
  tabPanel("Geographic Accessibility", ui_app1),  
  tabPanel("Hotspot Analysis", ui_app2),         
  tabPanel("Geospatial Segmentation", ui_app3)       
)

# Server logic for App 1
server_app1 <- function(input, output, session) {
  # Load Data
  hexagon_Hansen <- readRDS("data/rds/hexagon_Hansen.rds")
  hexagon_KD2SFCA <- readRDS("data/rds/hexagon_KD2SFCA.rds")
  hexagon_SAM <- readRDS("data/rds/hexagon_SAM.rds")
  schools_sf <- readRDS("data/rds/schools_sf.rds")
  MRT_sf <- readRDS("data/rds/MRT_sf.rds")
  mall_sf <- readRDS("data/rds/mall_sf.rds")
  supermarket_sf <- readRDS("data/rds/supermarket_sf.rds")
  hawker_sf <- readRDS("data/rds/hawker_sf.rds")
  mapex <- readRDS("data/rds/mapex.rds")
  
  renderPlotForMethod <- function(method, palette) {
    hex_data <- switch(method,
                       "Hansen" = hexagon_Hansen,
                       "KD2SFCA" = hexagon_KD2SFCA,
                       "SAM" = hexagon_SAM)
    
    amenity_data <- switch(input$amenity,
                           "Schools" = schools_sf,
                           "MRT" = MRT_sf,
                           "Mall" = mall_sf,
                           "Supermarket" = supermarket_sf,
                           "Hawker" = hawker_sf)
    
    amenity_name <- switch(input$amenity,
                           "Schools" = "schools",
                           "MRT" = "MRT",
                           "Mall" = "mall",
                           "Supermarket" = "supermarket",
                           "Hawker" = "hawker")
    
    score_col <- paste0(amenity_name, "_acc", method)
    
    tmap_options(main.title = paste("Accessibility to", input$amenity, ": ", method, "method"))
    
    tm_shape(hex_data, bbox = mapex) +
      tm_fill(col = score_col,
              n = 10,
              style = "quantile",
              palette = palette,
              border.col = "black",
              border.lwd = 1) +
      tm_shape(amenity_data) +
      tm_symbols(size = 0.1) + 
      tm_basemap("OpenStreetMap") +
      tm_scale_bar() + 
      tm_mouse_coordinates()
  }
  
  output$accessibilityPlot <- renderTmap({
    tmap_mode("view")
    
    renderPlotForMethod(input$access_method, input$map_style)
  })
}


# Server logic for App 2
server_app2 <- function(input, output, session) {
  islandPoly<- read_rds("data/rds/mpsz.rds")
  
  supermarkets_frequencyFinal<- read_rds("data/rds/supermarketFrequency.rds")
  combinedfrequencyFinal<- read_rds("data/rds/combinedFrequency.rds")
  localMoranData<- read_rds("data/rds/localMoran.rds")
  localMoranData2<- read_rds("data/rds/localMoran2.rds")
  localMoranData3<- read_rds("data/rds/localMoran3.rds")
  localMoranData4<- read_rds("data/rds/localMoran4.rds")
  localMoranData5<- read_rds("data/rds/localMoran5.rds")
  areaAll <-read_rds("data/rds/hotspot.rds")
  subzoneAll <-read_rds("data/rds/hotspot2.rds")
  areaSupermarket <-read_rds("data/rds/hotspot3.rds")
  subzoneSupermarket <-read_rds("data/rds/hotspot4.rds")
  areaMrt<-read_rds("data/rds/hotspot5.rds")
  areaHawker<-read_rds("data/rds/hotspot7.rds")
  areaMall<-read_rds("data/rds/hotspot9.rds")
  subzoneMrt<-read_rds("data/rds/hotspot6.rds")
  subzoneHawker<-read_rds("data/rds/hotspot8.rds")
  subzoneMall<-read_rds("data/rds/hotspot10.rds")
  
  output$mapPlot <- renderPlot(
    {
      tmap_mode("plot")
      tmap_options(check.and.fix=TRUE) 
      
      tm_shape(islandPoly)+
        tm_polygons()+
        tm_shape(get(input$amenities))+
        tm_fill("frequency",
                n = input$classes,
                style = input$style,
                palette = blues9)+
        tm_borders(alpha = 0.5) + 
        tm_compass(type="8star",size=2,color.dark = input$compassToggle, color.light = input$compassToggle,text.color = input$compassToggle)+
        tm_scale_bar(color.dark= input$compassToggle, text.color = input$compassToggle) + 
        tm_grid(alpha=0.2) +
        tm_layout(main.title = paste(input$amenities,"Heatmap"))
    }
  )
  
  output$mapPlot2 <- renderPlot(
    {
      tmap_mode("plot")
      tmap_options(check.and.fix=TRUE)
      
      localMI.map2 <- tm_shape(islandPoly) + 
          tm_polygons() +
        tm_shape(get(input$amenities2)) +
          tm_fill(col = "ii", 
                  style = "pretty", 
                  title = "local moran statistics",
                  n=input$classes2) +
        tm_borders(alpha = 0.5) + 
        tm_compass(type="8star",size=2,color.dark = input$compassToggle2, color.light = input$compassToggle2,text.color = input$compassToggle2)+
        tm_scale_bar(color.dark= input$compassToggle2, text.color = input$compassToggle2) + 
        tm_grid(alpha=0.2)
      
      pvalue.map2 <- tm_shape(islandPoly) + 
          tm_polygons() +
        tm_shape(get(input$amenities2)) +
          tm_fill(col = "p_ii", 
                  breaks=c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                  palette="-Blues", 
                  title = "local Moran's I p-values") +
        tm_borders(alpha = 0.5) + 
        tm_compass(type="8star",size=2,color.dark = input$compassToggle2, color.light = input$compassToggle2,text.color = input$compassToggle2)+
        tm_scale_bar(color.dark= input$compassToggle2, text.color = input$compassToggle2) + 
        tm_grid(alpha=0.2)
      
      tmap_arrange(localMI.map2, pvalue.map2, asp=1, ncol=2)
    }
  )
  
  output$mapPlot3 <- renderTmap(
    {
      tmap_mode("view")
      tmap_options(check.and.fix=TRUE)
      
      tm_shape(get(paste(input$zone, input$amenities3, sep=""))) +
        tm_fill(col = "cluster", id = "SUBZONE_N", alpha = 0.5) +
        tm_borders() + 
        tm_scale_bar() +
        tm_mouse_coordinates()
    }
  )
  
}

# Server logic for App 3
server_app3 <- function(input, output, session) {
  region_amenities_sf <- read_rds("data/rds/region_amenities_sf.rds")
  region_cluster_vars <- read_rds("data/rds/region_cluster_vars.rds")
  
  area_amenities_sf <- read_rds("data/rds/area_amenities_sf.rds")
  area_cluster_vars <- read_rds("data/rds/area_cluster_vars.rds")
  
  subzone_amenities_sf <- read_rds("data/rds/subzone_amenities_sf.rds")
  subzone_cluster_vars <- read_rds("data/rds/subzone_cluster_vars.rds")
  
  amenities_sf <- reactive({
    level <- input$level_select
    if (level == "Planning Region") {
      return(region_amenities_sf)
    } else if (level == "Planning Area") {
      return(area_amenities_sf)
    } else if (level == "Planning Subzone") {
      return(subzone_amenities_sf)
    }
  })
  
  cluster_vars <- reactive({
    level <- input$level_select
    if (level == "Planning Region") {
      return(region_cluster_vars)
    } else if (level == "Planning Area") {
      return(area_cluster_vars)
    } else if (level == "Planning Subzone") {
      return(subzone_cluster_vars)
    }
  })
  
  level_zones <- reactive({
    rownames(cluster_vars())
  })
  
  # Update choices and select all options for zone_select
  observeEvent(input$level_select, {
    updateSelectInput(session, "zone_select", 
                      choices = level_zones(), selected = level_zones()
    )
    updateSliderInput(session, "k_max", 
                      min = 2, max = length(level_zones()) - 1, 
                      value = min(length(level_zones()) - 1, 10), step = 1)
    updateSliderInput(session, "k", 
                      min = 2, max = length(level_zones()) - 1, 
                      value = min(length(level_zones()) - 1, 10), step = 1)
  })
  
  filtered_amenities_sf <- reactive({
    amenities_sf() %>%
      filter(.data[[colnames(.)[1]]] %in% input$zone_select) %>%
      select(1, 2, input$amenities_select)
  })
  
  filtered_cluster_vars <- reactive({
    cluster_vars() %>%
      filter(row.names(.) %in% input$zone_select) %>%
      select(1, 2, input$amenities_select %>% 
               str_replace("_count", "") %>% 
               str_to_title()
      )
  })
  
  popup_vars <- reactive({
    # Initialize the popup_vars list with Cluster ID
    popup_vars <- c("Cluster ID: " = "Cluster")
    
    # Add other popup variables based on selected amenities
    if ("mrt_count" %in% input$amenities_select) {
      popup_vars[["No. of MRT Stations: "]] <- "mrt_count"
    }
    if ("school_count" %in% input$amenities_select) {
      popup_vars[["No. of Schools: "]] <- "school_count"
    }
    if ("supermarket_count" %in% input$amenities_select) {
      popup_vars[["No. of Supermarkets: "]] <- "supermarket_count"
    }
    if ("mall_count" %in% input$amenities_select) {
      popup_vars[["No. of Malls: "]] <- "mall_count"
    }
    if ("hawker_count" %in% input$amenities_select) {
      popup_vars[["No. of Hawker-Centers: "]] <- "hawker_count"
    }
    
    return(popup_vars)
  })
  
  observeEvent(input$amenities_select, {
    scaled_amenities_options <- input$amenities_select %>% 
      str_replace("_count", "") %>% 
      str_to_title()
    
    updateSelectInput(session, "scaled_amenity", 
                      choices = scaled_amenities_options, 
                      selected = scaled_amenities_options[1]
    )
  })
  
  output$scale_plot <- renderPlot({
    amenity_to_scale <- input$scaled_amenity
    
    if (input$scale_plot_style == "Discrete") {
      r <- filtered_cluster_vars() %>% 
        ggplot(aes(x = .data[[input$scaled_amenity]])) + 
        geom_histogram(bins = 20, 
                       color = "black", 
                       fill="light blue") + 
        ggtitle("Non-standardised Amenity Count")
      
      s <- filtered_cluster_vars() %>% 
        normalize() %>% 
        ggplot(aes(x = .data[[input$scaled_amenity]])) + 
        geom_histogram(bins = 20, 
                       color = "black", 
                       fill="light blue") + 
        ggtitle("Min-Max Standardised Amenity Count")
      
      z <- filtered_cluster_vars() %>% 
        scale() %>% 
        ggplot(aes(x = .data[[input$scaled_amenity]])) + 
        geom_histogram(bins = 20, 
                       color = "black", 
                       fill="light blue") + 
        ggtitle("Z-score Standardised Amenity Count")
      
    } else if (input$scale_plot_style == "Continuous") {
      r <- filtered_cluster_vars() %>% 
        ggplot(aes(x = .data[[input$scaled_amenity]])) + 
        geom_density(color="black", 
                     fill="light blue") + 
        ggtitle("Non-standardised Amenity Count")
      
      s <- filtered_cluster_vars() %>% 
        normalize() %>% 
        ggplot(aes(x = .data[[input$scaled_amenity]])) + 
        geom_density(color="black", 
                     fill="light blue") + 
        ggtitle("Min-Max Standardised Amenity Count")
      
      z <- filtered_cluster_vars() %>% 
        scale() %>% 
        ggplot(aes(x = .data[[input$scaled_amenity]])) + 
        geom_density(color="black", 
                     fill="light blue") + 
        ggtitle("Z-score Standardised Amenity Count")
    }
    
    ggarrange(r, s, z,
              ncol = 3,
              nrow = 1)
  })
  
  std_amenities_sf <- reactive({
    if (input$scale_method == "None") {
      return(filtered_amenities_sf())
    } else if (input$scale_method == "Min-Max Standardisation") {
      return(filtered_amenities_sf() %>% 
               mutate(across(contains("_count"), 
                             normalize))
      )
    } else if (input$scale_method == "Z-score Standardisation") {
      return(filtered_amenities_sf() %>% 
               mutate(across(contains("_count"), 
                             scale))
      )
    }
  })
  
  std_cluster_vars <- reactive({
    if (input$scale_method == "None") {
      return(filtered_cluster_vars())
    } else if (input$scale_method == "Min-Max Standardisation") {
      return(filtered_cluster_vars() %>% 
               normalize())
    } else if (input$scale_method == "Z-score Standardisation") {
      return(filtered_cluster_vars() %>% 
               scale())
    }
  })
  
  proxmat <- reactive({
    std_cluster_vars() %>% 
      dist(method = input$diss_method)
  })
  
  output$n_cluster_plot <- renderUI({
    if (input$cluster_algo %in% c("Hierarchical", "K-Means", 
                                  "Spatially-constrained Ward Hierarchical", "SKATER")) {
      plotOutput("n_cluster_plot_output")
    }
  })
  
  output$n_cluster_plot_output <- renderPlot({
    if (input$cluster_algo %in% c("Hierarchical", "K-Means", 
                                  "Spatially-constrained Ward Hierarchical", "SKATER")) {
      optimal_clust() %>% 
        plot()
    }
  })
  
  optimal_clust <- reactive({
    if (input$cluster_algo %in% c("Hierarchical", "SKATER")) {
      fviz_nbclust(std_cluster_vars(), 
                   FUN = hcut, 
                   diss = proxmat(),
                   method = input$ncluster_estimation_method, 
                   nstart = input$nstart, 
                   k.max = input$k_max, 
                   nboot = input$nboot, 
                   hc_method = input$hc_method, 
                   hc_metric = input$diss_method,
                   verbose = FALSE)
      
    } else if (input$cluster_algo == "K-Means") {
      fviz_nbclust(std_cluster_vars(), 
                   FUN = kmeans, 
                   diss = proxmat(),
                   method = input$ncluster_estimation_method, 
                   nstart = input$nstart, 
                   k.max = input$k_max, 
                   nboot = input$nboot, 
                   algorithm = input$k_means_algo, 
                   verbose = FALSE)
      
    } else if (input$cluster_algo == "Spatially-constrained Ward Hierarchical") {
      fviz_nbclust(std_cluster_vars(), 
                   FUN = hcut, 
                   diss = proxmat(),
                   method = input$ncluster_estimation_method, 
                   nstart = input$nstart, 
                   k.max = input$k_max, 
                   nboot = input$nboot, 
                   hc_method = input$hcgeo_method, 
                   hc_metric = input$diss_method, 
                   verbose = FALSE)
      
    } else {
      NULL
    }
  })
  
  distmat <- reactive({
    if (input$cluster_algo == "Spatially-constrained Ward Hierarchical") {
      st_distance(std_amenities_sf(), 
                  std_amenities_sf()) %>% 
        as.dist()
    }
  })
  
  output$alpha_plot <- renderUI({
    if (input$cluster_algo == "Spatially-constrained Ward Hierarchical") {
      plotOutput("alpha_plot_output")
    } else {
      NULL
    }
  })
  
  output$alpha_plot_output <- renderPlot({
    if (input$cluster_algo == "Spatially-constrained Ward Hierarchical") {
      choicealpha(proxmat(), distmat(), 
                  range.alpha = seq(0, 1, 0.1), 
                  K = input$k, 
                  graph = TRUE)
    } else {
      NULL
    }
  })
  
  isolated_zones <- reactive({
    if (input$cluster_algo == "SKATER") {
      nb_counts <- std_amenities_sf() %>% 
        as_Spatial() %>% 
        poly2nb() %>% 
        card()
      
      return(
        which(nb_counts == 0)
      )
    }
  })
  
  linked_amenities_sf <- reactive({
    std_amenities_sf() %>% 
      slice(-isolated_zones())
  })
  
  linked_cluster_vars <- reactive({
    std_cluster_vars() %>% 
      slice(-isolated_zones())
  })
  
  skater_stree <- reactive({
    if (input$cluster_algo == "SKATER") {
      nblist <- linked_amenities_sf() %>% 
        as_Spatial() %>% 
        poly2nb()
      
      lcosts <- nbcosts(nblist, 
                        linked_cluster_vars())
      
      if (input$lcost_method == "Row-standardised") {
        weighting_style <- "W"
      } else if (input$lcost_method == "Binary Spatial") {
        weighting_style <- "B"
      } else if (input$lcost_method == "Spatial Contiguity") {
        weighting_style <- "C"
      } else if (input$lcost_method == "Spatial Distance") {
        weighting_style <- "U"
      } else if (input$lcost_method == "Spatial Kernel") {
        weighting_style <- "S"
      } else if (input$lcost_method == "Min-Max Standardised") {
        weighting_style <- "minmax"
      }
      
      return(
        nb2listw(nblist, 
                 lcosts, 
                 style = weighting_style)
      )
    } else {
      NULL
    }
  })
  
  output$skater_stree_summary <- renderUI({
    if (input$cluster_algo == "SKATER") {
      verbatimTextOutput("skater_stree_output")
    } else {
      NULL
    }
  })
  
  output$skater_stree_output <- renderPrint({
    if (input$cluster_algo == "SKATER") {
      skater_stree()$neighbours
    } else {
      NULL
    }
  })
  
  clusters <- reactive({
    clustering <- NULL
    if (input$cluster_algo == "Hierarchical") {
      clustering <- hclust(proxmat(), method = input$hc_method) %>% 
        cutree(k = input$k)
      
    } else if (input$cluster_algo == "K-Means") {
      clustering <- kmeans(proxmat(), 
                           centers = input$k, 
                           algorithm = input$k_means_algo)$cluster
      
    } else if (input$cluster_algo == "Spatially-constrained Ward Hierarchical") {
      clustering <- hclustgeo(proxmat(), 
                              distmat(), 
                              alpha = input$alpha) %>% 
        cutree(k = input$k)
      
    } else if (input$cluster_algo == "SKATER") {
      skater_mstree <- skater_stree() %>% 
        mstree()
      clustering <- skater(edges = skater_mstree[, 1:2], 
                           data = linked_cluster_vars(), 
                           method = input$diss_method, 
                           ncuts = input$k - 1)$groups %>% 
        as.matrix()
      
    }
    
    return(
      clustering %>% 
        factor(levels = sort(unique(as.integer(.))))
    )
  })
  
  amenities_sf_cluster <- reactive({
    if (input$cluster_algo == "SKATER") {
      cbind(linked_amenities_sf(), clusters()) %>%
        rename("Cluster" = "clusters..") %>%
        st_make_valid()
      
    } else {
      cbind(std_amenities_sf(), clusters()) %>%
        rename("Cluster" = "clusters..") %>%
        st_make_valid()
      
    } 
  })
  
  output$cluster_map <- renderTmap({
    tmap_options(main.title = "Amenity Clusters of Singapore")
    
    # Plotting the map
    tm_shape(amenities_sf_cluster()) +
      tm_polygons("Cluster",
                  popup.vars = popup_vars(), 
                  alpha = input$map_color_alpha, 
                  palette = input$map_color_palette, 
                  contrast = input$map_color_contrast, 
                  stretch.palette = input$map_color_stretch, 
                  group = "Overall amenities") +
      tm_basemap("OpenStreetMap") +
      tm_scale_bar() +
      tm_mouse_coordinates()
  })
  
  output$cluster_distribution_plot <- renderPlot({
    if (length(input$amenities_select) > 1){
      amenities_sf_cluster <- amenities_sf_cluster()
      
      original_names <- colnames(amenities_sf_cluster)
      colnames(amenities_sf_cluster) <- colnames(amenities_sf_cluster) %>% 
        str_remove("_count") %>% 
        str_to_title()
      
      ggparcoord(data = amenities_sf_cluster, 
                 columns = which(original_names %in% input$amenities_select), 
                 scale = "globalminmax",
                 showPoints = input$show_distribution_points, 
                 alphaLines = input$distribution_alpha, 
                 shadeBox = if (input$shadebox_colour == "") NULL else input$shadebox_colour, 
                 boxplot = input$incl_boxplot, 
                 title = "Multiple Parallel Coordinates Plots of Amenities Availability by Cluster") + 
        facet_grid(~ Cluster) + 
        xlab("Amenities") + 
        ylab("Amenity Count") +
        theme(axis.text.x = element_text(angle = 30))
      
    } else {
      ggplot(data = amenities_sf_cluster(), 
             aes(x = .data[["Cluster"]], 
                 y = .data[[input$amenities_select]])) + 
        geom_boxplot(color = input$boxplot_outline_color, 
                     fill = input$boxplot_fill_color,
                     outliers = input$boxplot_outlier,
                     outlier.colour = input$boxplot_outlier_colour,
                     outlier.alpha = input$boxplot_outlier_alpha,
                     notch = input$boxplot_notch,
                     notchwidth = input$boxplot_notch_width,
                     coef = input$boxplot_outlier_tolerance) + 
        ylab(input$amenities_select %>% 
               str_remove("_count") %>% 
               str_to_title())
    }
  })
}

# Main server function to call modularized app functions
server <- function(input, output, session) {
  server_app1(input, output, session)
  server_app2(input, output, session)
  server_app3(input, output, session)
}

# Run the app
shinyApp(ui = ui, server = server)

