# GRAPH CONSTRUCTION -----------------------------------------------------------------
# geom ------------------------------------------------------------------------
tab_panel_geom <- 
  tabPanel(title = "Geometry",
           selectInput(inputId = "viztype",
                       label = "Type of graph (Geom used)",
                       choices = cnf$choices$geoms
           ),
           prettySwitch(inputId = "alpha_05",
                        label = "Make semitransparent",
                        value = FALSE,
                        status = "primary", slim = TRUE),
           hr(),
           selectInput(
             "x_var", 
             HTML(paste("X-variable", icon("arrows-alt-h"))), 
             choices = ""
           ),
           tags$div(
             id = "toggle_y_var",
             selectInput(
               "y_var", 
               HTML(paste("Y-variable", icon("arrows-alt-v"))), 
               choices = ""
             )
           ),
           hr(),
           selectInput("group", "Group - Colour", choices = ""),
           tags$div(
             id = "toggle_position",
             radioGroupButtons(
               inputId = "position",
               label = "Geom grouping position", 
               choices = cnf$choices$positions
             )
           ),
           tags$div(
             id = "toggle_size",
             selectInput("size", "Group - Size", choices = "")
           ),
           tags$div(
             id = "toggle_shape",
             selectInput("shape", "Group - Shape", choices = "")
           )
)

# stats ------------------------------------------------------------------------
tab_panel_stats <- tabPanel(
  title = "Stats",
  tags$div(
    id = "toggle_regression",
    prettySwitch(inputId = "show_regression_line",
                 label = "Show regression line",
                 value = FALSE,
                 status = "primary", slim = TRUE)
  ),
  tags$div(
    id = "toggle_smooth_func",
    selectInput("smooth_func", 
                "Smoothening function",
                choices = cnf$choices$regression)
  ),
  tags$div(
    id = "toggle_bins",
    sliderInput("bins", 
                "Number of histogram bins", 
                min = 10, max = 100, value = 30, step = 1)
  )
                            
)

# coord ------------------------------------------------------------------------
tab_panel_coord <- tabPanel(
  title = "Coordinates",
  selectInput("coord","Coordinate type", 
              choices = cnf$choices$coordinates),
  tags$div(
    id = "toggle_coord_thetha",
    radioGroupButtons(
      inputId = "polar_coord_type",
      label = "Polar coordinate axis", 
      choices = cnf$choices$coordinates_axis,
      selected = cnf$choices$coordinates_axis[2]
    )
  ),
  tags$div(
    id = "toggle_coord_origin",
    prettySwitch(inputId = "include_x_origin",
                 label = "Force inclusion of X axis origin (zero)",
                 value = FALSE,
                 status = "primary", slim = TRUE),
    prettySwitch(inputId = "include_y_origin",
                 label = "Force inclusion of Y axis origin (zero)",
                 value = FALSE,
                 status = "primary", slim = TRUE)
  )
)

# facet ------------------------------------------------------------------------
tab_panel_facet <- tabPanel(
  title = "Facet",
  radioGroupButtons(
    inputId = "facet_type",
    label = "Facet Type", 
    choices = cnf$choices$facets
  ),
  selectInput("facet_row", "Facet Row", choices = ""),
  selectInput("facet_col", "Facet Column", choices = "")
)

# labels -----------------------------------------------------------------------
tab_panel_label <- tabPanel(
  title = "Labels",
  textInput("lab_title", "Title:", value = NA),
  textInput("lab_x", "X-axis:", value = NA),
  textInput("lab_y", "Y-axis:", value = NA),
  textInput("lab_caption", "Caption:", value = NA)
                            
)

# theme ------------------------------------------------------------------------
available_palettes <- list(
  "Default" = names(cnf$palettes$custom$default),
  "Categorical" = c(
    cnf$palettes$rcolorbrewer$categorical, 
    names(cnf$palettes$custom$categorical)
  ), 
  "Continuous - Diverging" = cnf$palettes$rcolorbrewer$diverging,
  "Continuous - Sequential" = cnf$palettes$rcolorbrewer$sequential
)

tab_panel_theme <- tabPanel(
  title = "Theme",
  selectInput("color_palette", "Color palette",
              choices = available_palettes),
  selectInput("theme_style", "Theme Style",
              choices = cnf$choices$themes,
              selected = "theme_grey()"),
  selectInput("pos_legend", "Position legend",
              choices = cnf$choices$legend_positions
  ),
  prettySwitch(inputId = "show_gridlines",
               label = "Show gridlines",
               value = TRUE,
               status = "primary", slim = TRUE),
  prettySwitch(inputId = "show_x_axis",
               label = "Show X axis",
               value = TRUE,
               status = "primary", slim = TRUE),
  prettySwitch(inputId = "show_y_axis",
               label = "Show Y axis",
               value = TRUE,
               status = "primary", slim = TRUE)
                            
)

# BOXES ----------------------------------------------------------------
box_upload <- 
  box(width = 12, 
      title = "Data",
      selectInput("preload_select", 
                  "Choose preloaded dataset",
                  names(cnf$datasets)
                  ),
      fileInput("file_input", 
                "Or upload file",
                accept = c(
                  "text/csv", 
                  "application/vnd.ms-excel", 
                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                  ),
                placeholder = "CSV or EXCEL file"
                )
      )

box_continuous_categorical <- 
  box(width = 12, 
      title = "Continuous and Categorical variables",
      collapsible = TRUE, 
      collapsed = TRUE,
      multiInput(
        inputId = "which_cat", 
        label = NULL,
        choices = c(""),
        selected = NA,
        options(
          enable_search = FALSE,
          non_selected_header = 'All variables:',
          selected_header = 'Categorical:'
        )
      )
      
  )

box_data_loaded <- 
  box(width = 12, 
      title = "Data loaded",
      collapsible = TRUE, 
      collapsed = FALSE,
      DT::DTOutput("out_table")
  )

box_code <- 
  box(width = 8, 
      title = "Code within", 
      collapsible = TRUE, 
      collapsed = TRUE,
      aceEditor("ace_graph", height = 200, mode = "r", wordWrap = TRUE)
  )

box_download <- 
  box(width = 4,
      title = "Download",
      collapsible = TRUE, 
      collapsed = TRUE,
      sliderInput("fig_height_download", 
                  "Plot height", 
                  min = 3, max = 30, value = 14, 
                  step = 1, post = " cm"),
      sliderInput("fig_width_download",
                  "Plot width", 
                  min = 3, max = 30, value = 14, 
                  step = 1, post = " cm"),
      downloadButton(
        "download_plot_PDF", "Download"
      )
  )

# PUT TOGETHER -----------------------------------------------------------------
dashboardPage(
  dashboardHeader(title = "ggplot VIZ"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    fluidRow(
      column(width = 4,
             box_upload,
             box_continuous_categorical,
             tabBox(width = 12,
                    tab_panel_geom,
                    tab_panel_stats,
                    tab_panel_facet,
                    tab_panel_coord,
                    tab_panel_label,
                    tab_panel_theme
             )
             ),
      column(width = 8,
             box_data_loaded,
             box(width = 12,
                 plotOutput("out_ggplot")
             ),
             box_code,
             box_download
             )
      )
    )
  )
