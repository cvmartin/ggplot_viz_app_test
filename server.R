server <- function(input, output, session) {
# INPUT ------------------------------------------------------------------------
  df_initial <- reactiveVal()
  
  observeEvent(input$preload_select, {
    df_initial(
      eval(cnf$datasets[[input$preload_select]])
    )
  })
  
  observeEvent(input$file_input, {
    req(input$file_input)
    file <- input$file_input$datapath
    extension <- tools::file_ext(file)
    
    df <- if (extension == "csv"){
      readr::read_csv(file)
    } else if (extension %in% c("xls", "xlsx")){
      readxl::read_excel(file)
    } else {
      showNotification(
        "Input a `.csv`, `.xls` or `.xlsx` file",
        type = "error")
    }
    
    req(extension %in% c("csv", "xls", "xlsx"))
    
    df_initial(df)
  })
  
  # all variables / initially identified as continuous / categorical
  # vars_all <- reactive(names(df_initial()))
  # initial_vars_cont <- reactive(names(discard(df_initial(), is_categorical)))
  # initial_vars_cate <- reactive()
  
  # use input of `which_cat` to see what are user-defined as continuous / categorical
  final_vars_cont <- reactive(
    names(df_initial())[!(names(df_initial()) %in% input$which_cat)]
  )
  final_vars_cate <- reactive(input$which_cat)

  df_final <- reactive({
    isolate(df_initial()) %>% 
      mutate_at(vars(final_vars_cate()), as.factor)
  })

# BUILD CODE ----------------------------------------------------------
  code_graph <- reactive({
    
    geom_is_filled <- input$viztype %in% cnf$specials$geoms$require_fill
    
    # aesthetics ----
    # a list of expressions for mapping of `x`, `y`, `color`, `shape`, `size`
    aesthetics <- exprs(y = !!sym(input$y_var))
    
    if (input$x_var != "NA"){
      aesthetics$x <- expr(!!sym(input$x_var))
    } else {
      aesthetics$x <- expr(NA)
    }
      
    if (input$group != "NA") {
      if (geom_is_filled){
        aesthetics$fill <- expr(!!sym(input$group))
      } else {
        aesthetics$color <- expr(!!sym(input$group))
      }
    }
    
    if ((input$viztype %in% cnf$specials$geoms$allow_size)
        && (input$size != "NA")){
      aesthetics$size <- expr(!!sym(input$size))
    }
    
    if ((input$viztype %in% cnf$specials$geoms$allow_shape)
        && (input$shape != "NA")){
      aesthetics$shape <- expr(!!sym(input$shape))
    }
  
    if (input$viztype %in% cnf$specials$geoms$deactivate_y){
      showNotification("Histograms do not use the Y axis")
      aesthetics$y <- NULL
    }
    
    first_line_code <- expr(
      ggplot2::ggplot(df, aes(!!!aesthetics))
      )
    
    # geom ----
    geom_code <- {
      geom_args <- exprs()
      
      if (input$viztype %in% cnf$specials$geoms$allow_bins){
        geom_args$bins <- expr(!!input$bins)
      } 
      
      if (input$viztype %in% cnf$specials$geoms$allow_position){
        geom_args$position <- expr(!!input$position)
      } 
      
      if (input$alpha_05 == TRUE) geom_args$alpha <- expr(0.5)
      
      expr(
        (!!sym(input$viztype))(!!!geom_args)
      )
    }

    # stats ----
    stats_code <- if ((input$viztype %in% cnf$specials$geoms$allow_regression)
      && (input$show_regression_line == TRUE)){
      expr(stat_smooth(method=!!input$smooth_func))
    }

    # facet ----
    facet_code <- if (input$facet_row != "." || input$facet_col != "."){
      expr(
        (!!sym(input$facet_type))(!!sym(input$facet_row) ~ !!sym(input$facet_col))
      )
    }

    # coord ----
    coord_code <- {
      coord_args <- exprs()
      
      if (input$coord %in% cnf$specials$coord$allow_origin){
        if (input$include_x_origin == TRUE){
          coord_args$xlim <- expr(c(0,max(df[[!!input$x_var]])))
        }
        if (input$include_y_origin == TRUE){
          coord_args$ylim <- expr(c(0,max(df[[!!input$y_var]])))
        }
      }
      
      if (input$coord %in% cnf$specials$coord$allow_thetha){
        coord_args$theta <- expr(!!input$polar_coord_type)
      }
      
      if (input$coord == "coord_cartesian" && is_empty(coord_args)){
        NULL
      } else {
        expr(
          (!!sym(input$coord))(!!!coord_args)
        )
      }
    }
    
    # labs ----
    labs_code <- {
      labs_args <- exprs()
      if (input$lab_title != "") labs_args$title <- expr(!!input$lab_title)
      if (input$lab_x != "") labs_args$x <- expr(!!input$lab_x)
      if (input$lab_y != "") labs_args$y <- expr(!!input$lab_y)
      if (input$lab_caption != "") labs_args$caption <- expr(!!input$lab_caption)
      
      if (!is_empty(labs_args)){
        expr(labs(!!!labs_args))
      }
    }
    
    # theme: style ----
    theme_style_code <- if (input$theme_style != "theme_grey") {
      expr(
        (!!sym(input$theme_style))()
      )
    }
      

    # theme: palette ----
    theme_palette_code <- if (input$group != "NA" && input$color_palette != "ggplot2 default") {
      palette_args <- exprs()
      
      custom_palettes <- flatten(cnf$palettes$custom)
      
      if (input$color_palette %in% names(custom_palettes)){
        palette_args$values <- expr(
          !!(custom_palettes[[input$color_palette]])
        )
        funcion_name_suffix <- "manual"
      } else {
        palette_args$palette <- expr(!!input$color_palette)
        funcion_name_suffix <- ifelse(
          input$group %in% final_vars_cate(), 
          "brewer", 
          "distiller"
        )
      }
      
      function_name <- paste(
        "scale", 
        ifelse(geom_is_filled, "fill", "color"), 
        funcion_name_suffix, 
        sep = "_"
      )
      
      expr(
        (!!sym(function_name))(!!!palette_args)
      )
    }
    
    # theme: elements ----
    theme_elements_code <- {
      theme_elements_args <- exprs()
      
      if (input$pos_legend != "right"){
        theme_elements_args$legend.position <- expr(!!input$pos_legend)
      } 
      
      if (input$show_gridlines == FALSE){
        theme_elements_args$panel.grid.major <- expr(element_blank())
        theme_elements_args$panel.grid.minor <- expr(element_blank())
      }
      
      if (input$show_x_axis == FALSE){
        theme_elements_args$axis.title.x <- expr(element_blank())
        theme_elements_args$axis.text.x <- expr(element_blank())
        theme_elements_args$axis.ticks.x <- expr(element_blank())
      }
      
      if (input$show_y_axis == FALSE){
        theme_elements_args$axis.title.y <- expr(element_blank())
        theme_elements_args$axis.text.y <- expr(element_blank())
        theme_elements_args$axis.ticks.y <- expr(element_blank())
      }
      
      if (!is_empty(theme_elements_args)){
        expr(theme(!!!theme_elements_args))
      }
    }
    
    # ...build from parts...----
    all_layers_code <- list(
      first_line_code,
      geom_code,
      facet_code,
      stats_code,
      coord_code,
      labs_code,
      theme_style_code,
      theme_palette_code,
      theme_elements_code
      )

    all_layers_code %>% 
      discard(is_empty) %>% 
      # take all non-empty parts of `all_layers_code` and put a `+` between them
      reduce(function(x,y) expr(!!enexpr(x) + !!enexpr(y)))
  })
  
  # OUTPUT -----------------------------------------------------------------------
  # table -------
  output$out_table <- DT::renderDT(
    DT::datatable(
      df_final(),
      rownames = FALSE,
      class = "row-border",
      options = list(
        scrollX = TRUE,
        pageLength = 8,
        dom = "rtip"
      )
    )
  )
  
  # graph -------
  evaluated_graph <- eventReactive(input$ace_graph,{
    
    req(input$ace_graph)
    
    eval_tidy(
      parse_expr(input$ace_graph), 
      data = list(df = df_final()), 
      env = safe_ggplot_env
      )
  })
  
  output$out_ggplot <- renderPlot(evaluated_graph())

  # download pdf ------
  output$download_plot_PDF <- downloadHandler(
    filename = function(){
      paste("figure_ggplotVIZ_", Sys.time(), ".pdf", sep = "")
    },
    content = function(file){
      ggsave(file, 
             evaluated_graph(), 
             width = input$fig_width_download,
             height = input$fig_height_download, 
             units = "cm")
    },
    contentType = "application/pdf"
  )

  # OBSERVERS --------------------------------------------------------------------
  # update initial categorical / continuous vars in `which_cat` -----
  observeEvent(df_initial(), {
    updateMultiInput(session, "which_cat",
                     choices = names(df_initial()),
                     selected = names(keep(df_initial(), is_categorical))
    )
  })
  
  # update with output of `which_cat` -----------------------------
  observeEvent(df_final(),{

    choices_cont <- from_vars_to_choice_list(final_vars_cont())
    choices_cate <-  from_vars_to_choice_list(final_vars_cate())
    
    updateSelectInput(session, "x_var", choices = list(
      Continuous = choices_cont,
      Categorical = choices_cate,
      `Choose no variable` = list(NA)
    ))
    
    updateSelectInput(
      session, "y_var", 
      choices = list(Continuous = choices_cont),
      selected = ifelse(length(choices_cont) > 1, choices_cont[2], choices_cont[1])
      )
    
    updateSelectInput(session, "group",  choices = list(
      `Choose no variable` = list(NA),
      Continuous = choices_cont,
      Categorical = choices_cate
    ))
    
    updateSelectInput(session, "size",  choices = list(
      `Choose no variable` = list(NA),
      Continuous = choices_cont
    ))
    
    updateSelectInput(session, "shape",  choices = list(
      `Choose no variable` = list(NA),
      Categorical = choices_cate
    ))
    
    updateSelectInput(session, "facet_row",  choices = list(
      `Choose no variable` = list("NA" = "."),
      Categorical = choices_cate
    ))
    
    updateSelectInput(session, "facet_col",  choices = list(
      `Choose no variable` = list("NA" = "."),
      Categorical = choices_cate
    ))
  })
  
  # update text in ACE editor -------
  observeEvent(code_graph(), {
    updateAceEditor(
      session = session, 
      editorId = "ace_graph", 
      expr_text(code_graph())
    )
  })
  
  # toggle content in UI -------------
  observe({
    toggle("toggle_y_var", 
           condition = !(input$viztype %in% cnf$specials$geoms$deactivate_y), 
           anim = TRUE)
  })
  
  observe({
    toggle("toggle_size", 
           condition = input$viztype %in% cnf$specials$geoms$allow_size, 
           anim = TRUE)
  })
  
  observe({
    toggle("toggle_shape", 
           condition = input$viztype %in% cnf$specials$geoms$allow_shape, 
           anim = TRUE)
  })
  
  observe({
    toggle("toggle_position", 
           condition = input$viztype %in% cnf$specials$geoms$allow_position, 
           anim = TRUE)
  })
  
  observe({
    toggle("toggle_regression", 
           condition = input$viztype %in% cnf$specials$geoms$allow_regression, 
           anim = TRUE)
  })
  
  observe({
    toggle("toggle_smooth_func", 
           condition = (input$viztype %in% cnf$specials$geoms$allow_regression 
                        && input$show_regression_line), 
           anim = TRUE)
  })
  
  observe({
    toggle("toggle_bins", 
           condition = input$viztype %in% cnf$specials$geoms$allow_bins, 
           anim = TRUE)
  })

  observe({
    toggle("toggle_coord_thetha", 
           condition = input$coord %in% cnf$specials$coord$allow_thetha, 
           anim = TRUE)
  })
  
  observe({
    toggle("toggle_coord_origin", 
           condition = input$coord %in% cnf$specials$coord$allow_origin, 
           anim = TRUE)
  })
  
}