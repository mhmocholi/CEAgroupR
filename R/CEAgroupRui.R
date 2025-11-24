#' Launch interactive CEAgroupR Shiny interface (refined classic UI)
#'
#' Interactive Shiny application for performing and visualizing
#' multistrategy cost-effectiveness analyses using the CEAgroupR package.
#'
#' @import shiny ggplot2 dplyr MASS DT shinythemes shinyjs readxl
#' @export
CEAgroupRui <- function() {

  # ============================================================
  # UI
  # ============================================================
  ui <- shiny::navbarPage(
    title = "CEAgroupR – Cost-Effectiveness Analysis Toolkit",
    id = "main_nav",
    theme = shinythemes::shinytheme("flatly"),

    header = shiny::tags$head(
      shiny::tags$style(HTML("
        body {background-color: #f8f9fa;}
        .container-fluid {padding: 20px;}
        h3, h4, h5 {font-weight: 600; color: #2c3e50;}
        .box-card {
          background: #ffffff;
          padding: 15px;
          border-radius: 8px;
          box-shadow: 0 1px 5px rgba(0,0,0,0.1);
          margin-bottom: 20px;
        }
        .plot-container {
          background: #ffffff;
          border-radius: 8px;
          box-shadow: 0 1px 6px rgba(0,0,0,0.08);
          padding: 10px;
        }
        .btn-primary {background-color: #007bff; border: none;}
        .btn-primary:hover {background-color: #0069d9;}
      "))
    ),

    ###################################################################
    # TAB 1 — DATA & SETTINGS
    ###################################################################
    shiny::tabPanel(
      "Data & Settings",
      shiny::fluidPage(
        shinyjs::useShinyjs(),
        shiny::h3("Configure your cost-effectiveness analysis"),
        shiny::hr(),

        shiny::fluidRow(

          # ---- Column 1: Data upload ----
          shiny::column(
            width = 4,
            shiny::div(class = "box-card",
                       shiny::h5("1. Data selection"),

                       fileInput("datafile", "Upload Excel file(s):",
                                 accept = c(".xlsx", ".xls"),
                                 multiple = TRUE),

                       selectInput("example_datasets",
                                   "Or use example datasets:",
                                   choices = c("cua_base", "cua_base_discounted",
                                               "cua_multi", "cua_multi_discounted"),
                                   multiple = TRUE),

                       helpText("Upload Excel datasets or choose built-in examples.")
            )
          ),

          # ---- Column 2: Variables ----
          shiny::column(
            width = 4,
            shiny::div(class = "box-card",
                       shiny::h5("2. Variables"),

                       selectInput("group_var", "Group variable:", choices = NULL),
                       selectInput("cost_var",  "Cost variable:",  choices = NULL),
                       selectInput("effect_var","Effect variable:",choices = NULL),

                       selectInput("subgroup_vars", "Subgroup variables:",
                                   choices = NULL, multiple = TRUE),

                       numericInput("R", "Bootstrap replications (R):",
                                    value = 1000, min = 50)
            )
          ),

          # ---- Column 3: Analysis options ----
          shiny::column(
            width = 4,
            shiny::div(class = "box-card",
                       shiny::h5("3. Analysis settings"),

                       textInput("lambda_values", "Lambda values (comma-separated):",
                                 value = "25000"),

                       uiOutput("ref_group_ui"),
                       uiOutput("alt_groups_ui"),

                       selectInput("ci_type", "CI type:",
                                   choices = c("bca", "perc", "basic", "norm")),

                       numericInput("seed", "Random seed:", value = NA),

                       checkboxInput("verbose", "Show progress messages", FALSE),

                       actionButton("run_analysis", "Run full analysis",
                                    class = "btn btn-primary w-100 mt-2")
            )
          )
        ),

        shiny::hr(),
        shiny::div(class = "box-card",
                   shiny::h4("Analysis status"),
                   verbatimTextOutput("analysis_status")
        )
      )
    ),

    ###################################################################
    # TAB 2 — ANALYTICAL RESULTS
    ###################################################################
    shiny::tabPanel(
      "Analytical Results",
      shiny::fluidPage(
        shiny::div(class = "box-card",
                   shiny::h4("Summary statistics"),
                   shiny::downloadButton("download_table", "Download CSV",
                                         class = "btn btn-primary mb-2"),
                   DTOutput("summary_table")
        ),
        shiny::div(class = "box-card",
                   shiny::h4("Reproducible R code"),
                   uiOutput("code_block"))
      )
    ),

    ###################################################################
    # TAB 3 — VISUALIZATIONS
    ###################################################################
    shiny::tabPanel(
      "Visualizations",
      shiny::fluidPage(
        shiny::tabsetPanel(

          #############################
          # ICER
          #############################
          shiny::tabPanel(
            "ICER Plane",
            shiny::fluidRow(

              shiny::column(
                width = 3,
                shiny::div(class = "box-card",
                           shiny::h5("Display options"),

                           selectInput("mode_icer", "Mode:",
                                       choices = c("Overall", "Subgroups")),

                           selectInput("color_by_icer", "Color by:",
                                       choices = c("none","dataset","comparison",
                                                   "subgroup_var","subgroup_level")),

                           selectInput("shape_by_icer", "Shape by:",
                                       choices = c("none","dataset","comparison",
                                                   "subgroup_var","subgroup_level")),

                           selectInput("facet_by_icer", "Facet by:",
                                       choices = c("none","dataset","subgroup_var",
                                                   "subgroup_level","subgroup")),

                           checkboxInput("show_points", "Show bootstrap points", TRUE),
                           checkboxInput("show_means",  "Show mean ICER", TRUE),
                           checkboxInput("show_contours", "Show probability contours", FALSE),

                           selectInput("contour_type", "Contour type:",
                                       choices = c("non_parametric","ellipse")),

                           textInput("filter_icer", "Data filter:", value = ""),
                           textInput("palette_icer", "Palette:", value = "Set2"),

                           shiny::downloadButton("download_icer", "Download (PNG)")
                )
              ),

              shiny::column(
                width = 9,
                shiny::div(class = "plot-container",
                           shiny::plotOutput("plot_icer", height = "650px"))
              )
            )
          ),

          #############################
          # CEAC
          #############################
          shiny::tabPanel(
            "CEAC",
            shiny::fluidRow(

              shiny::column(width = 3,
                            shiny::div(class = "box-card",
                                       shiny::h5("Display options"),

                                       selectInput("mode_ceac", "Mode:",
                                                   choices = c("Overall","Subgroups")),

                                       selectInput("color_by_ceac", "Color by:",
                                                   choices = c("dataset","comparison",
                                                               "subgroup_var","subgroup_level")),

                                       selectInput("shape_by_ceac", "Shape by:",
                                                   choices = c("none","dataset","comparison",
                                                               "subgroup_var","subgroup_level")),

                                       selectInput("facet_by_ceac", "Facet by:",
                                                   choices = c("none","dataset","subgroup_var",
                                                               "subgroup_level","subgroup")),

                                       checkboxInput("show_frontier_ceac",
                                                     "Show frontier curve", FALSE),

                                       numericInput("lambda_steps_ceac", "λ steps:",
                                                    value = 100, min = 10),

                                       textInput("filter_ceac", "Filter:", value = ""),
                                       textInput("palette_ceac", "Palette:", value = "Set2"),

                                       shiny::downloadButton("download_ceac", "Download (PNG)")
                            )
              ),

              shiny::column(
                width = 9,
                shiny::div(class = "plot-container",
                           shiny::plotOutput("plot_ceac", height = "650px"))
              )
            )
          ),

          #############################
          # EVPI
          #############################
          shiny::tabPanel(
            "EVPI",
            shiny::fluidRow(

              shiny::column(width = 3,
                            shiny::div(class = "box-card",
                                       shiny::h5("Display options"),

                                       selectInput("mode_evpi", "Mode:",
                                                   choices = c("Overall","Subgroups")),

                                       selectInput("color_by_evpi", "Color by:",
                                                   choices = c("dataset","comparison",
                                                               "subgroup_var","subgroup_level")),

                                       selectInput("shape_by_evpi", "Shape by:",
                                                   choices = c("none","dataset","comparison",
                                                               "subgroup_var","subgroup_level")),

                                       selectInput("facet_by_evpi", "Facet by:",
                                                   choices = c("none","dataset","subgroup_var",
                                                               "subgroup_level","subgroup")),

                                       numericInput("lambda_steps_evpi", "λ steps:",
                                                    value = 100, min = 10),

                                       textInput("filter_evpi", "Filter:", value = ""),
                                       textInput("palette_evpi", "Palette:", value = "Set2"),

                                       shiny::downloadButton("download_evpi", "Download (PNG)")
                            )
              ),

              shiny::column(
                width = 9,
                shiny::div(class = "plot-container",
                           shiny::plotOutput("plot_evpi", height = "650px"))
              )
            )
          ),

          #############################
          # MARGINALS
          #############################
          shiny::tabPanel(
            "Marginals",
            shiny::fluidRow(

              shiny::column(width = 3,
                            shiny::div(class = "box-card",
                                       shiny::h5("Display options"),

                                       selectInput("mode_marg", "Mode:",
                                                   choices = c("Overall","Subgroups")),

                                       selectInput("marg_variable", "Variable:",
                                                   choices = c("cost","effect")),

                                       selectInput("marg_geom", "Geometry:",
                                                   choices = c("histogram","density","boxplot")),

                                       selectInput("color_by_marg", "Color by:",
                                                   choices = c("dataset","comparison",
                                                               "subgroup_var","subgroup_level")),

                                       selectInput("shape_by_marg", "Shape by:",
                                                   choices = c("none","dataset","comparison",
                                                               "subgroup_var","subgroup_level")),

                                       selectInput("facet_by_marg", "Facet by:",
                                                   choices = c("none","dataset","subgroup_var",
                                                               "subgroup_level","subgroup")),

                                       numericInput("bins_marg", "Histogram bins:",
                                                    value = 30, min = 5),

                                       textInput("filter_marg", "Filter:", value = ""),
                                       textInput("palette_marg", "Palette:", value = "Set2"),

                                       shiny::downloadButton("download_marg", "Download (PNG)")
                            )
              ),

              shiny::column(width = 9,
                            shiny::div(class = "plot-container",
                                       shiny::plotOutput("plot_marginals", height = "650px")))
            )
          )
        )
      )
    )
  )

  # ============================================================
  # SERVER
  # ============================================================
  server <- function(input, output, session) {

    results <- shiny::reactiveVal(NULL)

    shinyjs::disable("run_analysis")
    shinyjs::disable(selector = "a[data-value='Analytical Results']")
    shinyjs::disable(selector = "a[data-value='Visualizations']")

    #####################################################################
    # 1. Load data + detect columns
    #####################################################################
    observeEvent(
      list(input$datafile, input$example_datasets),
      {

        data_preview <- NULL

        if (!is.null(input$datafile)) {
          data_preview <- readxl::read_excel(input$datafile$datapath[1], sheet = 1)
        } else if (length(input$example_datasets) > 0) {
          data_preview <- get(input$example_datasets[1])
        }

        if (!is.null(data_preview)) {

          cols <- names(data_preview)

          updateSelectInput(session, "group_var",  choices = cols, selected = cols[1])
          updateSelectInput(session, "cost_var",   choices = cols, selected = cols[2])
          updateSelectInput(session, "effect_var", choices = cols, selected = cols[3])

          updateSelectInput(session, "subgroup_vars", choices = cols)

          shinyjs::enable("run_analysis")
        }
      }
    )

    #####################################################################
    # 2. Reference & alternative group detection
    #####################################################################
    observeEvent(input$group_var, {

      req(input$group_var)

      data_preview <- NULL
      if (!is.null(input$datafile)) {
        data_preview <- readxl::read_excel(input$datafile$datapath[1], sheet = 1)
      } else if (length(input$example_datasets) > 0) {
        data_preview <- get(input$example_datasets[1])
      }

      if (!is.null(data_preview)) {

        levs <- sort(unique(as.character(data_preview[[input$group_var]])))

        output$ref_group_ui <- renderUI({
          selectInput("ref_group", "Reference group:",
                      choices = levs, selected = levs[1])
        })

        output$alt_groups_ui <- renderUI({
          selectInput("alt_groups", "Alternative strategies:",
                      choices = levs[-1], selected = levs[-1], multiple = TRUE)
        })
      }
    })

    #####################################################################
    # 3. Run analysis
    #####################################################################
    observeEvent(input$run_analysis, {

      req(input$group_var, input$cost_var, input$effect_var, input$ref_group)

      withProgress(message = "Running analysis...", value = 0.5, {

        # Load multiple Excel or example datasets
        if (!is.null(input$datafile)) {
          data_list <- lapply(input$datafile$datapath, readxl::read_excel)
          names(data_list) <- tools::file_path_sans_ext(basename(input$datafile$name))
        } else {
          data_list <- lapply(input$example_datasets, get)
          names(data_list) <- input$example_datasets
        }

        # Parse lambda values
        lambda_vals <- as.numeric(unlist(strsplit(input$lambda_values, ",\\s*")))
        lambda_vals <- lambda_vals[!is.na(lambda_vals)]
        if (length(lambda_vals) == 0) lambda_vals <- 25000

        # Compute
        res <- compute_icers(
          data = data_list,
          group = input$group_var,
          cost = input$cost_var,
          effect = input$effect_var,
          R = input$R,
          lambda = lambda_vals,
          ci_type = input$ci_type,
          subgroup_vars = input$subgroup_vars,
          seed = if (!is.na(input$seed)) input$seed else NULL,
          verbose = input$verbose,
          ref_group = input$ref_group,
          alt_groups = input$alt_groups
        )

        results(res)
      })

      shinyjs::enable(selector = "a[data-value='Analytical Results']")
      shinyjs::enable(selector = "a[data-value='Visualizations']")
      showNotification("Analysis completed successfully!", type = "message", duration = 3)
    })

    #####################################################################
    # 4. Summary stats
    #####################################################################
    output$summary_table <- DT::renderDT({
      req(results())
      df <- results()$summary_stats
      numeric_cols <- sapply(df, is.numeric)
      df[numeric_cols] <- lapply(df[numeric_cols], round, 3)
      DT::datatable(
        df,
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })

    #####################################################################
    # 5. Visualization logic (clean integration)
    #####################################################################

    ## === ICER ===
    output$plot_icer <- renderPlot({
      req(results())
      plot.icers(
        results(),
        mode        = input$mode_icer,
        color_by    = input$color_by_icer,
        shape_by    = input$shape_by_icer,
        facet_by    = input$facet_by_icer,
        filter_expr = input$filter_icer,
        show_points = input$show_points,
        show_means  = input$show_means,
        show_contours = input$show_contours,
        contour_type  = input$contour_type,
        palette       = input$palette_icer
      )
    })

    ## === CEAC ===
    output$plot_ceac <- renderPlot({
      req(results())
      plot.ceacs(
        results(),
        mode         = input$mode_ceac,
        color_by     = input$color_by_ceac,
        shape_by     = input$shape_by_ceac,
        facet_by     = input$facet_by_ceac,
        show_frontier = input$show_frontier_ceac,
        lambda_steps  = input$lambda_steps_ceac,
        filter_expr   = input$filter_ceac,
        palette       = input$palette_ceac
      )
    })

    ## === EVPI ===
    output$plot_evpi <- renderPlot({
      req(results())
      plot.evpis(
        results(),
        mode         = input$mode_evpi,
        color_by     = input$color_by_evpi,
        shape_by     = input$shape_by_evpi,
        facet_by     = input$facet_by_evpi,
        lambda_steps = input$lambda_steps_evpi,
        filter_expr  = input$filter_evpi        ,
        palette      = input$palette_evpi
      )
    })

    ## === MARGINALS ===
    output$plot_marginals <- renderPlot({
      req(results())
      plot.marginals(
        results(),
        variable     = input$marg_variable,
        geom_type    = input$marg_geom,
        mode         = input$mode_marg,
        color_by     = input$color_by_marg,
        shape_by     = input$shape_by_marg,
        facet_by     = input$facet_by_marg,
        bins         = input$bins_marg,
        filter_expr  = input$filter_marg,
        palette      = input$palette_marg
      )
    })

    #####################################################################
    # 6. Download Handlers
    #####################################################################

    output$download_icer <- downloadHandler(
      filename = function() { "icer_plot.png" },
      content = function(file) {
        png(file, width = 1600, height = 1200, res = 150)
        print(
          plot.icers(
            results(),
            mode        = input$mode_icer,
            color_by    = input$color_by_icer,
            shape_by    = input$shape_by_icer,
            facet_by    = input$facet_by_icer,
            filter_expr = input$filter_icer,
            show_points = input$show_points,
            show_means  = input$show_means,
            show_contours = input$show_contours,
            contour_type  = input$contour_type,
            palette       = input$palette_icer
          )
        )
        dev.off()
      }
    )

    output$download_ceac <- downloadHandler(
      filename = function() { "ceac_plot.png" },
      content = function(file) {
        png(file, width = 1600, height = 1200, res = 150)
        print(
          plot.ceacs(
            results(),
            mode         = input$mode_ceac,
            color_by     = input$color_by_ceac,
            shape_by     = input$shape_by_ceac,
            facet_by     = input$facet_by_ceac,
            show_frontier = input$show_frontier_ceac,
            lambda_steps  = input$lambda_steps_ceac,
            filter_expr   = input$filter_ceac,
            palette       = input$palette_ceac
          )
        )
        dev.off()
      }
    )

    output$download_evpi <- downloadHandler(
      filename = function() { "evpi_plot.png" },
      content = function(file) {
        png(file, width = 1600, height = 1200, res = 150)
        print(
          plot.evpis(
            results(),
            mode         = input$mode_evpi,
            color_by     = input$color_by_evpi,
            shape_by     = input$shape_by_evpi,
            facet_by     = input$facet_by_evpi,
            lambda_steps = input$lambda_steps_evpi,
            filter_expr  = input$filter_evpi,
            palette      = input$palette_evpi
          )
        )
        dev.off()
      }
    )

    output$download_marg <- downloadHandler(
      filename = function() { "marginals_plot.png" },
      content = function(file) {
        png(file, width = 1600, height = 1200, res = 150)
        print(
          plot.marginals(
            results(),
            variable     = input$marg_variable,
            geom_type    = input$marg_geom,
            mode         = input$mode_marg,
            color_by     = input$color_by_marg,
            shape_by     = input$shape_by_marg,
            facet_by     = input$facet_by_marg,
            bins         = input$bins_marg,
            filter_expr  = input$filter_marg,
            palette      = input$palette_marg
          )
        )
        dev.off()
      }
    )

    #####################################################################
    # 7. Summary table download
    #####################################################################
    output$download_table <- downloadHandler(
      filename = function() { "summary_stats.csv" },
      content = function(file) {
        req(results())
        write.csv(results()$summary_stats, file, row.names = FALSE)
      }
    )

  } # end server

  # ============================================================
  # Launch App
  # ============================================================
  shiny::shinyApp(ui, server)
}

