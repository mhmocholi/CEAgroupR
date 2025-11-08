#' Launch interactive CEAgroupR Shiny interface (modern UI)
#'
#' Interactive Shiny application for performing and visualizing
#' cost-effectiveness analyses using the \pkg{CEAgroupR} package.
#'
#' @import shiny ggplot2 dplyr MASS DT shinythemes shinyjs readxl rmarkdown knitr
#' @export
CEAgroupRui <- function() {

  ui <- shiny::navbarPage(
    title = "CEAgroupR – Cost-Effectiveness Analysis Toolkit",
    id = "main_nav",
    theme = shinythemes::shinytheme("flatly"),

    # Custom CSS for modern layout
    header = shiny::tags$head(
      shiny::tags$style(HTML("
        body {background-color: #f8f9fa;}
        .container-fluid {padding: 20px;}
        h3, h4, h5 {font-weight: 600; color: #2c3e50;}
        .well {background-color: #fefefe; border-left: 4px solid #007bff;}
        .btn-success {background-color: #007bff; border: none;}
        .btn-success:hover {background-color: #0069d9;}
        .box-card {background: #ffffff; padding: 15px; border-radius: 8px;
                   box-shadow: 0 1px 5px rgba(0,0,0,0.1); margin-bottom: 20px;}
        .plot-container {background: #ffffff; border-radius: 8px;
                         box-shadow: 0 1px 6px rgba(0,0,0,0.08); padding: 10px;}
      "))
    ),

    # ---- TAB 1: DATA & SETTINGS ----
    shiny::tabPanel(
      title = "Data & Settings",
      shiny::fluidPage(
        shinyjs::useShinyjs(),
        shiny::h3("Configure your cost-effectiveness analysis"),
        shiny::hr(),

        shiny::fluidRow(
          # --- Columna 1: Data selection ---
          shiny::column(
            width = 4,
            shiny::div(class = "box-card",
                       shiny::h5("1. Data selection"),
                       shiny::fileInput("datafile", "Upload Excel file(s):",
                                        accept = c(".xlsx", ".xls"), multiple = TRUE),
                       shiny::selectInput("example_datasets", "Or use example datasets:",
                                          choices = c("cua_base", "cua_base_discounted"),
                                          selected = NULL, multiple = TRUE),
                       shiny::helpText("Upload your dataset or choose a built-in example to test the app."))
          ),

          # --- Columna 2: Basic configuration ---
          shiny::column(
            width = 4,
            shiny::div(class = "box-card",
                       shiny::h5("2. Basic analysis settings"),
                       shiny::selectInput("group_var", "Group variable:", choices = NULL),
                       shiny::selectInput("cost_var", "Cost variable:", choices = NULL),
                       shiny::selectInput("effect_var", "Effect variable:", choices = NULL),
                       shiny::selectInput("subgroup_vars", "Subgroup variables (optional):",
                                          choices = NULL, multiple = TRUE, selectize = TRUE),
                       shiny::numericInput("R", "Bootstrap replications (R):", value = 1000, min = 100),
                       shiny::textInput("lambda_values", "Lambda values (comma-separated):",
                                        value = "25000", placeholder = "e.g., 25000,35000,50000"),
                       shiny::actionButton("run_analysis", "Run full analysis",
                                           icon = shiny::icon("play"), class = "btn-success", width = "100%")
            )
          ),

          # --- Columna 3: Advanced options ---
          shiny::column(
            width = 4,
            shiny::div(class = "box-card",
                       shiny::h5("3. Advanced options"),
                       shiny::selectInput("ci_type", "Confidence interval type:",
                                          choices = c("bca", "perc", "basic", "norm"),
                                          selected = "bca"),
                       shiny::uiOutput("ref_group_ui"),
                       shiny::numericInput("seed", "Random seed (optional):", value = NA),
                       shiny::checkboxInput("verbose", "Show progress messages (verbose)", value = FALSE),
                       shiny::helpText("Control bootstrap confidence interval type, reference group, and reproducibility.")
            )
          )
        ),

        shiny::hr(),
        shiny::div(style = "text-align:center;",
                   shiny::verbatimTextOutput("analysis_status"))
      )
    ),

    # ---- TAB 2: ANALYTICAL RESULTS ----
    shiny::tabPanel(
      title = "Analytical Results",
      shiny::fluidPage(
        shiny::div(class = "box-card",
                   shiny::h4("Summary statistics of input data"),
                   shiny::downloadButton("download_table", "Download Table (CSV)",
                                         class = "btn-primary", style = "margin-bottom:10px;"),
                   DT::DTOutput("summary_table")),
        shiny::div(class = "box-card",
                   shiny::h4("Reproducible R code"),
                   shiny::uiOutput("code_block"),
                   shiny::actionButton("copy_code", "Copy to Clipboard",
                                       icon = shiny::icon("clipboard")))
      )
    ),

    # ---- TAB 3: ANALYSIS CONFIGURATION ----
    shiny::tabPanel(
      title = "Analysis Configuration",
      shiny::fluidPage(
        shiny::div(class = "box-card",
                   shiny::h4("Analysis Parameters and Settings"),
                   shiny::tableOutput("settings_table"))
      )
    ),

    # ---- TAB 4: VISUALIZATIONS ----
    shiny::tabPanel(
      title = "Visualizations",
      shiny::fluidPage(
        shiny::tabsetPanel(
          id = "vis_tabs",
          type = "tabs",

          # === ICER ===
          shiny::tabPanel(
            "ICER Plane",
            shiny::fluidRow(
              shiny::column(
                width = 3,
                shiny::div(class = "box-card",
                           shiny::h5("Display options"),
                           shiny::selectInput("mode_icer", "Mode:",
                                              choices = c("Overall", "Subgroups", "Full"), selected = "Full"),
                           shiny::selectInput("color_by", "Color by:",
                                              choices = c("none", "dataset", "subgroup_var", "subgroup_level")),
                           shiny::selectInput("shape_by", "Shape by:",
                                              choices = c("none", "dataset", "subgroup_var", "subgroup_level")),
                           shiny::selectInput("facet_by", "Facet by:",
                                              choices = c("none", "dataset", "subgroup_var", "subgroup_level")),
                           shiny::checkboxInput("show_points", "Show bootstrap points", TRUE),
                           shiny::checkboxInput("show_means", "Show mean ICER points", TRUE),
                           shiny::checkboxInput("show_contours", "Show probability contours", FALSE),
                           shiny::selectInput("contour_type", "Contour type:",
                                              choices = c("non_parametric", "ellipse")),
                           shiny::downloadButton("download_icer", "Download plot (PNG)")
                )
              ),
              shiny::column(width = 9,
                            shiny::div(class = "plot-container",
                                       shiny::plotOutput("plot_icer", height = "600px"))))
          ),

          # === CEAC ===
          shiny::tabPanel(
            "CEAC",
            shiny::fluidRow(
              shiny::column(width = 3,
                            shiny::div(class = "box-card",
                                       shiny::h5("Display options"),
                                       shiny::selectInput("mode_ceac", "Mode:",
                                                          choices = c("Overall", "Subgroups", "Full"), selected = "Full"),
                                       shiny::selectInput("color_by_ceac", "Color by:",
                                                          choices = c("none", "dataset", "subgroup_var", "subgroup_level")),
                                       shiny::selectInput("facet_by_ceac", "Facet by:",
                                                          choices = c("none", "dataset", "subgroup_var")),
                                       shiny::numericInput("lambda_steps_ceac", "Number of λ steps:", value = 100, min = 10),
                                       shiny::downloadButton("download_ceac", "Download plot (PNG)"))),
              shiny::column(width = 9,
                            shiny::div(class = "plot-container",
                                       shiny::plotOutput("plot_ceac", height = "600px"))))
          ),

          # === EVPI ===
          shiny::tabPanel(
            "EVPI",
            shiny::fluidRow(
              shiny::column(width = 3,
                            shiny::div(class = "box-card",
                                       shiny::h5("Display options"),
                                       shiny::selectInput("mode_evpi", "Mode:",
                                                          choices = c("Overall", "Subgroups", "Full"), selected = "Full"),
                                       shiny::selectInput("color_by_evpi", "Color by:",
                                                          choices = c("none", "dataset", "subgroup_var", "subgroup_level")),
                                       shiny::selectInput("facet_by_evpi", "Facet by:",
                                                          choices = c("none", "dataset", "subgroup_var")),
                                       shiny::numericInput("lambda_steps_evpi", "Number of λ steps:", value = 100, min = 10),
                                       shiny::downloadButton("download_evpi", "Download plot (PNG)"))),
              shiny::column(width = 9,
                            shiny::div(class = "plot-container",
                                       shiny::plotOutput("plot_evpi", height = "600px"))))
          ),

          # === MARGINALS ===
          shiny::tabPanel(
            "Marginals",
            shiny::fluidRow(
              shiny::column(width = 3,
                            shiny::div(class = "box-card",
                                       shiny::h5("Display options"),
                                       shiny::selectInput("mode_marg", "Mode:",
                                                          choices = c("Overall", "Subgroups", "Full"), selected = "Full"),
                                       shiny::selectInput("marg_variable", "Variable:",
                                                          choices = c("cost", "effect"), selected = "cost"),
                                       shiny::selectInput("marg_geom", "Geometry:",
                                                          choices = c("histogram", "density", "boxplot"), selected = "histogram"),
                                       shiny::selectInput("color_by_marg", "Color by:",
                                                          choices = c("none", "dataset", "subgroup_var", "subgroup_level")),
                                       shiny::selectInput("facet_by_marg", "Facet by:",
                                                          choices = c("none", "dataset", "subgroup_var")),
                                       shiny::checkboxInput("compare_marg", "Show comparisons", FALSE),
                                       shiny::downloadButton("download_marg", "Download plot (PNG)"))),
              shiny::column(width = 9,
                            shiny::div(class = "plot-container",
                                       shiny::plotOutput("plot_marginals", height = "600px"))))
          )
        )
      )
    ),

    # ---- TAB 5: REPORT ----
    shiny::tabPanel(
      title = "Generate Report",
      shiny::fluidPage(
        shiny::div(class = "box-card",
                   shiny::h4("Generate reproducible report (RMarkdown)"),
                   shiny::p("Choose format and click 'Generate report'."),
                   shiny::selectInput("report_format", "Format:",
                                      choices = c("html_document", "pdf_document", "word_document"),
                                      selected = "html_document"),
                   shiny::textInput("report_title", "Report title:", value = "CEAgroupR Report"),
                   shiny::downloadButton("download_report", "Generate report"))
      )
    )
  )

  # ---- SERVER ----
  server <- function(input, output, session) {

    results <- shiny::reactiveVal(NULL)
    shinyjs::disable("run_analysis")
    shinyjs::disable(selector = "a[data-value='Analytical Results']")
    shinyjs::disable(selector = "a[data-value='Analysis Configuration']")
    shinyjs::disable(selector = "a[data-value='Visualizations']")
    shinyjs::disable(selector = "a[data-value='Generate Report']")

    # ---- Detect variables dynamically ----
    shiny::observeEvent(list(input$datafile, input$example_datasets), {
      cols <- NULL
      data_preview <- NULL
      if (!is.null(input$datafile)) {
        data_preview <- readxl::read_excel(input$datafile$datapath[1], sheet = 1)
        cols <- names(data_preview)
      } else if (!is.null(input$example_datasets) && length(input$example_datasets) > 0) {
        data_preview <- get(input$example_datasets[1])
        cols <- names(data_preview)
      }
      if (!is.null(cols)) {
        shiny::updateSelectInput(session, "group_var", choices = cols, selected = cols[1])
        shiny::updateSelectInput(session, "cost_var", choices = cols, selected = cols[2])
        shiny::updateSelectInput(session, "effect_var", choices = cols, selected = cols[3])
        shiny::updateSelectInput(session, "subgroup_vars", choices = cols)
        shinyjs::enable("run_analysis")
      }
    }, ignoreInit = TRUE)

    # ---- Update reference group options dynamically ----
    shiny::observeEvent(input$group_var, {
      req(input$group_var)
      data_preview <- NULL
      if (!is.null(input$datafile)) {
        data_preview <- readxl::read_excel(input$datafile$datapath[1], sheet = 1)
      } else if (!is.null(input$example_datasets) && length(input$example_datasets) > 0) {
        data_preview <- get(input$example_datasets[1])
      }
      if (!is.null(data_preview) && input$group_var %in% names(data_preview)) {
        ref_opts <- sort(unique(as.character(data_preview[[input$group_var]])))
        output$ref_group_ui <- shiny::renderUI({
          shiny::selectInput("ref_group", "Reference group:",
                             choices = ref_opts,
                             selected = ref_opts[1])
        })
      } else {
        output$ref_group_ui <- shiny::renderUI({
          shiny::textInput("ref_group", "Reference group (optional):", value = "")
        })
      }
    })

    # ---- Run analysis ----
    shiny::observeEvent(input$run_analysis, {
      shiny::req(input$group_var, input$cost_var, input$effect_var)
      shiny::withProgress(message = "Running analysis...", value = 0.5, {
        if (!is.null(input$datafile)) {
          data_list <- lapply(input$datafile$datapath, readxl::read_excel)
          names(data_list) <- tools::file_path_sans_ext(basename(input$datafile$name))
        } else {
          data_list <- lapply(input$example_datasets, get)
          names(data_list) <- input$example_datasets
        }
        lambda_values <- as.numeric(unlist(strsplit(input$lambda_values, ",\\s*")))
        lambda_values <- lambda_values[!is.na(lambda_values)]
        result <- compute_icers(
          data = data_list,
          group = input$group_var,
          cost = input$cost_var,
          effect = input$effect_var,
          R = input$R,
          lambda = if (length(lambda_values) > 0) lambda_values else 25000,
          ci_type = input$ci_type,
          subgroup_vars = input$subgroup_vars,
          seed = if (!is.na(input$seed)) input$seed else NULL,
          verbose = input$verbose,
          ref_group = if (nzchar(input$ref_group)) input$ref_group else NULL
        )
        results(result)
      })
      shinyjs::enable(selector = "a[data-value='Analytical Results']")
      shinyjs::enable(selector = "a[data-value='Analysis Configuration']")
      shinyjs::enable(selector = "a[data-value='Visualizations']")
      shinyjs::enable(selector = "a[data-value='Generate Report']")
      shiny::showNotification("Analysis completed successfully!", type = "message", duration = 3)
    })

    # ---- Outputs ----
    output$summary_table <- DT::renderDT({
      shiny::req(results())
      df <- results()$summary_stats
      num_cols <- sapply(df, is.numeric)
      df[num_cols] <- lapply(df[num_cols], function(x) round(x, 3))
      DT::datatable(df, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })
    output$settings_table <- shiny::renderTable({
      shiny::req(results())
      as.data.frame(results()$settings)
    }, striped = TRUE, hover = TRUE)
    output$plot_icer <- shiny::renderPlot({
      shiny::req(results())
      plot(results(), type = "icers",
           mode = input$mode_icer, color_by = input$color_by,
           shape_by = input$shape_by, facet_by = input$facet_by,
           show_points = input$show_points, show_means = input$show_means,
           show_contours = input$show_contours, contour_type = input$contour_type)
    })
    output$plot_ceac <- shiny::renderPlot({
      shiny::req(results())
      plot(results(), type = "ceacs", mode = input$mode_ceac,
           color_by = input$color_by_ceac, facet_by = input$facet_by_ceac,
           lambda_steps = input$lambda_steps_ceac)
    })
    output$plot_evpi <- shiny::renderPlot({
      shiny::req(results())
      plot(results(), type = "evpis", mode = input$mode_evpi,
           color_by = input$color_by_evpi, facet_by = input$facet_by_evpi,
           lambda_steps = input$lambda_steps_evpi)
    })
    output$plot_marginals <- shiny::renderPlot({
      shiny::req(results())
      plot(results(), type = "marginals", variable = input$marg_variable,
           geom_type = input$marg_geom, mode = input$mode_marg,
           color_by = input$color_by_marg, facet_by = input$facet_by_marg,
           compare = input$compare_marg)
    })

    # ---- Report generation ----
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        ext <- switch(input$report_format,
                      html_document = "html",
                      pdf_document = "pdf",
                      word_document = "docx")
        paste0("CEAgroupR_report_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        shiny::req(results())
        tmp_rds <- tempfile(fileext = ".rds")
        saveRDS(results(), tmp_rds)
        template_path <- system.file("templates", "report_template.Rmd", package = "CEAgroupR")
        if (template_path == "" || !file.exists(template_path)) {
          shiny::showNotification("Error: RMarkdown template not found in inst/templates/",
                                  type = "error", duration = NULL)
          return(NULL)
        }
        tmp_rds <- gsub("\\\\", "/", tmp_rds)
        template_path <- gsub("\\\\", "/", template_path)
        params <- list(
          results_path = tmp_rds,
          title = enc2utf8(input$report_title),
          author = "CEAgroupR Package"
        )
        tryCatch({
          rmarkdown::render(
            input = template_path,
            output_file = file,
            params = params,
            envir = new.env(parent = globalenv()),
            encoding = "UTF-8",
            quiet = TRUE
          )
          shiny::showNotification("Report generated successfully!",
                                  type = "message", duration = 4)
        },
        error = function(e) {
          shiny::showNotification(
            paste("Report generation failed:", e$message,
                  "\nIf PDF fails, try HTML or Word (no LaTeX required)."),
            type = "error", duration = NULL
          )
        })
      },
      contentType = "application/octet-stream"
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
