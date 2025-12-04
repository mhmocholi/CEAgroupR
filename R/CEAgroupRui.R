#' Launch Interactive CEAgroupR Dashboard
#'
#' Provides an interactive Shiny dashboard for performing multistrategy,
#' multidataset and subgroup-based cost-effectiveness analyses using the
#' CEAgroupR package. The interface allows users to upload datasets, configure
#' analysis options, execute full bootstrap workflows, inspect summary tables,
#' and visualize results through ICER planes, CEACs, EVPI curves and marginal
#' distributions. All plots rely on the unified graphical engine
#' \\code{ce_plot_base}, ensuring consistent aesthetics across the application.
#'
#' The dashboard uses a modern Bootstrap/bslib layout and supports PNG and PDF
#' exports of all visualizations. User-specified aesthetics override defaults.
#'
#' @importFrom shiny navbarPage fluidPage fluidRow column tabPanel
#' @importFrom shiny h3 hr selectInput fileInput verbatimTextOutput
#' @importFrom shiny uiOutput checkboxInput textInput numericInput
#' @importFrom shiny plotOutput downloadButton actionButton
#' @importFrom shiny reactiveVal observeEvent renderText renderUI
#' @importFrom shiny showNotification

#' @importFrom DT datatable renderDT DTOutput
#' @importFrom readxl read_excel
#' @importFrom bslib bs_theme font_google
#' @export
CEAgroupRui <- function() {

  # ======================================================================
  # THEME (Modern Tabler-like Bootstrap Theme via bslib)
  # ======================================================================
  theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#18BC9C",
    base_font = bslib::font_google("Inter"),
    code_font = bslib::font_google("Fira Code"),
    "border-radius" = "0.4rem",
    "card-border-radius" = "0.4rem",
    "enable-shadows" = TRUE
  )

  # ======================================================================
  # UI
  # ======================================================================
  ui <- shiny::navbarPage(
    title = "CEAgroupR - Cost-Effectiveness Analysis Toolkit",
    theme = theme,
    id = "main_nav",

    ###################################################################
    # TAB 1 - DATA & SETTINGS
    ###################################################################
    tabPanel(
      "Data & Settings",
      fluidPage(
        h3("Configure your cost-effectiveness analysis"),
        hr(),

        fluidRow(

          # ------------------------------------------------------------
          # COLUMN 1 - DATA
          # ------------------------------------------------------------
          column(
            width = 4,
            card(
              card_header("1. Data Selection"),
              fileInput("datafile", "Upload Excel file(s):",
                        accept = c(".xlsx", ".xls"), multiple = TRUE),
              selectInput("example_datasets", "Or use example datasets:",
                          choices = c("cua_base", "cua_base_discounted",
                                      "cua_multi", "cua_multi_discounted"),
                          multiple = TRUE)
            )
          ),

          # ------------------------------------------------------------
          # COLUMN 2 - VARIABLES
          # ------------------------------------------------------------
          column(
            width = 4,
            card(
              card_header("2. Variables"),
              selectInput("group_var", "Group variable:", choices = NULL),
              selectInput("cost_var",  "Cost variable:",  choices = NULL),
              selectInput("effect_var","Effect variable:",choices = NULL),
              selectInput("subgroup_vars", "Subgroup variables:",
                          choices = NULL, multiple = TRUE),
              numericInput("R", "Bootstrap replications (R):",
                           value = 1000, min = 50)
            )
          ),

          # ------------------------------------------------------------
          # COLUMN 3 - ANALYSIS SETTINGS
          # ------------------------------------------------------------
          column(
            width = 4,
            card(
              card_header("3. Analysis Settings"),
              textInput("lambda_values", "Lambda values (comma-separated):",
                        value = "25000"),
              uiOutput("ref_group_ui"),
              uiOutput("alt_groups_ui"),
              selectInput("ci_type", "CI type:",
                          choices = c("bca", "perc", "basic", "norm")),
              numericInput("seed", "Random seed:", value = NA),
              checkboxInput("verbose", "Show progress messages", FALSE),
              br(),
              actionButton("run_analysis", "Run Full Analysis",
                           class = "btn btn-primary w-100")
            )
          )
        ),

        hr(),
        card(
          card_header("Analysis Status"),
          verbatimTextOutput("analysis_status")
        )
      )
    ),

    ###################################################################
    # TAB 2 - ANALYTICAL RESULTS
    ###################################################################
    tabPanel(
      "Analytical Results",
      fluidPage(
        card(
          card_header("Summary Statistics"),
          downloadButton("download_table", "Download CSV"),
          DTOutput("summary_table")
        ),
        card(
          card_header("Reproducible R Code"),
          uiOutput("code_block")
        )
      )
    ),

    ###################################################################
    # TAB 3 - VISUALIZATIONS
    ###################################################################
    tabPanel(
      "Visualizations",
      fluidPage(

        tabsetPanel(

          #################################################################
          # ICER PLANE
          #################################################################
          tabPanel(
            "ICER Plane",
            fluidRow(
              column(
                width = 3,
                card(
                  card_header("Display Options"),
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
                  textInput("filter_icer", "Data filter:"),
                  textInput("palette_icer", "Palette:", value = "Set2"),
                  downloadButton("download_icer_png", "PNG"),
                  downloadButton("download_icer_pdf", "PDF")
                )
              ),
              column(
                width = 9,
                card(
                  card_header("ICER Plot"),
                  plotOutput("plot_icer", height = "650px")
                )
              )
            )
          ),
          #################################################################
          # CEAC
          #################################################################
          tabPanel(
            "CEAC",
            fluidRow(
              column(
                width = 3,
                card(
                  card_header("Display Options"),
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
                  numericInput("lambda_steps_ceac", "Lambda steps:",
                               value = 100, min = 10),
                  textInput("filter_ceac", "Filter:", value = ""),
                  textInput("palette_ceac", "Palette:", value = "Set2"),
                  downloadButton("download_ceac_png", "PNG"),
                  downloadButton("download_ceac_pdf", "PDF")
                )
              ),
              column(
                width = 9,
                card(
                  card_header("CEAC Plot"),
                  plotOutput("plot_ceac", height = "650px")
                )
              )
            )
          ),

          #################################################################
          # EVPI
          #################################################################
          tabPanel(
            "EVPI",
            fluidRow(
              column(
                width = 3,
                card(
                  card_header("Display Options"),
                  selectInput("mode_evpi", "Mode:",
                              choices = c("Overall","Subgroups")),
                  selectInput("color_by_evpi", "Color by:",
                              choices = c("dataset","comparison",
                                          "subgroup_var","subgroup_level")),
                  selectInput("shape_by_evpi", "Shape by:",
                              choices = c("none","dataset","comparison",
                                          "subgroup_var","subgroup_level")),
                  numericInput("lambda_steps_evpi", "Lambda steps:",
                               value = 100, min = 10),
                  textInput("filter_evpi", "Filter:", value = ""),
                  textInput("palette_evpi", "Palette:", value = "Set2"),
                  downloadButton("download_evpi_png", "PNG"),
                  downloadButton("download_evpi_pdf", "PDF")
                )
              ),
              column(
                width = 9,
                card(
                  card_header("EVPI Plot"),
                  plotOutput("plot_evpi", height = "650px")
                )
              )
            )
          ),

          #################################################################
          # MARGINALS
          #################################################################
          tabPanel(
            "Marginals",
            fluidRow(
              column(
                width = 3,
                card(
                  card_header("Display Options"),
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
                  downloadButton("download_marg_png", "PNG"),
                  downloadButton("download_marg_pdf", "PDF")
                )
              ),
              column(
                width = 9,
                card(
                  card_header("Marginal Distributions"),
                  plotOutput("plot_marginals", height = "650px")
                )
              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {}

  shiny::shinyApp(ui, server)
}
