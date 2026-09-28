# ClinimetriX

library(shiny)
library(bslib)
library(readxl)
library(ggplot2)
library(DT)

get_app_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_idx <- grep(file_arg, cmd_args)
  if (length(file_idx) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", cmd_args[file_idx[1]]), mustWork = TRUE)))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile, mustWork = TRUE)))
  }
  normalizePath(getwd(), mustWork = TRUE)
}

app_dir <- get_app_dir()
if (!requireNamespace("RclinimetriX", quietly = TRUE)) {
  stop("Install the RclinimetriX package from https://github.com in the R environment running ClinimetriX, then restart the app.", call. = FALSE)
}

# Source the integrated module.
source(file.path(app_dir, "mod", "mod_dcs.R"))

ui <- page_navbar(
  title = "ClinimetriX v1.3.1-beta",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#00CCBB",
    secondary = "#6C757D",
    success = "#00CCBB",
    info = "#DDF0EE",
    base_font = font_google("Inter")
  ),
  header = tags$head(
    tags$style(HTML("
      /* Global layout and typography */
      body {
        background-color: #F4F7F6;
      }

      /* Hide sidebar chevron toggle button */
      .bslib-sidebar-layout > .collapse-toggle,
      .sidebar .collapse-toggle {
        display: none !important;
      }
      
      /* Card layout */
      .card {
        border: none !important;
        border-radius: 12px !important;
        box-shadow: 0 4px 12px rgba(0,0,0,0.05) !important;
        margin-bottom: 20px;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
      }
      .card:hover {
        box-shadow: 0 6px 16px rgba(0,0,0,0.08) !important;
      }
      .card-header {
        background-color: #FFFFFF !important;
        border-bottom: 1px solid #EAEAEA !important;
        font-weight: 700 !important;
        color: #2C3E50 !important;
        border-radius: 12px 12px 0 0 !important;
        padding: 1rem 1.25rem !important;
      }

      /* Navbar */
      .navbar, .navbar-default, .navbar-expand-md {
        background-color: #00CCBB !important;
        border-color: #00CCBB !important;
        box-shadow: 0 2px 8px rgba(0, 204, 187, 0.4) !important;
      }

      .navbar .navbar-brand {
        font-weight: 800;
        letter-spacing: 0.5px;
      }
      
      .navbar .navbar-brand,
      .navbar .nav-link,
      .navbar .navbar-nav .nav-link,
      .nav-item .nav-link {
        color: #FFFFFF !important;
        transition: opacity 0.2s;
      }
      .navbar .nav-link:hover {
        opacity: 0.8 !important;
      }

      .navbar .nav-link.active,
      .navbar .navbar-nav .nav-link.active {
        color: #FFFFFF !important;
        font-weight: 700;
        border-bottom: 3px solid #FFFFFF;
      }

      /* Buttons and interactive controls */
      .progress-bar,
      .shiny-file-input-progress .progress-bar,
      .shiny-notification .progress-bar {
        background-color: #00CCBB !important;
      }

      .btn-primary, .btn-default {
        background-color: #00CCBB !important;
        border-color: #00CCBB !important;
        color: #FFFFFF !important;
        border-radius: 6px !important;
        font-weight: 600 !important;
        padding: 8px 16px !important;
        transition: all 0.2s ease !important;
      }

      .btn-primary:hover, .btn-default:hover,
      .btn-primary:focus, .btn-default:focus {
        background-color: #00B3A5 !important;
        border-color: #00B3A5 !important;
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(0, 204, 187, 0.3) !important;
      }
      
      /* DataTables layout */
      table.dataTable {
        border-collapse: collapse !important;
      }
      table.dataTable thead th {
        border-bottom: 2px solid #EAEAEA !important;
        color: #2F4858 !important;
      }
      table.dataTable tbody tr {
        transition: background-color 0.15s ease;
      }
      table.dataTable tbody tr:hover {
        background-color: #F8FBFB !important;
      }
      
      /* Home page accents */
      .home-shell .card {
        margin-bottom: 0 !important;
      }
      .home-shell .card > .card-header {
        display: block;
        position: relative;
      }
      .home-shell .card-header::after {
        content: \"\";
        display: block;
        width: 42px;
        height: 2px;
        margin-top: 8px;
        border-radius: 999px;
        background: linear-gradient(90deg, #00CCBB, #7EDFD6);
      }
      .home-two-col {
        display: grid;
        grid-template-columns: 7fr 5fr;
        gap: 0.9rem;
        align-items: stretch;
      }
      .home-two-col > .card {
        height: 100%;
      }
      @media (max-width: 992px) {
        .home-two-col {
          grid-template-columns: 1fr;
        }
      }
      @keyframes homeFadeInUp {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
    ")),
    tags$script(HTML("
      window.setDcsControlsDisabled = function(ids, disabled) {
        ids.forEach(function(id) {
          var control = document.getElementById(id);
          if (control && control.selectize) {
            if (disabled) control.selectize.disable();
            else control.selectize.enable();
          }
          if (control) control.disabled = disabled;
          document.querySelectorAll('#' + id + ' input, #' + id + ' select, #' + id + ' button')
            .forEach(function(element) { element.disabled = disabled; });
        });
      };
      Shiny.addCustomMessageHandler('dcs-controls', function(message) {
        window.setDcsControlsDisabled(message.ids, message.disabled);
      });
    "))
  ),
  
  nav_panel(
    "Home",
    div(
      class = "home-shell",
      style = "display:grid; row-gap:0.9rem; padding-bottom:1rem;",
      card(
        style = "margin-bottom:0; background-image: radial-gradient(rgba(255,255,255,0.30) 0.6px, transparent 0.6px), linear-gradient(125deg, #E6FAF7 0%, #F5FBFF 55%, #FFFFFF 100%); background-size: 8px 8px, auto; border: 1px solid #DDEBE8; overflow:hidden; opacity:0; animation:homeFadeInUp 0.55s ease-out forwards; animation-delay:0.04s;",
        card_body(
          div(
            style = "display:flex; align-items:flex-start; gap:1rem; flex-wrap:wrap;",
            div(
              tags$div("CLINIMETRIC PLATFORM", style = "font-size:0.78rem; letter-spacing:0.12em; font-weight:800; color:#1B6E67; margin-bottom:0.45rem;"),
              tags$h2("Clinimetrix 1.3.1-beta", style = "margin:0 0 0.5rem 0; color:#15364A; font-weight:800; line-height:1.12;"),
              tags$p("A clinimetric workbench for neuropsychological diagnostic accuracy analysis.", style = "margin:0; color:#355069; font-size:1.02rem;")
            )
          )
        )
      ),
      card(
        style = "margin-bottom:0; opacity:0; animation:homeFadeInUp 0.55s ease-out forwards; animation-delay:0.10s;",
        card_header("Conceptual Framework"),
        card_body(style = "font-size:0.98rem; line-height:1.62;",
          tags$p(HTML("<strong>Clinimetrics</strong> is the discipline focused on the quantitative measurement of clinical phenomena and on evaluating the diagnostic performance of tools used for disease classification.")),
          tags$p("In neuropsychology, a clinimetric framework is used to determine whether, and to what extent, a cognitive test is diagnostically useful by integrating psychometrics principles with methods from clinical epidemiology and medical statistics."),
          tags$p("This app provides novel clinimetric approaches for neuropsychologists, such as the Dual Cutoff Selection (DCS) strategy.")
        )
      ),
      div(
        class = "home-two-col",
        card(
          style = "margin-bottom:0; opacity:0; animation:homeFadeInUp 0.55s ease-out forwards; animation-delay:0.16s;",
          card_header("Modules"),
          card_body(
            tags$ul(style = "padding-left:1.15rem; margin-bottom:0; line-height:1.6;",
              tags$li(style = "margin-bottom:0;", tags$b("Dual Cutoff Selection (DCS):"), " Rule-out and rule-in thresholds with grey-zone characterization.")
            )
          )
        ),
        card(
          style = "margin-bottom:0; opacity:0; animation:homeFadeInUp 0.55s ease-out forwards; animation-delay:0.20s;",
          card_header("Contact"),
          card_body(
            tags$p(tags$b("Anonymous Author, PhD"), style = "margin-bottom:0.25rem;"),
            tags$p("Neuropsychologist and Clinical Psychometrician", style = "margin-bottom:0.5rem;"),
            tags$p("Anonymous Department", style = "margin-bottom:0.2rem;"),
            tags$p("Anonymous University", style = "margin-bottom:0.2rem;"),
            tags$p("Anonymous Address", style = "margin-bottom:0.5rem;"),
            tags$p(tags$a(href = "mailto:anonymous.author131@gmail.com", "anonymous.author131@gmail.com"), style = "margin-bottom:0;")
          )
        )
      ),
      card(
        style = "margin-bottom:0; border:1px solid #DDEBE8; opacity:0; animation:homeFadeInUp 0.55s ease-out forwards; animation-delay:0.26s;",
        card_header("References"),
        card_body(
          tags$ol(
            style = "padding-left:1.2rem; margin-bottom:0; line-height:1.6;",
            tags$li(
              style = "margin-bottom:0.65rem;",
              "Ilardi C. R. (2026). Clinimetrics: Towards a diagnostic neuropsychology grounded in Alzheimer's disease. ", tags$em("Journal of neuropsychology,"), "20(1), 246-255. ",
              tags$a(href = "https://doi.org/10.1111/jnp.70008", target = "_blank", "https://doi.org/10.1111/jnp.70008")
            ),
            tags$li(
              style = "margin-bottom:0;",
              "Anonymous Author et al. (in submission). When two is better than one: An R tutorial for the Dual Cut-off Selection (DCS) strategy. ", tags$em("Journal of neuropsychology.")
            )
          )
        )
      )
    )
  ),
  nav_panel(
    "1. Dual Cutoff Selection (DCS)",
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        open = "always",
        fileInput("dual_file", "Upload CSV or Excel file", accept = c(".csv", ".xlsx", ".xls")),
        uiOutput("dual_group_ui"),
        uiOutput("dual_score_ui"),
        uiOutput("dual_direction_ui"),
        hr(),
        mod_dcs_sidebar_ui("dual_1")
      ),
      card(
        full_screen = TRUE,
        card_header("Dual Cutoff Selection (DCS)"),
        card_body(
          mod_dcs_main_ui("dual_1")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  dual_raw_data <- reactive({
    req(input$dual_file)
    df <- tryCatch({
      extension <- tolower(tools::file_ext(input$dual_file$name))
      if (extension == "csv") {
        utils::read.csv(input$dual_file$datapath, check.names = FALSE, stringsAsFactors = FALSE)
      } else if (extension %in% c("xls", "xlsx")) {
        readxl::read_excel(input$dual_file$datapath)
      } else {
        stop("Unsupported file type.")
      }
    }, error = function(e) NULL)
    validate(
      need(!is.null(df), "Uploaded file is not a readable CSV or Excel sheet."),
      need(nrow(df) > 0, "The uploaded CSV or Excel file is empty."),
      need(ncol(df) > 1, "The uploaded file must contain at least two columns.")
    )
    df <- as.data.frame(df)
    names(df) <- trimws(names(df))
    validate(need(!anyDuplicated(names(df)), "Column names must remain unique after removing leading/trailing spaces."))
    df
  })
  output$dual_group_ui <- renderUI({
    req(dual_raw_data())
    selectInput(
      "dual_group",
      "Group variable (0 = controls, 1 = cases)",
      choices = c("Select a variable..." = "", names(dual_raw_data())),
      selected = ""
    )
  })
  output$dual_score_ui <- renderUI({
    req(dual_raw_data())
    selectInput(
      "dual_score",
      "Score variable",
      choices = c("Select a variable..." = "", names(dual_raw_data())),
      selected = ""
    )
  })
  output$dual_direction_ui <- renderUI({
    radioButtons("dual_direction", "Test direction",
      c("Higher score = more positive" = "higher", "Lower score = more positive" = "lower"),
      selected = "lower"
    )
  })
  mod_dcs_server(
    "dual_1",
    data_r = reactive({
      req(dual_raw_data(), input$dual_group, input$dual_score)
      dual_raw_data()
    }),
    group_r = reactive(input$dual_group),
    score_r = reactive(input$dual_score),
    direction_r = reactive(input$dual_direction),
    ready_r = reactive(!is.null(input$dual_group) && nzchar(input$dual_group) && !is.null(input$dual_score) && nzchar(input$dual_score))
  )
}

shinyApp(ui, server)














