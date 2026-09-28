mod_dcs_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h5("Analysis settings", style = "font-weight:700; color:#2C3E50;"),
    shiny::numericInput(ns("target_sensitivity"), "Target sensitivity (rule-out)", 0.90, min = 0, max = 1, step = 0.01),
    shiny::numericInput(ns("target_specificity"), "Target specificity (rule-in)", 0.90, min = 0, max = 1, step = 0.01),
    shiny::numericInput(ns("n_bootstrap"), "Bootstrap iterations", 1000, min = 2, step = 1),
    bslib::input_switch(ns("use_bootstrap_seed"), "Use a fixed bootstrap seed", value = TRUE),
    shiny::uiOutput(ns("bootstrap_seed_ui")),
    shiny::uiOutput(ns("actions"))
  )
}

mod_dcs_main_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(7, 5), gap = "1rem",
      bslib::card(bslib::card_header("Dual-cutoff distribution"), bslib::card_body(shiny::plotOutput(ns("plot"), height = "500px"))),
      bslib::card(bslib::card_header("Output table"), bslib::card_body(DT::DTOutput(ns("table"))))
    ),
    shiny::tags$div(
      style = "margin-top:1rem; padding:0.75rem 0.25rem; color:#4D5B66; font-size:0.9rem; clear:both;",
      shiny::tags$strong("Validation messages"),
      shiny::uiOutput(ns("validation"))
    )
  )
}

mod_dcs_server <- function(id, data_r, group_r, score_r, direction_r, ready_r) {
  shiny::moduleServer(id, function(input, output, session) {
    analysis_result <- shiny::reactiveVal(NULL)
    analysis_error <- shiny::reactiveVal(NULL)
    running <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(
      list(data_r(), group_r(), score_r(), direction_r(), input$target_sensitivity, input$target_specificity, input$n_bootstrap, input$use_bootstrap_seed, input$bootstrap_seed),
      { analysis_result(NULL); analysis_error(NULL) },
      ignoreInit = TRUE
    )

    output$bootstrap_seed_ui <- shiny::renderUI({
      if (!isTRUE(input$use_bootstrap_seed)) return(NULL)

      shiny::numericInput(
        session$ns("bootstrap_seed"),
        "Bootstrap seed",
        91000,
        min = 0,
        step = 1
      )
    })

    output$actions <- shiny::renderUI({
      ready <- isTRUE(ready_r()) && !running()
      complete <- !is.null(analysis_result()) && !running()
      control_ids <- c(
        "dual_file", "dual_group", "dual_score", "dual_direction",
        session$ns(c(
          "target_sensitivity", "target_specificity", "n_bootstrap",
          "use_bootstrap_seed", "bootstrap_seed"
        ))
      )
      controls_js <- paste0(
        "['", paste(control_ids, collapse = "', '"), "']"
      )
      disable_controls <- sprintf(
        "window.setDcsControlsDisabled(%s, true);",
        controls_js
      )
      download_args <- if (complete) list() else list(
        class = "btn btn-default disabled", tabindex = "-1", onclick = "return false;",
        style = "pointer-events:none; opacity:0.55; cursor:not-allowed;", `aria-disabled` = "true"
      )
      shiny::tagList(
        shiny::actionButton(
          session$ns("run"), "Run", class = "btn-primary",
          disabled = if (!ready) "disabled",
          onclick = sprintf("%s this.disabled=true; this.setAttribute('aria-disabled', 'true'); var download=document.getElementById('%s'); if(download){download.classList.add('disabled'); download.style.pointerEvents='none'; download.style.opacity='0.55'; download.setAttribute('aria-disabled','true'); download.onclick=function(){return false;};} return true;", disable_controls, session$ns("download_xlsx"))
        ),
        shiny::tags$br(), shiny::tags$br(),
        do.call(shiny::downloadButton, c(list(outputId = session$ns("download_xlsx"), label = "Download output (.xlsx)"), download_args))
      )
    })

    run_analysis <- function() {
      if (!isTRUE(ready_r())) stop("Select both the group and score variables before running the analysis.", call. = FALSE)
      if (!is.finite(input$target_sensitivity) || input$target_sensitivity < 0 || input$target_sensitivity > 1) stop("target_sensitivity must be within the 0-1 range, e.g., 0.95.", call. = FALSE)
      if (!is.finite(input$target_specificity) || input$target_specificity < 0 || input$target_specificity > 1) stop("target_specificity must be within the 0-1 range, e.g., 0.95.", call. = FALSE)
      if (!is.finite(input$n_bootstrap) || input$n_bootstrap < 2 || input$n_bootstrap != floor(input$n_bootstrap)) stop("n_bootstrap must be an integer greater than or equal to 2.", call. = FALSE)
      if (isTRUE(input$use_bootstrap_seed) && (!is.finite(input$bootstrap_seed) || input$bootstrap_seed < 0 || input$bootstrap_seed != floor(input$bootstrap_seed))) stop("bootstrap_seed must be a non-negative integer.", call. = FALSE)

      parse_column <- function(x, name) {
        if (is.numeric(x)) return(list(values = x, error = NULL))
        text <- trimws(as.character(x))
        text[text == ""] <- NA_character_
        values <- suppressWarnings(as.numeric(gsub(",", ".", text)))
        invalid <- is.na(values) & !is.na(text)
        list(
          values = values,
          error = if (any(invalid)) sprintf(
            "%s contains non-numeric values: %s",
            name, paste(utils::head(unique(text[invalid]), 5), collapse = ", ")
          ) else NULL
        )
      }
      group <- parse_column(data_r()[[group_r()]], group_r())
      score <- parse_column(data_r()[[score_r()]], score_r())
      errors <- Filter(Negate(is.null), list(group$error, score$error))
      n_excluded_missing <- 0L
      if (length(errors) == 0) {
        dat <- data.frame(group = group$values, score = score$values)
        complete <- complete.cases(dat)
        n_excluded_missing <- sum(!complete)
        dat <- dat[complete, , drop = FALSE]
        if (nrow(dat) <= 1) errors <- c(errors, "Not enough complete cases after removing missing values.")
        group_codes <- sort(unique(dat$group))
        if (!(length(group_codes) == 2 && all(group_codes == c(0, 1)))) {
          errors <- c(errors, "Group variable must contain exactly two numeric codes: 0 = controls, 1 = cases.")
        }
        if (nrow(dat) > 1 && any(!is.finite(dat$score))) errors <- c(errors, "Score variable must contain only finite numeric values.")
        if (nrow(dat) > 1 && length(unique(dat$score)) <= 1) errors <- c(errors, "Score variable must contain more than one distinct value.")
      }
      if (length(errors) > 0) stop(paste(errors, collapse = "\n"), call. = FALSE)

      bootstrap_seed <- if (isTRUE(input$use_bootstrap_seed)) {
        as.integer(input$bootstrap_seed)
      } else {
        NULL
      }

      cutoffs <- shiny::withProgress(
        message = "Running stratified bootstrap",
        detail = "Preparing analysis",
        value = 0,
        RclinimetriX::get_cutoffs(
          score = dat$score,
          group = dat$group,
          sensitivity_target = input$target_sensitivity,
          specificity_target = input$target_specificity,
          n_bootstrap = as.integer(input$n_bootstrap),
          seed = bootstrap_seed,
          direction = direction_r(),
          progress = function(iteration, total) {
            shiny::incProgress(1 / total, detail = sprintf("Bootstrap %d of %d", iteration, total))
          }
        )
      )
      classified <- RclinimetriX::classify_subjects(score = dat$score, group = dat$group, cutoffs = cutoffs)

      validation <- character()
      if (n_excluded_missing > 0) {
        validation <- c(validation, sprintf("%d row(s) excluded because group or score was missing.", n_excluded_missing))
      }
      if (isTRUE(all.equal(cutoffs$cutoffs[["ruleout"]], cutoffs$cutoffs[["rulein"]]))) {
        validation <- c(validation, "Rule-in and rule-out cutoffs are equal: gray zone collapses.")
      }
      if (!length(validation)) validation <- "No dual-cutoff validation warnings."

      list(
        cutoffs = cutoffs,
        classified = classified,
        dual_plot = RclinimetriX::dcs_plot(
          score = dat$score,
          group = dat$group,
          status = classified$data$status,
          cutoffs = cutoffs,
          score_label = score_r()
        ),
        output_table = summary(classified),
        validation_table = data.frame(Message = validation, stringsAsFactors = FALSE)
      )
    }

    shiny::observeEvent(input$run, {
      if (running() || !isTRUE(ready_r())) return()
      running(TRUE)
      on.exit({
        running(FALSE)
        session$sendCustomMessage(
          "dcs-controls",
          list(
            ids = c(
              "dual_file", "dual_group", "dual_score", "dual_direction",
              session$ns(c(
                "target_sensitivity", "target_specificity", "n_bootstrap",
                "use_bootstrap_seed", "bootstrap_seed"
              ))
            ),
            disabled = FALSE
          )
        )
      }, add = TRUE)
      analysis_result(NULL)
      analysis_error(NULL)
      result <- tryCatch(run_analysis(), error = function(error) error)
      if (inherits(result, "error")) {
        analysis_error(conditionMessage(result))
      } else {
        analysis_result(result)
      }
    })

    output$plot <- shiny::renderPlot({ shiny::req(analysis_result()); analysis_result()$dual_plot })
    output$table <- DT::renderDT({
      shiny::req(analysis_result())
      DT::datatable(analysis_result()$output_table, rownames = FALSE, width = "100%", options = list(paging = FALSE, searching = FALSE, info = FALSE, dom = "t"))
    })
    output$validation <- shiny::renderUI({
      if (!is.null(analysis_error())) return(shiny::tags$ul(lapply(strsplit(analysis_error(), "\n", fixed = TRUE)[[1]], shiny::tags$li), class = "text-danger", style = "margin:0.35rem 0 0; padding-left:1.25rem; font-weight:600;"))
      if (is.null(analysis_result())) return(shiny::tags$p("Select the variables and press Run to start the analysis.", style = "margin:0.35rem 0 0;"))
      shiny::tags$ul(lapply(analysis_result()$validation_table$Message, shiny::tags$li), style = "margin:0.35rem 0 0; padding-left:1.25rem;")
    })
    output$download_xlsx <- shiny::downloadHandler(
      filename = function() paste0("DCS_output_", Sys.Date(), ".xlsx"),
      content = function(file) {
        shiny::req(!running(), analysis_result())
        writexl::write_xlsx(list(output = analysis_result()$output_table, validation = analysis_result()$validation_table), file)
      }
    )
  })
}
