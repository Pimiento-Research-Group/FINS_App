# server.R — FINS Explorer: Server logic

server <- function(input, output, session) {
  
  # Use rv$occ / rv$col / rv$refs everywhere instead of raw occ/col/refs
  rv <- reactiveValues(occ = occ, col = col, refs = refs)
  
  # Cache for paleocoords keyed by the uploaded Collections file contents
  rv$paleo_cache <- list()
  
  # Read CSV with flexible delimiter
  read_upload_csv <- function(file, header = TRUE, delim_choice = "Auto") {
    if (is.null(file)) return(NULL)
    path <- file$datapath
    delim <- switch(delim_choice, "," = ",", ";" = ";", "\t" = "\t", "Auto" = NULL, NULL)
    if (is.null(delim)) {
      readr::read_csv(path, show_col_types = FALSE, progress = FALSE, col_names = header, guess_max = 10000)
    } else if (delim == "\t") {
      readr::read_tsv(path, show_col_types = FALSE, progress = FALSE, col_names = header, guess_max = 10000)
    } else {
      readr::read_delim(path, delim = delim, show_col_types = FALSE, progress = FALSE, col_names = header, guess_max = 10000)
    }
  }
  
  # Align an uploaded frame to a template (FINS schema)
  align_to_template <- function(df_new, template_df) {
    for (nm in setdiff(names(template_df), names(df_new))) {
      tpl_col <- template_df[[nm]]
      if (is.numeric(tpl_col))      df_new[[nm]] <- NA_real_
      else if (is.integer(tpl_col)) df_new[[nm]] <- NA_integer_
      else if (is.logical(tpl_col)) df_new[[nm]] <- NA
      else                          df_new[[nm]] <- NA_character_
    }
    ordered <- c(names(template_df), setdiff(names(df_new), names(template_df)))
    df_new[, ordered, drop = FALSE]
  }
  
  # Read PBDB CSV with auto-detection of header row and delimiter
  read_pbdb_csv <- function(file, header = TRUE, delim_choice = "Auto") {
    if (is.null(file)) return(NULL)
    path  <- file$datapath
    lines <- readr::read_lines(path, skip_empty_rows = FALSE)
    
    n <- length(lines)
    if (!n) return(data.frame())
    
    # Find header line by counting known PBDB tokens
    header_tokens <- c("collection_number", "collection_no","collection_name",
                       "lng","lat","early_interval","late_interval","max_ma","min_ma","cc","state")
    
    count_tokens <- function(s) {
      s <- tolower(s)
      sum(vapply(header_tokens, function(tok) {
        grepl(paste0("\\b", tok, "\\b"), s)
      }, logical(1)))
    }
    
    token_hits <- vapply(lines, count_tokens, integer(1))
    header_idx <- which(token_hits >= 3)
    header_idx <- if (length(header_idx)) header_idx[1] else NA_integer_
    
    # Fallback: look for "Records:" marker
    if (is.na(header_idx)) {
      rec_idx <- which(grepl("^\\s*Records\\s*:\\s*$", lines, ignore.case = TRUE))
      if (length(rec_idx)) {
        j <- rec_idx[1] + 1
        while (j <= n && grepl("^\\s*$", lines[j])) j <- j + 1
        header_idx <- if (j <= n) j else NA_integer_
      }
    }
    
    if (is.na(header_idx)) header_idx <- 1L
    
    header_line <- lines[header_idx]
    
    # Choose delimiter
    if (!is.null(delim_choice) && !identical(delim_choice, "Auto")) {
      delim <- switch(delim_choice, "," = ",", ";" = ";", "\t" = "\t", NULL)
    } else {
      if (grepl(",", header_line))      delim <- ","
      else if (grepl("\t", header_line)) delim <- "\t"
      else if (grepl(";", header_line))  delim <- ";"
      else                               delim <- ","
    }
    
    skip_n <- header_idx - 1L
    loc    <- readr::locale(encoding = "UTF-8")
    
    if (identical(delim, "\t")) {
      df <- readr::read_tsv(path, skip = skip_n, col_names = header,
                            locale = loc, show_col_types = FALSE, progress = FALSE, guess_max = 10000)
    } else {
      df <- readr::read_delim(path, delim = delim, skip = skip_n, col_names = header,
                              locale = loc, show_col_types = FALSE, progress = FALSE, guess_max = 10000)
    }
    
    # Drop fully-empty rows
    if (nrow(df)) {
      empty_row <- apply(df, 1, function(r) all(is.na(r) | r == ""))
      df <- df[!empty_row, , drop = FALSE]
    }
    
    df
  }
  
  # PBDB Occurrences processor
  process_pbdb_occurrences <- function(df, keep_only_needed = TRUE) {
    names(df) <- tolower(names(df))
    
    needed <- c(
      "occurrence_no","occurrence_id",
      "collection_number",
      "identified_name","taxon_name","accepted_name",
      "genus","family","order","superorder","rank","status",
      "lng","lat","early_interval","late_interval","max_ma","min_ma", "reference"
    )
    df <- ensure_cols(df, needed)
    
    occ_no_raw <- if (!all(is.na(df[["occurrence_no"]]))) df[["occurrence_no"]] else df[["occurrence_id"]]
    occ_no_chr <- suppressWarnings(as.character(occ_no_raw))
    occurrence_number <- ifelse(!is.na(occ_no_chr) & occ_no_chr != "",
                                paste0("PBDB_", occ_no_chr), NA_character_)
    
    colno_raw <- if ("collection_no" %in% names(df) && !all(is.na(df[["collection_no"]]))) {
      df[["collection_no"]]
    } else if ("collection_number" %in% names(df) && !all(is.na(df[["collection_number"]]))) {
      df[["collection_number"]]
    } else {
      NA
    }
    colno_chr <- suppressWarnings(as.character(colno_raw))
    coll_id <- ifelse(!is.na(colno_chr) & colno_chr != "",
                      paste0("PBDB_", colno_chr), NA_character_)
    
    out <- tibble::tibble(
      occurrence_number = occurrence_number,
      collection_no     = coll_id,
      coll_id           = coll_id,
      identified_name   = df[["identified_name"]],
      genus             = df[["genus"]],
      family            = df[["family"]],
      order             = df[["order"]],
      superorder        = df[["superorder"]],
      rank              = df[["rank"]],
      status            = df[["status"]],
      source            = factor("PBDB", levels = c("PBDB","Literature")),
      reference         = df[["ref_author"]]
    )
    
    # Process taxonomy from identified_name
    # Process taxonomy from identified_name
    if ("identified_name" %in% names(out) && exists("process_taxonomy_batch")) {
      tax_result <- process_taxonomy_batch(out$identified_name)
      
      out$modified_identified_name <- tax_result$modified_identified_name
      out$accepted_name <- tax_result$accepted_name
      out$genus <- tax_result$genus
      out$family <- tax_result$family
      out$order <- tax_result$order
      out$superorder <- tax_result$superorder
      
      # Add marker columns for highlighting
      out$m_modified_identified_name <- !is.na(tax_result$modified_identified_name)
      out$m_accepted_name <- !is.na(tax_result$accepted_name)
      out$m_genus <- !is.na(tax_result$genus)
      out$m_family <- !is.na(tax_result$family)
      out$m_order <- !is.na(tax_result$order)
      out$m_superorder <- !is.na(tax_result$superorder)
    }
    
    if (!keep_only_needed) {
      extras <- df[, setdiff(names(df), names(out)), drop = FALSE]
      extras <- tibble::as_tibble(lapply(extras, function(x) if (is.list(x)) sapply(x, as.character) else x))
      out <- dplyr::bind_cols(out, extras)
    }
    
    out
  }
  
  # ---- Welcome tab ----
  output$readme_ui_about <- renderUI({
    readme_path <- here::here("data", "ReadMe_about.md")
    if (file.exists(readme_path)) {
      # Read the markdown file
      content <- paste(readLines(readme_path, warn = FALSE), collapse = "\n")
      
      # Remove "How to cite" section and everything after it
      # This removes both "How to cite FINS?" and "Contact us" sections
      content <- gsub("#### How to cite.*$", "", content, ignore.case = TRUE)
      content <- gsub("#### Contact us.*$", "", content, ignore.case = TRUE)
      
      # Also try with different header levels
      content <- gsub("### How to cite.*$", "", content, ignore.case = TRUE)
      content <- gsub("### Contact us.*$", "", content, ignore.case = TRUE)
      content <- gsub("## How to cite.*$", "", content, ignore.case = TRUE)
      content <- gsub("## Contact us.*$", "", content, ignore.case = TRUE)
      
      # Convert to HTML
      HTML(markdown::markdownToHTML(text = content, fragment.only = TRUE))
    } else {
      # Default About content if file doesn't exist
      tags$div(
        p(strong("FINS"), " (FossIl NeoSelachians) is a comprehensive global database of fossil sharks, rays, and skates."),
        h5("Database Contents"),
        tags$ul(
          tags$li("Fossil occurrence records with detailed taxonomic information"),
          tags$li("Collection localities with geographic coordinates"),
          tags$li("Temporal data spanning from the Cretaceous to the Quaternary"),
          tags$li("References to scientific literature")
        ),
        p(em("Note: Create a README_about.md file in the data directory to customize this content."))
      )
    }
  })
  
  output$readme_ui_guide <- renderUI({
    readme_path <- here::here("data", "ReadMe_guide.md")
    if (file.exists(readme_path)) {
      # Read and display the guide as-is
      HTML(markdown::markdownToHTML(readme_path, fragment.only = TRUE))
    } else {
      # Default Guide content if file doesn't exist
      tags$div(
        p("To contribute new fossil occurrence data to the FINS database:"),
        tags$ol(
          tags$li(strong("Navigate to the 'Add data' tab")),
          tags$li(strong("Upload PBDB Collections:"), " Choose your PBDB Collections CSV file"),
          tags$li(strong("Preview and apply:"), " Review the data alignment and click 'Apply Collections'")
        ),
        p(em("Note: Create a README_guide.md file in the data directory to customize this guide."))
      )
    }
  })
  
  # ----- Occurrences tab -----
  
  # Cascading filters: Superorder → Order → Family
  # Incompatible selections are prevented by filtering the dropdown choices
  
  # Forward cascade: When superorder changes, update available orders
  observeEvent(input$superorder_occ, {
    if (length(input$superorder_occ) > 0) {
      # Filter orders based on selected superorders
      available_orders <- c()
      for (so in input$superorder_occ) {
        if (!is.null(superorder_to_orders[[so]])) {
          available_orders <- c(available_orders, superorder_to_orders[[so]])
        }
      }
      available_orders <- sort(unique(available_orders[!is.na(available_orders)]))
      
      if (length(available_orders) > 0) {
        updateSelectizeInput(session, "order_occ", 
                             choices = available_orders,
                             server = TRUE)
      }
    } else {
      # No superorder selected - show all orders
      updateSelectizeInput(session, "order_occ", 
                           choices = order_choices,
                           server = TRUE)
    }
  }, ignoreNULL = FALSE)
  
  # When order or superorder changes, update available families
  observeEvent(list(input$order_occ, input$superorder_occ), {
    available_families <- c()
    
    if (length(input$order_occ) > 0) {
      # If orders are selected, use order -> family mapping
      for (ord in input$order_occ) {
        if (!is.null(order_to_families[[ord]])) {
          available_families <- c(available_families, order_to_families[[ord]])
        }
      }
    } else if (length(input$superorder_occ) > 0) {
      # If only superorders are selected, use superorder -> family mapping
      for (so in input$superorder_occ) {
        if (!is.null(superorder_to_families[[so]])) {
          available_families <- c(available_families, superorder_to_families[[so]])
        }
      }
    }
    
    available_families <- sort(unique(available_families[!is.na(available_families)]))
    
    # If no families found, show all families
    if (length(available_families) == 0) {
      available_families <- family_choices
    }
    
    # Keep only currently selected families that are still valid
    current_selection <- intersect(input$family_occ, available_families)
    
    updateSelectizeInput(session, "family_occ",
                         choices = available_families,
                         selected = current_selection,
                         server = TRUE)
  }, ignoreNULL = FALSE)
  
  # When taxonomic filters change, update available statuses
  observeEvent(list(input$superorder_occ, input$order_occ, input$family_occ), {
    # Filter data based on current taxonomic selections
    filtered_data <- occ
    
    if (length(input$superorder_occ) > 0) {
      filtered_data <- filtered_data %>% filter(superorder %in% input$superorder_occ)
    }
    if (length(input$order_occ) > 0) {
      filtered_data <- filtered_data %>% filter(order %in% input$order_occ)
    }
    if (length(input$family_occ) > 0) {
      filtered_data <- filtered_data %>% filter(family %in% input$family_occ)
    }
    
    # Get available statuses from filtered data
    available_statuses <- us(filtered_data$status)
    
    # If no taxonomic filters selected, show all statuses
    if (length(input$superorder_occ) == 0 && 
        length(input$order_occ) == 0 && 
        length(input$family_occ) == 0) {
      available_statuses <- status_choices
    }
    
    # Keep only currently selected statuses that are still valid
    current_selection <- intersect(input$status_occ, available_statuses)
    
    updateSelectizeInput(session, "status_occ",
                         choices = available_statuses,
                         selected = current_selection,
                         server = TRUE)
  }, ignoreNULL = FALSE)
  
  # Select All / Clear All observers for Occurrences filters
  observeEvent(input$select_all_epochs_occ, {
    updateSelectizeInput(session, "epochs_occ", selected = epoch_choices_occ)
  })
  observeEvent(input$clear_all_epochs_occ, {
    updateSelectizeInput(session, "epochs_occ", selected = character(0))
  })
  
  observeEvent(input$select_all_periods_occ, {
    updateSelectizeInput(session, "periods_occ", selected = period_choices_occ)
  })
  observeEvent(input$clear_all_periods_occ, {
    updateSelectizeInput(session, "periods_occ", selected = character(0))
  })
  
  observeEvent(input$select_all_continent_occ, {
    updateSelectizeInput(session, "continent_occ", selected = continent_choices_occ)
  })
  observeEvent(input$clear_all_continent_occ, {
    updateSelectizeInput(session, "continent_occ", selected = character(0))
  })
  
  observeEvent(input$select_all_paleocean_occ, {
    updateSelectizeInput(session, "paleocean_occ", selected = paleoocean_choices_occ)
  })
  observeEvent(input$clear_all_paleocean_occ, {
    updateSelectizeInput(session, "paleocean_occ", selected = character(0))
  })
  
  observeEvent(input$select_all_order_occ, {
    # Get current available orders based on superorder selection
    if (length(input$superorder_occ) > 0) {
      available_orders <- c()
      for (so in input$superorder_occ) {
        if (!is.null(superorder_to_orders[[so]])) {
          available_orders <- c(available_orders, superorder_to_orders[[so]])
        }
      }
      available_orders <- sort(unique(available_orders[!is.na(available_orders)]))
      if (length(available_orders) > 0) {
        updateSelectizeInput(session, "order_occ", selected = available_orders)
      } else {
        updateSelectizeInput(session, "order_occ", selected = order_choices)
      }
    } else {
      updateSelectizeInput(session, "order_occ", selected = order_choices)
    }
  })
  observeEvent(input$clear_all_order_occ, {
    updateSelectizeInput(session, "order_occ", selected = character(0))
  })
  
  observeEvent(input$select_all_superorder_occ, {
    updateSelectizeInput(session, "superorder_occ", selected = superorder_choices)
  })
  observeEvent(input$clear_all_superorder_occ, {
    updateSelectizeInput(session, "superorder_occ", selected = character(0))
  })
  
  observeEvent(input$select_all_family_occ, {
    # Get current available families based on order/superorder selection
    available_families <- c()
    
    if (length(input$order_occ) > 0) {
      for (ord in input$order_occ) {
        if (!is.null(order_to_families[[ord]])) {
          available_families <- c(available_families, order_to_families[[ord]])
        }
      }
    } else if (length(input$superorder_occ) > 0) {
      for (so in input$superorder_occ) {
        if (!is.null(superorder_to_families[[so]])) {
          available_families <- c(available_families, superorder_to_families[[so]])
        }
      }
    }
    
    available_families <- sort(unique(available_families[!is.na(available_families)]))
    
    if (length(available_families) > 0) {
      updateSelectizeInput(session, "family_occ", selected = available_families)
    } else {
      updateSelectizeInput(session, "family_occ", selected = family_choices)
    }
  })
  observeEvent(input$clear_all_family_occ, {
    updateSelectizeInput(session, "family_occ", selected = character(0))
  })
  
  observeEvent(input$select_all_rank_occ, {
    updateSelectizeInput(session, "rank_occ", selected = rank_choices)
  })
  observeEvent(input$clear_all_rank_occ, {
    updateSelectizeInput(session, "rank_occ", selected = character(0))
  })
  
  observeEvent(input$select_all_status_occ, {
    # Get current available statuses based on taxonomic selection
    filtered_data <- occ
    
    if (length(input$superorder_occ) > 0) {
      filtered_data <- filtered_data %>% filter(superorder %in% input$superorder_occ)
    }
    if (length(input$order_occ) > 0) {
      filtered_data <- filtered_data %>% filter(order %in% input$order_occ)
    }
    if (length(input$family_occ) > 0) {
      filtered_data <- filtered_data %>% filter(family %in% input$family_occ)
    }
    
    available_statuses <- us(filtered_data$status)
    
    # If no taxonomic filters, use all statuses
    if (length(input$superorder_occ) == 0 && 
        length(input$order_occ) == 0 && 
        length(input$family_occ) == 0) {
      available_statuses <- status_choices
    }
    
    updateSelectizeInput(session, "status_occ", selected = available_statuses)
  })
  observeEvent(input$clear_all_status_occ, {
    updateSelectizeInput(session, "status_occ", selected = character(0))
  })
  
  apply_age_thresh_occ <- function(df, choice) {
    if (choice == "any" || !"age_range_any" %in% names(df)) return(df)
    thr <- as.numeric(choice)
    df %>% filter(!is.na(age_range_any) & age_range_any <= thr)
  }
  
  current_name_occ <- reactive({
    if (input$name_mode_occ == "cur") "name_curated" else "name_raw"
  })
  
  occ_filtered <- reactive({
    out <- rv$occ
    out <- overlaps_time(out, input$epochs_occ, input$periods_occ)
    out <- apply_age_thresh_occ(out, input$age_thresh_occ)
    out <- apply_geog(out, input$continent_occ, NULL, input$paleocean_occ)
    if (length(input$order_occ))      out <- out %>% filter(order %in% input$order_occ)
    if (length(input$superorder_occ)) out <- out %>% filter(superorder %in% input$superorder_occ)
    if (length(input$family_occ))     out <- out %>% filter(family %in% input$family_occ)
    if (length(input$rank_occ))       out <- out %>% filter(rank %in% input$rank_occ)
    if (length(input$status_occ))     out <- out %>% filter(status %in% input$status_occ)
    out <- apply_validation(out, input$val_filters_occ)
    if (length(input$source_filter_occ)) out <- out %>% filter(source %in% input$source_filter_occ)
    
    nm <- current_name_occ()
    if (isTRUE(input$collapse_dups_occ) && nm %in% names(out)) 
      out <- out %>% distinct(.data[[nm]], .keep_all = TRUE)
    
    # Uncertain nomenclature
    idn <- out$identified_name
    idn_str <- ifelse(is.na(idn), "", idn)
    
    uncertain_pat <- '(?i)\\baff\\.|\\bcf\\.|\\?|"'
    uncertain <- grepl(uncertain_pat, idn_str, perl = TRUE)
    
    if ("Exclude" %in% (input$uncertain_mode_occ %||% character(0))) {
      out <- out[!uncertain, , drop = FALSE]
      idn <- out$identified_name
      idn_str <- ifelse(is.na(idn), "", idn)
      uncertain <- grepl(uncertain_pat, idn_str, perl = TRUE)
    }
    
    if ("Flag" %in% (input$uncertain_mode_occ %||% character(0))) {
      out[["certain_nomenclature"]] <- ifelse(uncertain, "No", "Yes")
    }
    
    out %>% mutate(name_display = .data[[nm]])
  })
  
  output$source_counts_occ <- renderTable({
    tab <- occ_filtered() %>%
      count(source, name = "n", sort = FALSE) %>%
      tidyr::complete(source = source_levels, fill = list(n = 0))
    
    total <- sum(tab$n, na.rm = TRUE)
    tab <- dplyr::bind_rows(tab, tibble::tibble(source = "Total", n = total))
    
    names(tab) <- c("Source", "Count")
    tab
  }, bordered = TRUE, striped = TRUE, digits = 0)
  
  output$curation_snapshot_occ <- renderUI({
    df <- occ_filtered()
    n <- nrow(df)
    n_updated <- sum(df$name_updated %||% FALSE, na.rm = TRUE)
    lab <- if (input$name_mode_occ == "cur") "Curated names" else "As-provided names"
    HTML(sprintf("<p><b>%s</b>: %d records. Updated by curation: %d (%.1f%%).</p>",
                 lab, n, n_updated, ifelse(n>0, 100*n_updated/n, 0)))
  })
  
  output$map_occ <- renderLeaflet({ 
    leaflet(options = leafletOptions(
      worldCopyJump = FALSE,     # Prevent jumping between world copies
      maxBounds = list(list(-180, -90), list(180, 90)),  # Limit panning to one world
      maxBoundsViscosity = 1.0,   # Make bounds hard (can't pan beyond)
      minZoom = 2,                # Prevent zooming out beyond initial view
      maxZoom = 18                # Allow zooming in to street level
    )) %>% 
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)  # Restrict view to one Earth
  })
  outputOptions(output, "map_occ", suspendWhenHidden = FALSE)
  
  observe({
    pts <- occ_filtered() %>% filter(!is.na(latitude), !is.na(longitude))
    if (nrow(pts) == 0) { 
      leafletProxy("map_occ") %>% clearMarkers() %>% clearControls()
      return() 
    }
    leafletProxy("map_occ") %>% clearMarkers() %>% clearControls() %>%
      addCircleMarkers(
        data = pts, lng = ~longitude, lat = ~latitude,
        radius = 5, opacity = 0.9, fillOpacity = 0.75,
        color = "#FFFFFF", weight = 0.5,  # Thin white border
        fillColor = ~pal_source(source),
        popup = ~paste0(
          "<b>", name_display %||% "", "</b><br/>",
          "Order: ", order %||% "", "<br/>",
          "Period: ", (early_period %||% ""), " – ", (late_period %||% ""), "<br/>",
          "Epoch: ", (early_epoch %||% ""),  " – ", (late_epoch %||% ""), "<br/>",
          "Source: ", as.character(source) %||% ""
        )
      ) %>%
      addLegend(
        "topright", pal = pal_source,
        values = factor(source_levels, levels = source_levels),
        title = "Source", opacity = 0.9
      )
  })
  
  output$table_occ <- renderDT({
    df <- occ_filtered()
    show_cols <- c("collection_number","name_display","superorder","order","family","genus","rank","status",
                   "early_period","late_period","early_epoch","late_epoch",
                   "early_interval","late_interval","max_ma","min_ma","age_range_any",
                   "continent","paleoocean","age_evaluation","taxonomy_validation","evidence_validation",
                   "source","reference", "certain_nomenclature")
    show_cols <- intersect(show_cols, names(df))
    datatable(df %>% select(all_of(show_cols)) %>% rename(Name = name_display,
                                                          `age_range (Myr)` = age_range_any),
              options = list(pageLength = 12), rownames = FALSE)
  })
  
  output$download_occ <- downloadHandler(
    filename = function() paste0("fins_occurrences_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(occ_filtered(), file, row.names = FALSE)
  )
  
  # ----- Data Visualisation Charts -----
  
  # Dynamic container for charts
  output$charts_container_occ <- renderUI({
    selected <- input$chart_selector_occ
    
    if (length(selected) == 0) {
      return(p("Select at least one visualisation above.", style = "color: #666; font-style: italic;"))
    }
    
    plot_list <- list()
    
    if ("epoch" %in% selected) {
      plot_list <- c(plot_list, list(
        h4("Temporal distribution"),
        plotOutput("epoch_plot_occ", height = 400),
        tags$hr()
      ))
    }
    
    if ("continent" %in% selected) {
      plot_list <- c(plot_list, list(
        h4("Geographic distribution"),
        plotOutput("continent_plot_occ", height = 350),
        tags$hr()
      ))
    }
    
    if ("paleoocean" %in% selected) {
      plot_list <- c(plot_list, list(
        h4("Paleogeographic distribution"),
        plotOutput("paleoocean_plot_occ", height = 400),
        tags$hr()
      ))
    }
    
    if ("order" %in% selected) {
      plot_list <- c(plot_list, list(
        h4("Taxonomic distribution"),
        plotOutput("order_plot_occ", height = 450),
        tags$hr()
      ))
    }
    
    if ("rank" %in% selected) {
      plot_list <- c(plot_list, list(
        h4("Taxonomic rank distribution"),
        plotOutput("rank_plot_occ", height = 300),
        tags$hr()
      ))
    }
    
    # Remove last hr
    if (length(plot_list) > 0) {
      plot_list <- plot_list[-length(plot_list)]
    }
    
    do.call(tagList, plot_list)
  })
  
  # Color palette for sources (shared)
  source_colors <- c("PBDB" = "#b56a9c", "Literature" = "#037c6e", "PBDB_U" = "#80c7ff")
  
  # EPOCH CHART
  output$epoch_plot_occ <- renderPlot({
    df <- occ_filtered()
    
    epoch_boundaries <- data.frame(
      epoch = c("Early Cretaceous", "Late Cretaceous", "Paleocene", "Eocene", 
                "Oligocene", "Miocene", "Pliocene", "Pleistocene", "Holocene"),
      min_age = c(145, 100.5, 66, 56, 33.9, 23.03, 5.333, 2.58, 0.0117),
      max_age = c(100.5, 66, 56, 33.9, 23.03, 5.333, 2.58, 0.0117, 0),
      stringsAsFactors = FALSE
    )
    
    classify_epoch <- function(min_ma, max_ma) {
      if (is.na(min_ma) || is.na(max_ma)) return(NA_character_)
      overlaps <- epoch_boundaries[
        max_ma >= epoch_boundaries$min_age & min_ma <= epoch_boundaries$max_age,
      ]
      if (nrow(overlaps) == 0) return(NA_character_)
      if (nrow(overlaps) == 1) return(overlaps$epoch[1])
      midpoint <- (min_ma + max_ma) / 2
      idx <- which(midpoint >= overlaps$min_age & midpoint <= overlaps$max_age)
      if (length(idx) > 0) overlaps$epoch[idx[1]] else NA_character_
    }
    
    df_classified <- df %>%
      filter(!is.na(min_ma), !is.na(max_ma)) %>%
      mutate(epoch_classified = mapply(classify_epoch, min_ma, max_ma, SIMPLIFY = TRUE, USE.NAMES = FALSE)) %>%
      filter(!is.na(epoch_classified))
    
    if (nrow(df_classified) == 0) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data with valid age ranges", size = 8, color = "#4a6b6b") +
        theme_void()
    } else {
      plot_data <- df_classified %>% count(epoch_classified, source)
      epoch_totals <- plot_data %>%
        group_by(epoch_classified) %>%
        summarise(total = sum(n), .groups = "drop") %>%
        mutate(percentage = round(100 * total / sum(total), 1),
               label = paste0(total, " (", percentage, "%)")) %>%
        arrange(total)
      plot_data <- plot_data %>%
        mutate(epoch_factor = factor(epoch_classified, levels = epoch_totals$epoch_classified))
      
      ggplot(plot_data, aes(x = epoch_factor, y = n, fill = source)) +
        geom_col(position = "stack") +
        geom_text(data = epoch_totals %>% mutate(epoch_factor = factor(epoch_classified, levels = epoch_totals$epoch_classified)),
                  aes(x = epoch_factor, y = total, label = label, fill = NULL), hjust = -0.1, size = 3.5, fontface = "bold") +
        scale_fill_manual(values = source_colors, name = "Source") +
        coord_flip() +
        labs(title = "Occurrences by epoch",
             subtitle = "Based on age range (min_ma - max_ma) overlap with epoch boundaries",
             x = NULL, y = "Number of occurrences") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14, color = "#4a6b6b"),
              plot.subtitle = element_text(size = 10, color = "#666666", margin = margin(b = 15)),
              axis.text = element_text(size = 11), legend.position = "bottom",
              legend.title = element_text(face = "bold"),
              panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
    }
  })
  
  # CONTINENT CHART
  output$continent_plot_occ <- renderPlot({
    df <- occ_filtered()
    
    df_with_continent <- df %>%
      mutate(continent = ifelse(is.na(continent) | continent == "", "Unknown", continent))
    
    if (nrow(df_with_continent) == 0) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No occurrence data", size = 8, color = "#4a6b6b") +
        theme_void()
    } else {
      plot_data <- df_with_continent %>% count(continent, source)
      continent_totals <- plot_data %>%
        group_by(continent) %>%
        summarise(total = sum(n), .groups = "drop") %>%
        mutate(percentage = round(100 * total / sum(total), 1),
               label = paste0(total, " (", percentage, "%)")) %>%
        arrange(total)
      plot_data <- plot_data %>%
        mutate(continent_factor = factor(continent, levels = continent_totals$continent))
      
      ggplot(plot_data, aes(x = continent_factor, y = n, fill = source)) +
        geom_col(position = "stack") +
        geom_text(data = continent_totals %>% mutate(continent_factor = factor(continent, levels = continent_totals$continent)),
                  aes(x = continent_factor, y = total, label = label, fill = NULL), hjust = -0.1, size = 3.5, fontface = "bold") +
        scale_fill_manual(values = source_colors, name = "Source") +
        coord_flip() +
        labs(title = "Occurrences by continent",
             subtitle = "Distribution of fossil occurrences across continents with source breakdown",
             x = NULL, y = "Number of occurrences") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14, color = "#4a6b6b"),
              plot.subtitle = element_text(size = 10, color = "#666666", margin = margin(b = 15)),
              axis.text = element_text(size = 11), legend.position = "bottom",
              legend.title = element_text(face = "bold"),
              panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.25)))
    }
  })
  
  # PALEOOCEAN CHART
  output$paleoocean_plot_occ <- renderPlot({
    df <- occ_filtered()
    
    df_with_paleoocean <- df %>%
      mutate(paleoocean = ifelse(is.na(paleoocean) | paleoocean == "", "Unknown", paleoocean))
    
    if (nrow(df_with_paleoocean) == 0) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No occurrence data", size = 8, color = "#4a6b6b") +
        theme_void()
    } else {
      plot_data <- df_with_paleoocean %>% count(paleoocean, source)
      paleoocean_totals <- plot_data %>%
        group_by(paleoocean) %>%
        summarise(total = sum(n), .groups = "drop") %>%
        mutate(percentage = round(100 * total / sum(total), 1),
               label = paste0(total, " (", percentage, "%)")) %>%
        arrange(total)
      plot_data <- plot_data %>%
        mutate(paleoocean_factor = factor(paleoocean, levels = paleoocean_totals$paleoocean))
      
      ggplot(plot_data, aes(x = paleoocean_factor, y = n, fill = source)) +
        geom_col(position = "stack") +
        geom_text(data = paleoocean_totals %>% mutate(paleoocean_factor = factor(paleoocean, levels = paleoocean_totals$paleoocean)),
                  aes(x = paleoocean_factor, y = total, label = label, fill = NULL), hjust = -0.1, size = 3.5, fontface = "bold") +
        scale_fill_manual(values = source_colors, name = "Source") +
        coord_flip() +
        labs(title = "Occurrences by paleoocean",
             subtitle = "Distribution of fossil occurrences across ancient ocean basins with source breakdown",
             x = NULL, y = "Number of occurrences") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14, color = "#4a6b6b"),
              plot.subtitle = element_text(size = 10, color = "#666666", margin = margin(b = 15)),
              axis.text = element_text(size = 11), legend.position = "bottom",
              legend.title = element_text(face = "bold"),
              panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
    }
  })
  
  # ORDER CHART
  output$order_plot_occ <- renderPlot({
    df <- occ_filtered()
    
    df_with_order <- df %>%
      mutate(order = ifelse(is.na(order) | order == "", "Unknown", order))
    
    if (nrow(df_with_order) == 0) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No occurrence data", size = 8, color = "#4a6b6b") +
        theme_void()
    } else {
      plot_data <- df_with_order %>% count(order, source)
      order_totals <- plot_data %>%
        group_by(order) %>%
        summarise(total = sum(n), .groups = "drop") %>%
        mutate(percentage = round(100 * total / sum(total), 1),
               label = paste0(total, " (", percentage, "%)")) %>%
        arrange(total)
      plot_data <- plot_data %>%
        mutate(order_factor = factor(order, levels = order_totals$order))
      
      ggplot(plot_data, aes(x = order_factor, y = n, fill = source)) +
        geom_col(position = "stack") +
        geom_text(data = order_totals %>% mutate(order_factor = factor(order, levels = order_totals$order)),
                  aes(x = order_factor, y = total, label = label, fill = NULL), hjust = -0.1, size = 3.5, fontface = "bold") +
        scale_fill_manual(values = source_colors, name = "Source") +
        coord_flip() +
        labs(title = "Occurrences by order",
             subtitle = "Taxonomic distribution of fossil occurrences with source breakdown",
             x = NULL, y = "Number of occurrences") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14, color = "#4a6b6b"),
              plot.subtitle = element_text(size = 10, color = "#666666", margin = margin(b = 15)),
              axis.text = element_text(size = 11), legend.position = "bottom",
              legend.title = element_text(face = "bold"),
              panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
    }
  })
  
  # RANK CHART
  output$rank_plot_occ <- renderPlot({
    df <- occ_filtered()
    
    df_with_rank <- df %>%
      mutate(
        rank_grouped = case_when(
          is.na(rank) | rank == "" ~ "Unknown",
          tolower(rank) %in% c("species", "genus", "family", "order") ~ tools::toTitleCase(tolower(rank)),
          TRUE ~ "Other"
        )
      )
    
    if (nrow(df_with_rank) == 0) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No occurrence data", size = 8, color = "#4a6b6b") +
        theme_void()
    } else {
      plot_data <- df_with_rank %>% count(rank_grouped, source)
      rank_totals <- plot_data %>%
        group_by(rank_grouped) %>%
        summarise(total = sum(n), .groups = "drop") %>%
        mutate(percentage = round(100 * total / sum(total), 1),
               label = paste0(total, " (", percentage, "%)")) %>%
        arrange(total)
      plot_data <- plot_data %>%
        mutate(rank_factor = factor(rank_grouped, levels = rank_totals$rank_grouped))
      
      ggplot(plot_data, aes(x = rank_factor, y = n, fill = source)) +
        geom_col(position = "stack") +
        geom_text(data = rank_totals %>% mutate(rank_factor = factor(rank_grouped, levels = rank_totals$rank_grouped)),
                  aes(x = rank_factor, y = total, label = label, fill = NULL), hjust = -0.1, size = 3.5, fontface = "bold") +
        scale_fill_manual(values = source_colors, name = "Source") +
        coord_flip() +
        labs(title = "Occurrences by taxonomic rank",
             subtitle = "Distribution by identification precision (species, genus, family, order, and other)",
             x = NULL, y = "Number of occurrences") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14, color = "#4a6b6b"),
              plot.subtitle = element_text(size = 10, color = "#666666", margin = margin(b = 15)),
              axis.text = element_text(size = 11), legend.position = "bottom",
              legend.title = element_text(face = "bold"),
              panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
    }
  })
  
  # ----- Collections tab -----
  
  # Select All / Clear All observers for Collections filters
  observeEvent(input$select_all_epochs_col, {
    updateSelectizeInput(session, "epochs_col", selected = epoch_choices_col)
  })
  observeEvent(input$clear_all_epochs_col, {
    updateSelectizeInput(session, "epochs_col", selected = character(0))
  })
  
  observeEvent(input$select_all_periods_col, {
    updateSelectizeInput(session, "periods_col", selected = period_choices_col)
  })
  observeEvent(input$clear_all_periods_col, {
    updateSelectizeInput(session, "periods_col", selected = character(0))
  })
  
  observeEvent(input$select_all_continent_col, {
    updateSelectizeInput(session, "continent_col", selected = continent_choices_col)
  })
  observeEvent(input$clear_all_continent_col, {
    updateSelectizeInput(session, "continent_col", selected = character(0))
  })
  
  observeEvent(input$select_all_country_col, {
    updateSelectizeInput(session, "country_col", selected = country_choices_col)
  })
  observeEvent(input$clear_all_country_col, {
    updateSelectizeInput(session, "country_col", selected = character(0))
  })
  
  observeEvent(input$select_all_paleocean_col, {
    updateSelectizeInput(session, "paleocean_col", selected = paleoocean_choices_col)
  })
  observeEvent(input$clear_all_paleocean_col, {
    updateSelectizeInput(session, "paleocean_col", selected = character(0))
  })
  
  apply_age_thresh_col <- function(df, choice) {
    if (choice == "any" || !"age_range" %in% names(df)) return(df)
    thr <- as.numeric(choice)
    df %>% filter(!is.na(age_range) & age_range <= thr)
  }
  
  occ_coll_ids_filtered <- reactive({     
    ids <- occ_filtered()$coll_id %||% character(0)
    # Remove NA values - we only want valid collection IDs
    ids <- ids[!is.na(ids) & ids != ""]
    unique(ids)
  })
  
  col_pool <- reactive({
    out <- rv$col
    if (isTRUE(input$sync_col_with_occ)) {
      ids <- occ_coll_ids_filtered()
      out <- out %>% filter(coll_id %in% ids)
    }
    out <- overlaps_time(out, input$epochs_col, input$periods_col)
    out <- apply_age_thresh_col(out, input$age_thresh_col)
    out <- apply_geog(out, input$continent_col, input$country_col, input$paleocean_col)
    out
  })
  
  # Dynamic source choices for Collections
  observeEvent(list(input$sync_col_with_occ, input$epochs_col, input$periods_col,
                    input$age_thresh_col, input$continent_col, input$paleocean_col), {
                      pool <- col_pool()
                      present <- intersect(source_levels_col_all, us(pool$collection_source))
                      if (length(present) == 0) present <- source_levels_col_all[0]
                      new_selected <- intersect(input$source_filter_col %||% character(0), present)
                      if (length(new_selected) == 0) new_selected <- present
                      updateCheckboxGroupInput(session, "source_filter_col",
                                               choices = present, selected = new_selected, inline = TRUE)
                    }, ignoreInit = FALSE)
  
  col_filtered <- reactive({
    out <- col_pool()
    if ("collection_source" %in% names(out) && length(input$source_filter_col)) {
      out <- out %>% filter(collection_source %in% input$source_filter_col)
    }
    out
  })
  
  output$source_counts_col <- renderTable({
    tab <- col_filtered() %>%
      count(collection_source, name = "n", sort = FALSE) %>%
      tidyr::complete(collection_source = factor(source_levels, levels = source_levels), 
                      fill = list(n = 0))
    
    total <- sum(tab$n, na.rm = TRUE)
    tab <- dplyr::bind_rows(tab, tibble::tibble(collection_source = "Total", n = total))
    
    names(tab) <- c("Source", "Count")
    tab
  }, bordered = TRUE, striped = TRUE, digits = 0)
  
  output$map_col <- renderLeaflet({ 
    leaflet(options = leafletOptions(
      worldCopyJump = FALSE,
      maxBounds = list(list(-180, -90), list(180, 90)),
      maxBoundsViscosity = 1.0,
      minZoom = 2,                # Prevent zooming out beyond initial view
      maxZoom = 18                # Allow zooming in to street level
    )) %>% 
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)
  })
  outputOptions(output, "map_col", suspendWhenHidden = FALSE)
  
  observe({
    pts <- col_filtered() %>% filter(!is.na(latitude), !is.na(longitude))
    if (nrow(pts) == 0) { 
      leafletProxy("map_col") %>% clearMarkers() %>% clearControls()
      return() 
    }
    leafletProxy("map_col") %>% clearMarkers() %>% clearControls() %>%
      addCircleMarkers(
        data = pts %>% mutate(source_chr = as.character(collection_source)),
        lng = ~longitude, lat = ~latitude,
        radius = 6, opacity = 0.9, fillOpacity = 0.75,
        color = "#FFFFFF", weight = 0.5,  # Thin white border
        fillColor = ~ifelse(is.na(source_chr), "#808080", pal_source(source_chr)),
        popup = ~paste0(
          "<b>Collection ", (collection_number %||% coll_id %||% "ID?"), "</b><br/>",
          "Period: ", (early_period %||% ""), " – ", (late_period %||% ""), "<br/>",
          "Epoch: ", (early_epoch %||% ""),  " – ", (late_epoch %||% ""), "<br/>",
          "Source: ", as.character(collection_source) %||% ""
        )
      ) %>%
      addLegend(
        "topright", pal = pal_source,
        values = factor(source_levels, levels = source_levels),
        title = "Collection source", opacity = 0.9
      )
  })
  
  output$plot_types_col <- renderPlot({
    df <- col_filtered()
    if (!"continent" %in% names(df)) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Continents: 'continent' column not found in Data_S2",
                 size = 5) +
        theme_void()
    } else {
      tab <- df %>%
        mutate(continent = ifelse(is.na(continent) | continent == "", "Unknown", continent)) %>%
        count(continent, name = "n", sort = TRUE) %>%
        mutate(
          percentage = round(100 * n / sum(n), 1),
          label = paste0(n, " (", percentage, "%)")
        )
      
      if (nrow(tab) == 0) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Continents: no data",
                   size = 5) +
          theme_void()
      } else {
        # Create color palette
        colors <- c(
          "Africa" = "#EDAD08",
          "Asia" = "#0F8554",           
          "Europe" = "#38A6A5",
          "North America" = "#1D6996",
          "South America" = "#5F4690",
          "Oceania" = "#CC503E",
          "Offshore samples" = "#94346E",
          "Antarctica" = "#E17C05",
          "Unknown" = "#7f8c8d"
        )
        
        ggplot(tab, aes(x = reorder(continent, n), y = n, fill = continent)) +
          geom_col() +
          geom_text(aes(label = label), hjust = -0.1, size = 3.5) +
          scale_fill_manual(values = colors, na.value = "#95a5a6") +
          coord_flip() +
          labs(
            title = "Collection localities by continent",
            x = NULL,
            y = "Count"
          ) +
          theme_minimal() +
          theme(
            legend.position = "none",
            plot.title = element_text(face = "bold", size = 14),
            axis.text = element_text(size = 11),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
      }
    }
  })
  
  output$table_col <- renderDT({
    df <- col_filtered()
    show_cols <- c("collection_number","coll_id","country","state","continent","paleoocean",
                   "early_period","late_period","early_epoch","late_epoch",
                   "early_interval","late_interval","max_ma","min_ma","age_range",
                   "latitude","longitude","collection_source","fossil_type")
    show_cols <- intersect(show_cols, names(df))
    
    dt <- datatable(
      df %>% select(all_of(show_cols)) %>% rename(`age_range (Myr)` = age_range),
      options = list(pageLength = 12, scrollX = TRUE),
      rownames = FALSE
    )
    
    dt %>% formatStyle(
      "collection_source",
      target = "row",
      backgroundColor = styleEqual(
        c("PBDB","Literature","PBDB_U"),
        c(NA, NA, "#e8f4ff")
      )
    )
  })
  
  output$download_col <- downloadHandler(
    filename = function() paste0("fins_collections_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(col_filtered(), file, row.names = FALSE)
  )
  
  # ----- References tab -----
  ref_keys_from_occ <- reactive({
    rf <- occ_filtered()
    if ("reference_key" %in% names(rf)) unique(na.omit(rf$reference_key)) else character(0)
  })
  
  ref_keys_from_col <- reactive({
    cf <- col_filtered()
    if ("reference_key" %in% names(cf)) unique(na.omit(cf$reference_key)) else character(0)
  })
  
  refs_pool <- reactive({
    mode <- input$sync_refs_mode %||% "both"  # Default to "both" (current behavior)
    
    # Get keys based on selected mode
    keys <- switch(mode,
                   "occ" = ref_keys_from_occ(),
                   "col" = ref_keys_from_col(),
                   "both" = union(ref_keys_from_occ(), ref_keys_from_col()),
                   "none" = NULL
    )
    
    # If "none" mode, return all references
    if (mode == "none") return(refs)
    
    # If no keys found or reference_key column missing, return empty
    if (length(keys) == 0 || !"reference_key" %in% names(refs)) return(refs[0, ])
    
    # Filter references by keys
    refs %>% filter(reference_key %in% keys)
  })
  
  refs_lang_choices <- reactive({
    pool <- refs_pool()
    if (!"language" %in% names(pool)) pool$language <- NA_character_
    us(ifelse(is.na(pool$language) | pool$language == "", "Unknown", pool$language))
  })
  
  observeEvent(list(input$sync_refs_mode, occ_filtered(), col_filtered()), {
    ch <- refs_lang_choices()
    sel <- intersect(input$ref_langs %||% character(0), ch)
    if (length(sel) == 0) sel <- ch
    updateSelectizeInput(session, "ref_langs", choices = ch, selected = sel, server = TRUE)
  }, ignoreInit = FALSE)
  
  refs_filtered <- reactive({
    rf <- refs_pool()
    if (!"language" %in% names(rf)) rf$language <- NA_character_
    rf <- rf %>% mutate(language_disp = ifelse(is.na(language) | language == "", "Unknown", language))
    keep <- c("reference_display","author","year","title","journal","language_disp")
    keep <- keep[keep %in% names(rf)]
    rf %>% select(all_of(keep)) %>% rename(Language = language_disp, Reference = reference_display)
  })
  
  output$refs_summary <- renderTable({
    data.frame(`Papers` = nrow(refs_filtered()))
  }, bordered = TRUE, striped = TRUE, digits = 0, colnames = FALSE)
  
  output$refs_table <- renderDT({
    datatable(refs_filtered(), options = list(pageLength = 12), rownames = FALSE)
  })
  
  output$download_refs <- downloadHandler(
    filename = function() paste0("fins_references_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(refs_filtered(), file, row.names = FALSE)
  )
  
  # ---- Add Data tab: PBDB Collections ----
  pbdb_col_raw <- reactive({
    req(input$pbdb_col_file)
    read_pbdb_csv(input$pbdb_col_file, header = TRUE, delim_choice = "Auto")
  })
  
  pbdb_col_processed <- reactive({
    df_raw <- pbdb_col_raw()
    validate(need(!is.null(df_raw), "Upload a Collections CSV to preview."))
    
    df <- df_raw
    names(df) <- tolower(names(df))
    
    # Synonym mapping
    syn_map <- list(
      collection_no  = "collection_number",
      "collection_no." = "collection_number",
      longitude      = "lng",
      lon            = "lng",
      latitude       = "lat",
      cc2            = "cc"
    )
    for (s in names(syn_map)) {
      if (s %in% names(df) && !(syn_map[[s]] %in% names(df))) {
        df[[ syn_map[[s]] ]] <- df[[ s ]]
      }
    }
    
    needed <- c("collection_number","formation","lng","lat","collection_name","collection_aka",
                "early_interval","late_interval","max_ma","min_ma","cc","state",
                "latlng_basis","geogcomments","ref_author","member","stratscale")
    df <- ensure_cols(df, needed)
    
    join_comments <- function(a, b) {
      a <- ifelse(is.na(a) | a == "", NA, a)
      b <- ifelse(is.na(b) | b == "", NA, b)
      dplyr::case_when(
        !is.na(a) & !is.na(b) ~ paste0(a, "; ", b),
        !is.na(a) ~ a,
        !is.na(b) ~ b,
        TRUE ~ NA_character_
      )
    }
    
    late_final <- dplyr::coalesce(df[["late_interval"]], df[["early_interval"]])
    m_late_from_early <- is.na(df[["late_interval"]]) & !is.na(df[["early_interval"]])
    
    comments <- join_comments(df[["latlng_basis"]], df[["geogcomments"]])
    m_collection_comments <- !is.na(comments) & ( (!is.na(df[["latlng_basis"]]) & df[["latlng_basis"]] != "") |
                                                    (!is.na(df[["geogcomments"]]) & df[["geogcomments"]] != "") )
    
    max_ma_num <- suppressWarnings(as.numeric(df[["max_ma"]]))
    min_ma_num <- suppressWarnings(as.numeric(df[["min_ma"]]))
    age_range  <- ifelse(!is.na(max_ma_num) & !is.na(min_ma_num),
                         round(max_ma_num - min_ma_num, 2), NA_real_)
    m_age_range <- !is.na(age_range)
    
    cc_raw   <- df[["cc"]]
    country  <- expand_country_codes(cc_raw)
    m_country <- !is.na(cc_raw) & cc_raw != "" & !is.na(country) & (toupper(trimws(as.character(cc_raw))) != country)
    
    continent <- country_to_continent(country)
    m_continent <- !is.na(country) & country != "" & !is.na(continent)
    
    lat_num <- suppressWarnings(as.numeric(df[["lat"]]))
    lat_band <- dplyr::case_when(
      is.na(lat_num)                      ~ NA_character_,
      lat_num >=  23.436                  ~ "Northern Temperate",
      lat_num <= -23.436                  ~ "Southern Temperate",
      TRUE                                ~ "Tropical"
    )
    m_lat_band <- !is.na(lat_band)
    
    time_interval_type <- vapply(df[["early_interval"]], classify_interval_type, character(1))
    m_time_type <- !is.na(time_interval_type)
    
    collection_source <- factor("PBDB_U", levels = c("PBDB","Literature","PBDB_U"))
    m_collection_source <- TRUE
    
    ee <- resolve_interval_hierarchy(df[["early_interval"]])
    le <- resolve_interval_hierarchy(late_final)
    
    lat_num <- suppressWarnings(as.numeric(coalesce_col(df, c("lat", "latitude"))))
    lon_num <- suppressWarnings(as.numeric(coalesce_col(df, c("lng", "lon", "longitude"))))
    
    # Paleocoordinates
    paleolon   <- rep(NA_real_, length(lat_num))
    paleolat   <- rep(NA_real_, length(lat_num))
    m_paleo    <- rep(FALSE,     length(lat_num))
    paleo_note <- rep(NA_character_, length(lat_num))
    
    do_paleo <- isTRUE("compute" %in% (input$pbdb_col_paleo %||% character(0)))
    
    key <- .make_paleo_key(df_raw)
    if (!is.null(key) && !is.null(rv$paleo_cache[[key]])) {
      cached <- rv$paleo_cache[[key]]
      coll_ids_raw <- if (!all(is.na(df[["collection_number"]]))) df[["collection_number"]] else NA
      coll_ids_std <- ifelse(!is.na(coll_ids_raw), paste0("PBDB_", coll_ids_raw), NA_character_)
      if (!is.null(cached$collection_number) && any(!is.na(coll_ids_std))) {
        m <- match(coll_ids_std, cached$collection_number)
        okm <- is.finite(m)
        paleolon[okm] <- cached$paleolongitude[m[okm]]
        paleolat[okm] <- cached$paleolatitude[m[okm]]
        m_paleo[okm]  <- is.finite(paleolon[okm]) & is.finite(paleolat[okm])
        paleo_note[okm] <- ifelse(m_paleo[okm], "from cache", NA_character_)
      } else if (length(cached$paleolongitude) == length(paleolon)) {
        paleolon <- cached$paleolongitude
        paleolat <- cached$paleolatitude
        m_paleo  <- is.finite(paleolon) & is.finite(paleolat)
        paleo_note[m_paleo] <- "from cache"
      }
    }
    
    need_compute <- do_paleo && any(!m_paleo) && requireNamespace("rgplates", quietly = TRUE)
    
    if (need_compute) {
      withProgress(message = "Reconstructing paleocoordinates…", value = 0, {
        n <- length(lat_num)
        for (i in seq_len(n)) {
          if (m_paleo[i]) { incProgress(1/n); next }
          
          res <- .paleo_mc_median(
            lon    = lon_num[i],
            lat    = lat_num[i],
            min_ma = max_ma_num[i],
            max_ma = min_ma_num[i],
            n = 30
          )
          
          paleolon[i]   <- res$xy[1]
          paleolat[i]   <- res$xy[2]
          m_paleo[i]    <- isTRUE(res$ok)
          paleo_note[i] <- res$reason
          incProgress(1/n)
        }
      })
      
      if (!is.null(key)) {
        rv$paleo_cache[[key]] <- list(
          collection_number = ifelse(!is.na(df[["collection_number"]]),
                                     paste0("PBDB_", df[["collection_number"]]),
                                     NA_character_),
          paleolongitude = paleolon,
          paleolatitude  = paleolat
        )
      }
    } else if (do_paleo && !requireNamespace("rgplates", quietly = TRUE)) {
      showNotification("rgplates not available — using cached paleocoordinates (if any).", type = "warning", duration = 8)
    }
    
    # Paleoocean assignment
    paleoocean   <- rep(NA_character_, length(lat_num))
    m_paleoocean <- rep(FALSE, length(lat_num))
    
    do_ocean <- isTRUE("ocean" %in% (input$pbdb_col_paleo %||% character(0)))
    
    if (do_ocean) {
      have_coords <- is.finite(paleolon) & is.finite(paleolat)
      n_have <- sum(have_coords, na.rm = TRUE)
      if (n_have == 0) {
        showNotification(
          "Assign paleooceans: compute paleocoordinates first (toggle above).",
          type = "error", duration = 8
        )
      } else {
        withProgress(message = "Assigning paleooceans from existing paleocoords (30×)…", value = 0, {
          n <- length(lat_num)
          for (i in seq_len(n)) {
            if (!have_coords[i]) { incProgress(1/n); next }
            
            a_min <- suppressWarnings(min(max_ma_num[i], min_ma_num[i], na.rm = TRUE))
            a_max <- suppressWarnings(max(max_ma_num[i], min_ma_num[i], na.rm = TRUE))
            if (!is.finite(a_min) || !is.finite(a_max)) { incProgress(1/n); next }
            
            ages <- if (a_min == a_max) rep(a_min, 30) else stats::runif(30, min = a_min, max = a_max)
            epochs <- vapply(ages, .epoch_from_age_ma, character(1))
            
            oceans_i <- vapply(epochs, function(ep) {
              .classify_ocean(ep, paleolon[i], paleolat[i])
            }, character(1))
            
            oce_mode <- .mode1(oceans_i)
            if (!is.na(oce_mode)) {
              paleoocean[i]   <- oce_mode
              m_paleoocean[i] <- TRUE
            }
            incProgress(1/n)
          }
        })
        
        ok_o <- sum(m_paleoocean, na.rm = TRUE); n <- length(m_paleoocean)
        showNotification(sprintf("Paleooceans assigned for %d/%d rows (mode of 30).", ok_o, n),
                         type = if (ok_o > 0) "message" else "warning", duration = 6)
      }
    }
    
    early_epoch  <- ee$epoch
    early_period <- ee$period
    early_era    <- ee$era
    
    late_epoch   <- le$epoch
    late_period  <- le$period
    late_era     <- le$era
    
    m_early_epoch  <- !is.na(early_epoch)
    m_late_epoch   <- !is.na(late_epoch)
    m_early_period <- !is.na(early_period)
    m_late_period  <- !is.na(late_period)
    m_early_era    <- !is.na(early_era)
    m_late_era     <- !is.na(late_era)
    
    out <- tibble::tibble(
      collection_number   = dplyr::if_else(
        !is.na(df[["collection_number"]]) & df[["collection_number"]] != "",
        paste0("PBDB_", as.character(df[["collection_number"]])),
        NA_character_
      ),
      formation           = df[["formation"]],
      longitude           = suppressWarnings(as.numeric(df[["lng"]])),
      latitude            = lat_num,
      collection_name     = df[["collection_name"]],
      collection_aka      = df[["collection_aka"]],
      early_interval      = df[["early_interval"]],
      late_interval       = late_final,
      early_epoch         = early_epoch,
      late_epoch          = late_epoch,
      early_period        = early_period,
      late_period         = late_period,
      early_era           = early_era,
      late_era            = late_era,
      max_ma              = max_ma_num,
      min_ma              = min_ma_num,
      age_range           = age_range,
      country             = country,
      state               = df[["state"]],
      continent           = continent,
      latitude_band       = lat_band,
      collection_comments = comments,
      time_interval_type  = time_interval_type,
      collection_source   = collection_source,
      reference           = df[["ref_author"]],
      paleolatitude       = paleolat,
      paleolongitude      = paleolon,
      paleoocean          = paleoocean,
      member              = df[["member"]],
      stratscale          = df[["stratscale"]]
    )
    
    out$m_collection_comments  <- m_collection_comments
    out$m_continent            <- m_continent
    out$m_latitudinal_band     <- m_lat_band
    out$m_age_range            <- m_age_range
    out$m_time_interval_type   <- m_time_type
    out$m_collection_source    <- m_collection_source
    out$m_late_from_early      <- m_late_from_early
    out$m_early_epoch   <- m_early_epoch
    out$m_late_epoch    <- m_late_epoch
    out$m_early_period  <- m_early_period
    out$m_late_period   <- m_late_period
    out$m_early_era     <- m_early_era
    out$m_late_era      <- m_late_era
    out$m_paleocoords   <- m_paleo
    out$m_paleoocean <- m_paleoocean
    
    out
  })
  
  pbdb_col_aligned <- reactive({
    out <- align_to_template(pbdb_col_processed(), rv$col)
    out
  })
  
  output$pbdb_col_preview <- renderDT({
    df <- pbdb_col_aligned()
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    
    show <- head(df, 100)
    
    dt <- DT::datatable(
      show,
      options = list(
        pageLength = 10, scrollX = TRUE,
        columnDefs = list(
          list(targets = grep("^m_", names(show)) - 1L, visible = FALSE)
        )
      ),
      rownames = FALSE
    )
    
    tint <- "#e6f3ff"
    
    color_cell <- function(col, mask) {
      if (col %in% names(show) && mask %in% names(show)) {
        dt <<- dt %>% DT::formatStyle(
          columns = col,
          valueColumns = mask,
          backgroundColor = DT::styleEqual(c(TRUE, FALSE), c(tint, NA))
        )
      }
    }
    
    color_cell("collection_comments", "m_collection_comments")
    color_cell("continent",           "m_continent")
    color_cell("latitude_band",    "m_latitudinal_band")
    color_cell("age_range",           "m_age_range")
    color_cell("time_interval_type",  "m_time_interval_type")
    color_cell("collection_source",   "m_collection_source")
    color_cell("late_interval",       "m_late_from_early")
    color_cell("early_epoch",  "m_early_epoch")
    color_cell("late_epoch",   "m_late_epoch")
    color_cell("early_period", "m_early_period")
    color_cell("late_period",  "m_late_period")
    color_cell("early_era",    "m_early_era")
    color_cell("late_era",     "m_late_era")
    color_cell("paleolatitude",  "m_paleocoords")
    color_cell("paleolongitude", "m_paleocoords")
    color_cell("paleoocean", "m_paleoocean")
    
    dt
  })
  
  output$pbdb_col_status <- renderUI({
    n <- nrow(pbdb_col_aligned())
    ok <- sum(pbdb_col_aligned()[["m_paleocoords"]] %||% FALSE, na.rm = TRUE)
    need_pkg <- !requireNamespace("rgplates", quietly = TRUE)
    do_paleo <- "compute" %in% (input$pbdb_col_paleo %||% character(0))
    HTML(paste0(
      "<div style='font-size:12px;margin-top:6px;'>",
      "<span style='display:inline-block;width:14px;height:14px;background:#fff7e6;border:1px solid #f2e1cc;margin-right:6px;'></span>",
      "Cells highlighted = filled/derived by the app",
      if (do_paleo)
        sprintf("<br/>Paleocoords: %s/%s rows succeeded.", ok, n) else "",
      if (need_pkg)
        "<br/><span style='color:#a33;'>Note: 'rgplates' not available—paleocoordinates will be NA.</span>" else "",
      "</div>"
    ))
  })
  
  # ---- PBDB Occurrences ----
  pbdb_occ_raw <- reactive({
    req(input$pbdb_occ_file)
    read_pbdb_csv(input$pbdb_occ_file, header = TRUE, delim_choice = "Auto")
  })
  
  observeEvent(pbdb_occ_raw(), {
    df_raw <- pbdb_occ_raw()
    if (is.null(df_raw) || !nrow(df_raw)) return()
    
    nms <- tolower(names(df_raw))
    
    coll_no_vec <- if ("collection_number" %in% nms) df_raw[[which(nms == "collection_number")[1]]] else
      if ("collection_number" %in% nms) df_raw[[which(nms == "collection_number")[1]]] else NA
    
    coll_no_chr <- suppressWarnings(as.character(coll_no_vec))
    occ_coll_ids <- unique(na.omit(ifelse(!is.na(coll_no_chr) & coll_no_chr != "",
                                          paste0("PBDB_", coll_no_chr),
                                          NA_character_)))
    
    known_ids <- character(0)
    if ("coll_id" %in% names(rv$col)) {
      known_ids <- rv$col$coll_id
    } else if ("collection_number" %in% names(rv$col)) {
      known_ids <- rv$col$collection_number
    }
    known_ids <- unique(na.omit(as.character(known_ids)))
    
    missing_ids <- setdiff(occ_coll_ids, known_ids)
    
    if (length(missing_ids) > 0) {
      showNotification(
        sprintf("Occurrences refer to %d collection(s) not present in the Collections tab.", length(missing_ids)),
        type = "error", duration = 8
      )
      
      to_show <- head(sort(missing_ids), 200)
      extras  <- if (length(missing_ids) > 200)
        sprintf("…and %d more not shown.", length(missing_ids) - 200) else ""
      
      showModal(modalDialog(
        title = sprintf("%d missing collection(s) in Collections", length(missing_ids)),
        div(
          p("These collection IDs are referenced by the uploaded Occurrences but do not exist in the current Collections data. ",
            "Please append the missing Collections first, then re-upload the Occurrences."),
          tags$hr(),
          tags$strong("Missing collection IDs (PBDB_*):"),
          tags$pre(paste(to_show, collapse = "\n")),
          if (nzchar(extras)) p(em(extras))
        ),
        easyClose = TRUE, size = "l"
      ))
    } else {
      showNotification("All occurrence collections are present in the Collections tab ✔️", type = "message", duration = 4)
    }
  })
  
  pbdb_occ_processed <- reactive({
    df <- pbdb_occ_raw()
    validate(need(!is.null(df), "Upload an Occurrences CSV to preview."))
    
    # Process basic occurrence data
    out <- process_pbdb_occurrences(df, keep_only_needed = TRUE)
    
    # Get current collections data
    col_data <- rv$col
    
    # Check if we have collections to join with
    if (is.null(col_data) || nrow(col_data) == 0) {
      showNotification("No collections data available. Upload collections first to enrich occurrences.", 
                       type = "warning", duration = 5)
      return(out)
    }
    
    # Columns to populate from collections (using collection column names)
    cols_from_collections <- c(
      "max_ma", "min_ma", "age_range",
      "early_interval", "late_interval",
      "early_epoch", "late_epoch",
      "early_period", "late_period",
      "early_era", "late_era",
      "time_interval_type",
      "latitude", "longitude",
      "continent", "latitude_band",
      "paleoocean",
      "paleolatitude", "paleolongitude",
      "locality_id"
    )
    
    # Keep only columns that exist in collections
    cols_available <- intersect(cols_from_collections, names(col_data))
    
    if (length(cols_available) == 0) {
      showNotification("Collections data doesn't have expected columns for enrichment.", 
                       type = "warning", duration = 5)
      return(out)
    }
    
    # Prepare collections data for join (select coll_id and relevant columns)
    col_for_join <- col_data %>%
      select(coll_id, all_of(cols_available)) %>%
      distinct(coll_id, .keep_all = TRUE)
    
    # Join occurrences with collections
    out_enriched <- out %>%
      left_join(col_for_join, by = "coll_id", suffix = c("", "_from_col"))
    
    # For any columns that exist in both, use the collection value if occurrence value is NA
    for (col_name in cols_available) {
      if (col_name %in% names(out)) {
        from_col_name <- paste0(col_name, "_from_col")
        if (from_col_name %in% names(out_enriched)) {
          out_enriched[[col_name]] <- dplyr::coalesce(out_enriched[[col_name]], out_enriched[[from_col_name]])
          out_enriched[[from_col_name]] <- NULL
        }
      } else {
        # Column doesn't exist in occurrences, it was added from collections - rename if needed
        from_col_name <- paste0(col_name, "_from_col")
        if (from_col_name %in% names(out_enriched)) {
          names(out_enriched)[names(out_enriched) == from_col_name] <- col_name
        }
      }
    }
    
    # Rename columns to match occurrence schema
    if ("time_interval_type" %in% names(out_enriched)) {
      out_enriched$int_type <- out_enriched$time_interval_type
      out_enriched$time_interval_type <- NULL
    }
    if ("paleolatitude" %in% names(out_enriched)) {
      out_enriched$paleolat <- out_enriched$paleolatitude
      out_enriched$paleolatitude <- NULL
    }
    if ("paleolongitude" %in% names(out_enriched)) {
      out_enriched$paleolon <- out_enriched$paleolongitude
      out_enriched$paleolongitude <- NULL
    }
    
    # Add marker column to show which values came from collections
    out_enriched$m_enriched_from_col <- out_enriched$coll_id %in% col_for_join$coll_id
    
    # Count successful joins
    
    n_matched <- sum(out_enriched$m_enriched_from_col, na.rm = TRUE)
    n_total <- nrow(out_enriched)
    
    if (n_matched > 0) {
      showNotification(
        sprintf("Enriched %d/%d occurrences with collection data.", n_matched, n_total),
        type = "message", duration = 4
      )
    } else {
      showNotification(
        "No occurrences matched with collections. Check collection IDs.",
        type = "warning", duration = 5
      )
    }
    
    out_enriched
  })
  
  pbdb_occ_aligned <- reactive({
    df <- align_to_template(pbdb_occ_processed(), rv$occ)
    # Keep only columns that exist in the original FINS schema (plus marker columns)
    marker_cols <- grep("^m_", names(df), value = TRUE)
    keep_cols <- intersect(names(df), c(occ_original_cols, marker_cols))
    # Exclude derived columns that shouldn't appear in preview
    exclude_cols <- c("reference_display", "reference_key")
    keep_cols <- setdiff(keep_cols, exclude_cols)
    df[, keep_cols, drop = FALSE]
  })
  
  output$pbdb_occ_preview <- renderDT({
    df <- pbdb_occ_aligned()
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    
    show <- head(df, 100)
    
    dt <- DT::datatable(
      show,
      options = list(
        pageLength = 10, 
        scrollX = TRUE,
        columnDefs = list(
          list(targets = grep("^m_", names(show)) - 1L, visible = FALSE)
        )
      ),
      rownames = FALSE
    )
    
    # Highlight enriched cells in light blue (from collections)
    if ("m_enriched_from_col" %in% names(show)) {
      tint <- "#e6f3ff"
      
      enriched_cols <- c("max_ma", "min_ma", "age_range", "early_interval", "late_interval",
                         "early_epoch", "late_epoch", "early_period", "late_period",
                         "early_era", "late_era", "int_type", "latitude", "longitude",
                         "continent", "latitude_band", "paleoocean", "paleolat", "paleolon",
                         "locality_id")
      
      for (col in intersect(enriched_cols, names(show))) {
        dt <- dt %>% DT::formatStyle(
          columns = col,
          valueColumns = "m_enriched_from_col",
          backgroundColor = DT::styleEqual(c(TRUE, FALSE), c(tint, NA))
        )
      }
    }
    
    # Highlight taxonomy columns populated by the pipeline
    tint <- "#e6f3ff"
    
    color_cell <- function(col, mask) {
      if (col %in% names(show) && mask %in% names(show)) {
        dt <<- dt %>% DT::formatStyle(
          columns = col,
          valueColumns = mask,
          backgroundColor = DT::styleEqual(c(TRUE, FALSE), c(tint, NA))
        )
      }
    }
    
    color_cell("modified_identified_name", "m_modified_identified_name")
    color_cell("accepted_name", "m_accepted_name")
    color_cell("genus", "m_genus")
    color_cell("family", "m_family")
    color_cell("order", "m_order")
    color_cell("superorder", "m_superorder")
    
    dt
  })
  
  output$pbdb_occ_status <- renderUI({ NULL })
  
  observeEvent(input$pbdb_occ_apply, {
    mode <- input$pbdb_occ_mode
    df_new <- pbdb_occ_aligned()
    if (mode == "append") {
      rv$occ <- dplyr::bind_rows(
        align_to_template(rv$occ, rv$occ)[, union(names(rv$occ), names(df_new)), drop = FALSE],
        df_new[, union(names(rv$occ), names(df_new)), drop = FALSE]
      )
    } else if (mode == "replace") {
      rv$occ <- df_new
    }
    if (mode != "preview") {
      output$pbdb_occ_status <- renderUI({
        tags$div(class = "text-success",
                 sprintf("Occurrences %s: %s rows now in memory.",
                         if (mode == "append") "appended" else "replaced",
                         format(nrow(rv$occ), big.mark=",")))
      })
    }
  })
  
  # ----- Apply PBDB Collections -----
  observeEvent(input$pbdb_col_apply, {
    mode <- input$pbdb_col_mode
    
    df_new <- isolate(pbdb_col_aligned())
    df_new <- df_new %>% dplyr::select(-tidyselect::matches("^m_"))
    req(!is.null(df_new), nrow(df_new) > 0)
    
    if (!"coll_id" %in% names(df_new)) df_new$coll_id <- NA_character_
    df_new <- df_new %>%
      dplyr::mutate(
        coll_id = dplyr::coalesce(as.character(coll_id),
                                  as.character(collection_number),
                                  as.character(collection_number))
      )
    
    if ("collection_source" %in% names(df_new)) {
      df_new$collection_source <- as.character(df_new$collection_source)
    }
    
    keep_cols <- union(names(rv$col), names(df_new))
    if (mode == "append") {
      rv$col <- dplyr::bind_rows(
        tibble::as_tibble(rv$col),
        tibble::as_tibble(df_new)
      )
    } else if (mode == "replace") {
      rv$col <- tibble::as_tibble(df_new)
    } else {
      return(invisible(NULL))
    }
    
    num_cols <- c("longitude","latitude","max_ma","min_ma","age_range","paleolatitude","paleolongitude")
    for (nm in intersect(num_cols, names(rv$col))) {
      rv$col[[nm]] <- suppressWarnings(as.numeric(rv$col[[nm]]))
    }
    
    if ("collection_source" %in% names(rv$col)) {
      rv$col$collection_source <- factor(rv$col$collection_source,
                                         levels = c("PBDB","Literature","PBDB_U"))
    }
    
    showNotification(
      sprintf("Collections %s: %s rows now in memory.",
              if (mode == "append") "appended" else "replaced",
              format(nrow(rv$col), big.mark=",")),
      type = "message", duration = 5
    )
    
    isolate({
      if (isTRUE(input$sync_col_with_occ)) {
        showNotification(
          "Note: 'Sync with current Occurrence filters' is ON — new Collections may be hidden if no current occurrences reference them.",
          type = "warning", duration = 7
        )
      }
    })
  })
  
}
