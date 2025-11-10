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
    
    colno_raw <- if (!all(is.na(df[["collection_number"]]))) df[["collection_number"]] else df[["collection_number"]]
    colno_chr <- suppressWarnings(as.character(colno_raw))
    coll_id <- ifelse(!is.na(colno_chr) & colno_chr != "",
                      paste0("PBDB_", colno_chr), NA_character_)
    
    out <- tibble::tibble(
      occurrence_number = occurrence_number,
      coll_id           = coll_id,
      collection_number     = coll_id,
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
    
    if (!keep_only_needed) {
      extras <- df[, setdiff(names(df), names(out)), drop = FALSE]
      extras <- tibble::as_tibble(lapply(extras, function(x) if (is.list(x)) sapply(x, as.character) else x))
      out <- dplyr::bind_cols(out, extras)
    }
    
    out
  }
  
  # ---- Welcome tab ----
  output$readme_ui_about <- renderUI({
    readme_path <- here::here("data", "README_about.md")
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
        p(strong("FINS"), " (FossIl NeoselachianS) is a comprehensive global database of fossil sharks, rays, and skates."),
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
    readme_path <- here::here("data", "README_guide.md")
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
    out <- apply_geog(out, input$continent_occ, input$paleocean_occ)
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
      worldCopyJump = FALSE,
      maxBounds = list(list(-180, -90), list(180, 90)),
      maxBoundsViscosity = 1.0,
      minZoom = 2,        # ← Can't zoom OUT past level 2
      maxZoom = 18        # ← Can zoom IN to street level
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
        radius = 5, opacity = 1, fillOpacity = 0.75,
        color = "#FFFFFF", weight = 1.5,  # White border for visibility on satellite
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
    show_cols <- c("collection_number","name_display","order","superorder","family","genus","rank","status",
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
  
  # ----- Collections tab -----
  apply_age_thresh_col <- function(df, choice) {
    if (choice == "any" || !"age_range" %in% names(df)) return(df)
    thr <- as.numeric(choice)
    df %>% filter(!is.na(age_range) & age_range <= thr)
  }
  
  occ_coll_ids_filtered <- reactive({ 
    unique(occ_filtered()$coll_id %||% NA_character_) 
  })
  
  col_pool <- reactive({
    out <- rv$col
    if (isTRUE(input$sync_col_with_occ)) {
      ids <- occ_coll_ids_filtered()
      out <- out %>% filter(coll_id %in% ids)
    }
    out <- overlaps_time(out, input$epochs_col, input$periods_col)
    out <- apply_age_thresh_col(out, input$age_thresh_col)
    out <- apply_geog(out, input$continent_col, input$paleocean_col)
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
  
  output$map_col <- renderLeaflet({ 
    leaflet(options = leafletOptions(
      worldCopyJump = FALSE,
      maxBounds = list(list(-180, -90), list(180, 90)),
      maxBoundsViscosity = 1.0
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
        radius = 6, opacity = 1, fillOpacity = 0.75,
        color = "#FFFFFF", weight = 1.5,  # White border for visibility
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
    if (!"fossil_type" %in% names(df)) {
      plot.new()
      title("Fossil types: 'fossil_type' column not found in Data_S1")
      return()
    }
    
    tab <- df %>%
      mutate(fossil_type = ifelse(is.na(fossil_type), "", fossil_type)) %>%
      tidyr::separate_rows(fossil_type, sep = ",") %>%
      mutate(
        fossil_type = trimws(fossil_type),
        fossil_type = dplyr::na_if(fossil_type, "")
      ) %>%
      mutate(fossil_type = ifelse(is.na(fossil_type), "Unknown", fossil_type)) %>%
      count(fossil_type, name = "n", sort = TRUE)
    
    if (nrow(tab) == 0) { 
      plot.new()
      title("Fossil types: no data")
      return() 
    }
    
    barplot(tab$n, names.arg = tab$fossil_type, las = 1, cex.names = 0.8,
            main = "Fossil types (collections)", ylab = "Count")
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
    if (!isTRUE(input$sync_refs_with_occ)) return(refs)
    keys <- union(ref_keys_from_occ(), ref_keys_from_col())
    if (length(keys) == 0 || !"reference_key" %in% names(refs)) return(refs[0, ])
    refs %>% filter(reference_key %in% keys)
  })
  
  refs_lang_choices <- reactive({
    pool <- refs_pool()
    if (!"language" %in% names(pool)) pool$language <- NA_character_
    us(ifelse(is.na(pool$language) | pool$language == "", "Unknown", pool$language))
  })
  
  observeEvent(list(input$sync_refs_with_occ, occ_filtered(), col_filtered()), {
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
    data.frame(`# Papers` = nrow(refs_filtered()))
  }, bordered = TRUE, striped = TRUE, digits = 0)
  
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
    
    tint <- "#fff7e6"
    
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
    process_pbdb_occurrences(df, keep_only_needed = TRUE)
  })
  
  pbdb_occ_aligned <- reactive({
    align_to_template(pbdb_occ_processed(), rv$occ)
  })
  
  output$pbdb_occ_preview <- renderDT({
    df <- pbdb_occ_aligned()
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    DT::datatable(head(df, 100), options = list(pageLength = 10, scrollX = TRUE))
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