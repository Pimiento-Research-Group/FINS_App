# ui.R — FINS Explorer: User Interface

ui <- tagList(
  tags$head(
    tags$style(HTML("
      /* Main navbar styling */
      .navbar-default {
        background-color: #4a6b6b !important;  /* Dark teal from logo */
        border-color: #3d5a5a !important;
      }
      
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: #ffffff !important;
        font-weight: 500;
      }
      
      .navbar-default .navbar-nav > li > a:hover,
      .navbar-default .navbar-nav > li > a:focus {
        background-color: #5a7d7d !important;
        color: #ffffff !important;
      }
      
      /* Active tab */
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        background-color: #8fb3a3 !important;  /* Medium sage */
        color: #ffffff !important;
      }
      
      /* Active 'Add data' tab - looks like other active tabs */
      .navbar-default .navbar-nav > li.active > a[data-value='Add data'],
      .navbar-default .navbar-nav > li.active > a[data-value='Add data']:hover,
      .navbar-default .navbar-nav > li.active > a[data-value='Add data']:focus {
        background-color: #8fb3a3 !important;
        color: #ffffff !important;
        border-radius: 0 !important;
        margin: 0 !important;
      }
      
      /* Sidebar panels */
      .well {
        background-color: #f5f9f7 !important;
        border: 1px solid #b8d4c8 !important;
      }
      
      /* Headers */
      h2, h3, h4, h5 {
        color: #4a6b6b !important;
      }
      
      /* Primary buttons */
      .btn-primary {
        background-color: #4a6b6b !important;
        border-color: #3d5a5a !important;
      }
      
      .btn-primary:hover {
        background-color: #5a7d7d !important;
        border-color: #4a6b6b !important;
      }
      
      /* Info boxes - update the existing light blue ones */
      div[style*='background-color: #e3f2fd'] {
        background-color: #d4e8df !important;
        border-left: 4px solid #8fb3a3 !important;
      }
      
      /* Success/highlight boxes */
      div[style*='background-color: #e8f5e9'] {
        background-color: #d4e8df !important;
        border-left: 3px solid #8fb3a3 !important;
      }
      
      /* Action links */
      a {
        color: #4a6b6b !important;
      }
      
      a:hover {
        color: #5a7d7d !important;
      }
      
      /* Download buttons */
      .btn-default {
        background-color: #8fb3a3 !important;
        color: #ffffff !important;
        border-color: #7a9d8d !important;
      }
      
      .btn-default:hover {
        background-color: #7a9d8d !important;
        border-color: #6a8d7d !important;
      }
      
      /* Collapsible panels */
      .panel-default > .panel-heading {
        background-color: #b8d4c8 !important;
        color: #4a6b6b !important;
        border-color: #a3c4b8 !important;
      }
      
      /* Tables */
      .table-striped > tbody > tr:nth-of-type(odd) {
        background-color: #f5f9f7 !important;
      }
      
      /* Leaflet map controls */
      .leaflet-control-layers {
        background-color: #f5f9f7 !important;
      }
    "))
  ),
  
  navbarPage(
    title = div(
      style = "display: flex; align-items: center; height: 100%;",
      img(src = "branding/logo.png", height = "40px", style = "margin-right: 10px;"),
      span("FINS app")
    ),
    id = "main_tabs",
    windowTitle = "FINS app",
    
    # ---- WELCOME / README ----
    tabPanel("Home",
             fluidPage(
               h2("FINS - FossIl NeoselachianS - app"),
               p("This app allows users to explore and add data to the FINS dataset, which consists of fossil occurrences, each linked to a collection and reference."),
               tags$hr(),
               fluidRow(
                 column(4, wellPanel(
                   h4("About"),
                   uiOutput("readme_ui_about")
                 )),
                 column(4, wellPanel(
                   h4("Guide to adding new data"),
                   uiOutput("readme_ui_guide")
                 )),
                 column(4, wellPanel(
                   h4("How to cite & Contact"),
                   tags$div(
                     h5("How to cite FINS?"),
                     p("Kocakova et al. FINS - A global occurrence dataset of fossil sharks, rays and skates from the Cretaceous to the Quaternary"),
                     tags$hr(),
                     h5("Contact us"),
                     p("If you would like to report an error or provide a suggestion on how to improve our platform please contact us via the email below."),
                     p("*Corresponding author: ", tags$a(href = "mailto:kristina.kocakova@pim.uzh.ch", "kristina.kocakova@pim.uzh.ch"))
                   )
                 ))
               )
             )
    ),
    
    # ---- OCCURRENCES ----
    tabPanel("Occurrences",
             sidebarLayout(
               sidebarPanel(
                 h3(style = "margin-top: 0; color: #2c3e50;", "Filters"),
                 div(
                   style = "background-color: #d4e8df; padding: 10px; margin-bottom: 15px; border-radius: 5px; border-left: 4px solid #8fb3a3;",
                   tags$strong("ℹ️ Filter behavior:"),
                   tags$p(style = "margin: 5px 0 0 0; font-size: 12px;",
                          "By default, all data is shown. Use filters below to narrow down the dataset. Empty filters = no restriction.")
                 ),
                 
                 # Collapsible panels
                 bsCollapse(id = "collapse_occ", open = "panel_time_occ", multiple = TRUE,
                            
                            # TIME PANEL
                            bsCollapsePanel("Time", 
                                            style = "default",
                                            value = "panel_time_occ",
                                            
                                            selectizeInput("epochs_occ",  "Epochs",  choices = epoch_choices_occ,  multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_epochs_occ", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_epochs_occ", "Clear All", style = "font-size: 11px;")),
                                            helpText("Uses columns: early_epoch & late_epoch"),
                                            
                                            selectizeInput("periods_occ", "Periods", choices = period_choices_occ, multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_periods_occ", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_periods_occ", "Clear All", style = "font-size: 11px;")),
                                            helpText("Uses columns: early_period & late_period"),
                                            
                                            selectizeInput(
                                              "age_thresh_occ", "Max age-range (Myr):",
                                              choices = c("Any" = "any", "5", "15", "20"),
                                              selected = "any",
                                              options = list(create = TRUE, createOnBlur = TRUE, persist = TRUE, placeholder = "Any or type a number")
                                            ),
                                            helpText("Type any number (e.g., 7.5) or pick a preset. Use 'Any' to disable.")
                            ),
                            
                            # GEOGRAPHY PANEL
                            bsCollapsePanel("Geography",
                                            style = "default",
                                            value = "panel_geo_occ",
                                            
                                            selectizeInput("continent_occ", "Continent:",  choices = continent_choices_occ,  multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_continent_occ", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_continent_occ", "Clear All", style = "font-size: 11px;")),
                                            
                                            selectizeInput("paleocean_occ", "Paleoocean:", choices = paleoocean_choices_occ, multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_paleocean_occ", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_paleocean_occ", "Clear All", style = "font-size: 11px;"))
                            ),
                            
                            # TAXA & STATUS PANEL
                            bsCollapsePanel("Taxa & Status",
                                            style = "default",
                                            value = "panel_taxa_occ",
                                            
                                            div(
                                              style = "background-color: #d4e8df; padding: 8px; margin-bottom: 10px; border-radius: 4px; border-left: 3px solid #8fb3a3; font-size: 11px;",
                                              tags$strong("✓ Hierarchical filtering:"),
                                              " Selecting Superorder filters Orders. Selecting Order filters Families. Status filters based on selected taxonomy."
                                            ),
                                            
                                            selectizeInput("superorder_occ", "Superorder:", choices = superorder_choices, multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_superorder_occ", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_superorder_occ", "Clear All", style = "font-size: 11px;")),
                                            
                                            selectizeInput("order_occ",      "Order:",      choices = order_choices,      multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_order_occ", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_order_occ", "Clear All", style = "font-size: 11px;")),
                                            
                                            selectizeInput("family_occ",     "Family:",     choices = family_choices,     multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_family_occ", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_family_occ", "Clear All", style = "font-size: 11px;")),
                                            
                                            selectizeInput("rank_occ",       "Rank:",       choices = rank_choices,       multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_rank_occ", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_rank_occ", "Clear All", style = "font-size: 11px;")),
                                            
                                            selectizeInput("status_occ",     "Status:",     choices = status_choices,     multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_status_occ", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_status_occ", "Clear All", style = "font-size: 11px;"))
                            ),
                            
                            # CURATION & SOURCE PANEL
                            bsCollapsePanel("Curation & Source",
                                            style = "default",
                                            value = "panel_curation_occ",
                                            
                                            radioButtons("name_mode_occ", "Name display:",
                                                         choices = c("Curated (accepted_name)"="cur","As provided (identified_name)"="raw"),
                                                         selected = "cur"),
                                            checkboxGroupInput("val_filters_occ", "Valid only:",
                                                               choices = c("Age"="age","Taxonomy"="tax","Evidence"="ev"), inline = TRUE),
                                            checkboxGroupInput("source_filter_occ", "Sources:",
                                                               choices = source_levels_occ, selected = source_levels_occ, inline = TRUE),
                                            checkboxGroupInput(
                                              "uncertain_mode_occ", "Uncertain nomenclature:",
                                              choices = c("Flag", "Exclude"),
                                              selected = character(0), inline = TRUE
                                            ),
                                            helpText('Uncertain terms detected in "identified_name": aff., cf., ?, "'),
                                            
                                            checkboxInput("collapse_dups_occ", "Collapse duplicate taxa (by name)", FALSE)
                            )
                 ),
                 
                 # Download button outside the collapse
                 tags$div(style = "margin-top: 15px;",
                          downloadButton("download_occ", "Download current Occurrences", style = "width: 100%;")
                 )
               ),
               mainPanel(
                 fluidRow(
                   column(6, wellPanel(
                     style = "padding: 10px; margin-bottom: 10px;",
                     h5(style = "margin-top: 0;", "Counts by source"),
                     tableOutput("source_counts_occ")
                   )),
                   column(6, wellPanel(
                     style = "padding: 10px; margin-bottom: 10px;",
                     h5(style = "margin-top: 0;", "Curation snapshot"),
                     htmlOutput("curation_snapshot_occ")
                   ))
                 ),
                 leafletOutput("map_occ", height = 550),
                 DTOutput("table_occ")
               )  # End of mainPanel
             )  # End of sidebarLayout
    ),  # End of tabPanel("Occurrences")
    
    # ---- COLLECTIONS ----
    tabPanel("Collections",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("sync_col_with_occ", "Sync with current Occurrence filters", FALSE),
                 
                 div(
                   style = "background-color: #d4e8df; padding: 10px; margin-bottom: 15px; margin-top: 10px; border-radius: 5px; border-left: 4px solid #8fb3a3;",
                   tags$strong("ℹ️ Filter behavior:"),
                   tags$p(style = "margin: 5px 0 0 0; font-size: 12px;",
                          "By default, all data is shown. Use filters below to narrow down the dataset. Empty filters = no restriction.")
                 ),
                 
                 # Collapsible panels
                 bsCollapse(id = "collapse_col", open = "panel_time_col", multiple = TRUE,
                            
                            # TIME PANEL
                            bsCollapsePanel("Time",
                                            style = "default",
                                            value = "panel_time_col",
                                            
                                            selectizeInput("epochs_col",  "Epochs",  choices = epoch_choices_col,  multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_epochs_col", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_epochs_col", "Clear All", style = "font-size: 11px;")),
                                            helpText("Uses columns: early_epoch & late_epoch"),
                                            
                                            selectizeInput("periods_col", "Periods", choices = period_choices_col, multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_periods_col", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_periods_col", "Clear All", style = "font-size: 11px;")),
                                            helpText("Uses columns: early_period & late_period"),
                                            
                                            selectizeInput(
                                              "age_thresh_col", "Max age-range (Myr):",
                                              choices = c("Any" = "any", "5", "15", "20"),
                                              selected = "any",
                                              options = list(create = TRUE, createOnBlur = TRUE, persist = TRUE, placeholder = "Any or type a number")
                                            ),
                                            helpText("Type any number (e.g., 10) or pick a preset. Use 'Any' to disable.")
                            ),
                            
                            # GEOGRAPHY PANEL
                            bsCollapsePanel("Geography",
                                            style = "default",
                                            value = "panel_geo_col",
                                            
                                            selectizeInput("continent_col", "Continent:",  choices = continent_choices_col,  multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_continent_col", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_continent_col", "Clear All", style = "font-size: 11px;")),
                                            
                                            selectizeInput("country_col", "Country:",  choices = country_choices_col,  multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_country_col", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_country_col", "Clear All", style = "font-size: 11px;")),
                                            
                                            selectizeInput("paleocean_col", "Paleoocean:", choices = paleoocean_choices_col, multiple = TRUE),
                                            div(style = "margin-top: -10px; margin-bottom: 10px;",
                                                actionLink("select_all_paleocean_col", "Select All", style = "font-size: 11px; margin-right: 10px;"),
                                                actionLink("clear_all_paleocean_col", "Clear All", style = "font-size: 11px;"))
                            ),
                            
                            # SOURCE PANEL
                            bsCollapsePanel("Source",
                                            style = "default",
                                            value = "panel_source_col",
                                            
                                            checkboxGroupInput("source_filter_col", "Collection source:",
                                                               choices = source_levels_col_all, selected = source_levels_col_all, inline = TRUE)
                            )
                 ),
                 
                 # Download button outside the collapse
                 tags$div(style = "margin-top: 15px;",
                          downloadButton("download_col", "Download current Collections", style = "width: 100%;")
                 )
               ),
               mainPanel(
                 wellPanel(
                   style = "padding: 10px; margin-bottom: 10px;",
                   h5(style = "margin-top: 0;", "Counts by source"),
                   tableOutput("source_counts_col")
                 ),
                 leafletOutput("map_col", height = 430),
                 plotOutput("plot_types_col", height = 260),
                 DTOutput("table_col")
               )
             )
    ),
    
    # ---- REFERENCES ----
    tabPanel("References",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("sync_refs_mode", "Sync references with:",
                              choices = c(
                                "Occurrences filters only" = "occ",
                                "Collections filters only" = "col",
                                "Both filters (union)" = "both",
                                "No filters (all references)" = "none"
                              ),
                              selected = "both"),
                 helpText("Choose which filters determine which references are shown."),
                 selectizeInput("ref_langs", "Language:", choices = NULL, multiple = TRUE),
                 downloadButton("download_refs", "Download current References")
               ),
               mainPanel(
                 fluidRow(
                   column(4, wellPanel(h5("Papers in current selection"), tableOutput("refs_summary"))),
                   column(8)
                 ),
                 DTOutput("refs_table")
               )
             )
    ),
    
    # ---- ADD DATA ----
    tabPanel("Add data",
             sidebarLayout(
               sidebarPanel(
                 h4("Upload PBDB Collections"),
                 fileInput("pbdb_col_file", "Choose PBDB Collections CSV",
                           accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                 checkboxGroupInput(
                   "pbdb_col_paleo",
                   "Paleocoordinate and paleoocean addition",
                   choices = c(
                     "Compute paleocoordinates (30 replicates)" = "compute",
                     "Assign paleooceans (Compute paleocoordinates first!)" = "ocean"
                   ),
                   selected = NULL
                 ),
                 radioButtons("pbdb_col_mode", "Apply to Collections:",
                              choices = c("Preview only" = "preview", "Append to current" = "append", "Replace current" = "replace"),
                              selected = "preview"),
                 actionButton("pbdb_col_apply", "Apply Collections", class = "btn-primary"),
                 tags$hr(),
                 
                 h4("Upload PBDB Occurrences"),
                 fileInput("pbdb_occ_file", "Choose PBDB Occurrences CSV",
                           accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                 radioButtons("pbdb_occ_mode", "Apply to Occurrences:",
                              choices = c("Preview only" = "preview", "Append to current" = "append", "Replace current" = "replace"),
                              selected = "preview"),
                 actionButton("pbdb_occ_apply", "Apply Occurrences", class = "btn-primary")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Collections preview",
                            helpText("Showing the uploaded PBDB Collections aligned to the current FINS Collections schema (missing columns added empty)."),
                            DTOutput("pbdb_col_preview"),
                            uiOutput("pbdb_col_status")
                   ),
                   tabPanel("Occurrences preview",
                            helpText("Showing the uploaded PBDB Occurrences aligned to the current FINS Occurrences schema (missing columns added empty)."),
                            DTOutput("pbdb_occ_preview"),
                            uiOutput("pbdb_occ_status")
                   )
                 )
               )
             )
    )
  )
)

