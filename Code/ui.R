# ui.R — FINS Explorer: User Interface

ui <- tagList(
  tags$head(
    tags$style(HTML("
      /* Light-blue 'Add data' tab */
      .navbar-nav > li > a[data-value='Add data']{
        background-color:#e6f4ff !important;  /* light blue */
        color:#0b5cab !important;              /* deep blue text */
        border-radius:6px;
        margin: 0 6px;
        font-weight: 600;
      }
      .navbar-nav > li > a[data-value='Add data']:hover{
        background-color:#d7ecff !important;
        color:#0b5cab !important;
      }
      /* Active state (when the tab is selected) */
      .navbar-nav > li.active > a[data-value='Add data'],
      .navbar-nav > li.active > a[data-value='Add data']:focus,
      .navbar-nav > li.active > a[data-value='Add data']:hover{
        background-color:#cfe8ff !important;
        color:#073b7a !important;
      }
    "))
  ),
  
  navbarPage(
    "FINS app",
    id = "main_tabs",
    
    # ---- WELCOME / README ----
    tabPanel("Welcome",
             fluidPage(
               h2("FINS - FossIl NeoselachianS - app"),
               p("This app allows users to explore and add data to the FINS dataset, which consists of fossil occurrences, each linked to a collection and reference."),
               tags$hr(),
               fluidRow(
                 column(6, wellPanel(
                   h4("About"),
                   uiOutput("readme_ui_about")
                 )),
                 column(6, wellPanel(
                   h4("Guide to adding new data"),
                   uiOutput("readme_ui_guide")
                 ))
               )
             )
    ),
    
    # ---- OCCURRENCES ----
    tabPanel("Occurrences",
             sidebarLayout(
               sidebarPanel(
                 h4("Time"),
                 selectizeInput("epochs_occ",  "Epochs",  choices = epoch_choices_occ,  multiple = TRUE),
                 helpText("Uses columns: early_epoch & late_epoch"),
                 selectizeInput("periods_occ", "Periods", choices = period_choices_occ, multiple = TRUE),
                 helpText("Uses columns: early_period & late_period"),
                 selectizeInput(
                   "age_thresh_occ", "Max age-range (Myr):",
                   choices = c("Any" = "any", "5", "15", "20"),
                   selected = "any",
                   options = list(create = TRUE, createOnBlur = TRUE, persist = TRUE, placeholder = "Any or type a number")
                 ),
                 helpText("Type any number (e.g., 7.5) or pick a preset. Use 'Any' to disable."),
                 
                 h4("Geography"),
                 selectizeInput("continent_occ", "Continent:",  choices = continent_choices_occ,  multiple = TRUE),
                 selectizeInput("paleocean_occ", "Paleoocean:", choices = paleoocean_choices_occ, multiple = TRUE),
                 
                 h4("Taxa & Status"),
                 selectizeInput("order_occ",      "Order:",      choices = order_choices,      multiple = TRUE),
                 selectizeInput("superorder_occ", "Superorder:", choices = superorder_choices, multiple = TRUE),
                 selectizeInput("family_occ",     "Family:",     choices = family_choices,     multiple = TRUE),
                 selectizeInput("rank_occ",       "Rank:",       choices = rank_choices,       multiple = TRUE),
                 selectizeInput("status_occ",     "Status:",     choices = status_choices,     multiple = TRUE),
                 
                 h4("Curation & Source"),
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
                 
                 checkboxInput("collapse_dups_occ", "Collapse duplicate taxa (by name)", FALSE),
                 downloadButton("download_occ", "Download current Occurrences")
               ),
               mainPanel(
                 fluidRow(
                   column(4, wellPanel(h5("Counts by source"), tableOutput("source_counts_occ"))),
                   column(8, wellPanel(h5("Curation snapshot"), htmlOutput("curation_snapshot_occ")))
                 ),
                 leafletOutput("map_occ", height = 430),
                 DTOutput("table_occ")
               )
             )
    ),
    
    # ---- COLLECTIONS ----
    tabPanel("Collections",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("sync_col_with_occ", "Sync with current Occurrence filters", TRUE),
                 
                 h4("Time"),
                 selectizeInput("epochs_col",  "Epochs",  choices = epoch_choices_col,  multiple = TRUE),
                 helpText("Uses columns: early_epoch & late_epoch"),
                 selectizeInput("periods_col", "Periods", choices = period_choices_col, multiple = TRUE),
                 helpText("Uses columns: early_period & late_period"),
                 selectizeInput(
                   "age_thresh_col", "Max age-range (Myr):",
                   choices = c("Any" = "any", "5", "15", "20"),
                   selected = "any",
                   options = list(create = TRUE, createOnBlur = TRUE, persist = TRUE, placeholder = "Any or type a number")
                 ),
                 helpText("Type any number (e.g., 10) or pick a preset. Use 'Any' to disable."),
                 
                 h4("Geography"),
                 selectizeInput("continent_col", "Continent:",  choices = continent_choices_col,  multiple = TRUE),
                 selectizeInput("paleocean_col", "Paleoocean:", choices = paleoocean_choices_col, multiple = TRUE),
                 
                 h4("Source"),
                 checkboxGroupInput("source_filter_col", "Collection source:",
                                    choices = source_levels_col_all, selected = source_levels_col_all, inline = TRUE),
                 
                 downloadButton("download_col", "Download current Collections")
               ),
               mainPanel(
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
                 checkboxInput("sync_refs_with_occ", "Sync with current Occurrence filters", TRUE),
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

