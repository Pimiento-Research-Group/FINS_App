# global.R — FINS Explorer: Global variables, helpers, and data loading

# ---------- Libraries ----------
library(shiny)
library(dplyr)
library(readr)
library(DT)
library(leaflet)
library(markdown)
library(tidyr)
library(ggplot2)
library(rgplates)
library(geojsonsf)
library(digest)
library(here)
library(shinyBS)
library(forcats)
library(readxl)

# Make branding folder available for logos
shiny::addResourcePath("branding", here::here("branding"))

# ---------- Options ----------
options(shiny.maxRequestSize = 50 * 1024^2)  # allow up to 50MB uploads

# ---------- Helper functions ----------
`%||%` <- function(a, b) ifelse(is.na(a), b, a)
us <- function(x) sort(unique(na.omit(x)))
drop_dot_cols <- function(df) df %>% select(-matches("^\\.\\.\\d+$"), everything())
has_col <- function(df, nm) nm %in% names(df)

# Ensure dataframe has specified columns (add as NA if missing)
ensure_cols <- function(df, col_names) {
  for (nm in col_names) {
    if (!nm %in% names(df)) {
      df[[nm]] <- NA_character_
    }
  }
  df
}

# Coalesce across multiple column names (returns first non-NA column)
coalesce_col <- function(df, col_names) {
  for (nm in col_names) {
    if (nm %in% names(df)) {
      vals <- df[[nm]]
      if (any(!is.na(vals))) return(vals)
    }
  }
  rep(NA, nrow(df))
}

# Epoch/Period filters: keep row if EITHER early_* OR late_* matches a selected value
overlaps_time <- function(df, epochs_sel, periods_sel) {
  out <- df
  if (length(epochs_sel))  out <- out %>% filter((early_epoch %in% epochs_sel)  | (late_epoch %in% epochs_sel))
  if (length(periods_sel)) out <- out %>% filter((early_period %in% periods_sel) | (late_period %in% periods_sel))
  out
}

apply_geog <- function(df, continents, countries, paleo) {
  out <- df
  if (length(continents)) out <- out %>% filter(continent %in% continents)
  if (length(countries))  out <- out %>% filter(country %in% countries)
  if (length(paleo))      out <- out %>% filter(paleoocean %in% paleo)
  out
}

apply_validation <- function(df, val_flags) {
  out <- df
  if ("age" %in% val_flags && "age_evaluation" %in% names(out)) out <- out %>% filter(age_evaluation == "valid")
  if ("tax" %in% val_flags && "taxonomy_validation" %in% names(out)) out <- out %>% filter(taxonomy_validation == "valid")
  if ("ev" %in% val_flags && "evidence_validation" %in% names(out)) out <- out %>% filter(evidence_validation == "with_evidence")
  out
}

# Expand ISO2 country codes (e.g., "US") to full names ("United States").
# Uses countrycode if available; otherwise a small built-in fallback map.
expand_country_codes <- function(x) {
  x <- toupper(trimws(as.character(x)))
  out <- rep(NA_character_, length(x))
  
  if (requireNamespace("countrycode", quietly = TRUE)) {
    out <- countrycode::countrycode(x, origin = "iso2c", destination = "country.name", warn = FALSE)
  } else {
    # Minimal fallback; add more as needed
    fallback <- c(
      "US"="United States", "GB"="United Kingdom", "UK"="United Kingdom",
      "DE"="Germany", "FR"="France", "ES"="Spain", "IT"="Italy",
      "PT"="Portugal", "NL"="Netherlands", "BE"="Belgium", "LU"="Luxembourg",
      "CH"="Switzerland", "AT"="Austria", "SE"="Sweden", "NO"="Norway",
      "FI"="Finland", "DK"="Denmark", "IS"="Iceland", "IE"="Ireland",
      "PL"="Poland", "CZ"="Czechia", "SK"="Slovakia", "HU"="Hungary",
      "RO"="Romania", "BG"="Bulgaria", "GR"="Greece", "TR"="Türkiye",
      "RU"="Russia", "UA"="Ukraine", "BY"="Belarus", "EE"="Estonia",
      "LV"="Latvia", "LT"="Lithuania",
      "CN"="China", "JP"="Japan", "KR"="South Korea", "IN"="India",
      "ID"="Indonesia", "PH"="Philippines", "VN"="Vietnam", "TH"="Thailand",
      "AU"="Australia", "NZ"="New Zealand",
      "CA"="Canada", "MX"="Mexico", "BR"="Brazil", "AR"="Argentina", "CL"="Chile",
      "ZA"="South Africa", "EG"="Egypt", "MA"="Morocco", "TN"="Tunisia",
      "IR"="Iran", "IQ"="Iraq", "SA"="Saudi Arabia", "AE"="United Arab Emirates"
    )
    out <- unname(fallback[x])
  }
  
  # If we couldn't map a code, keep the original (so nothing is lost)
  ifelse(is.na(out) | out == "", x, out)
}

# Map full country names to continents
country_to_continent <- function(country_vec) {
  cn <- trimws(as.character(country_vec))
  fb <- c(
    # Europe
    "United Kingdom"="Europe","Ireland"="Europe","France"="Europe","Spain"="Europe","Portugal"="Europe",
    "Germany"="Europe","Switzerland"="Europe","Austria"="Europe","Italy"="Europe","Netherlands"="Europe",
    "Belgium"="Europe","Luxembourg"="Europe","Denmark"="Europe","Sweden"="Europe","Norway"="Europe",
    "Finland"="Europe","Poland"="Europe","Czechia"="Europe","Czech Republic"="Europe","Slovakia"="Europe",
    "Hungary"="Europe","Romania"="Europe","Bulgaria"="Europe","Greece"="Europe","Turkey"="Europe",
    "Türkiye"="Europe","Russia"="Europe","Ukraine"="Europe",
    # Asia
    "China"="Asia","Japan"="Asia","South Korea"="Asia","North Korea"="Asia","India"="Asia","Indonesia"="Asia",
    "Philippines"="Asia","Vietnam"="Asia","Thailand"="Asia","Iran"="Asia","Iraq"="Asia","Saudi Arabia"="Asia",
    "United Arab Emirates"="Asia","Israel"="Asia","Jordan"="Asia",
    # North America
    "United States"="North America","Canada"="North America","Mexico"="North America",
    "Guatemala"="North America","Belize"="North America","Honduras"="North America","El Salvador"="North America",
    "Nicaragua"="North America","Costa Rica"="North America","Panama"="North America",
    "Bahamas"="North America","Cuba"="North America","Jamaica"="North America","Haiti"="North America",
    "Dominican Republic"="North America","Trinidad and Tobago"="North America",
    # South America
    "Brazil"="South America","Argentina"="South America","Chile"="South America","Peru"="South America",
    "Colombia"="South America","Uruguay"="South America","Paraguay"="South America","Bolivia"="South America",
    "Ecuador"="South America","Venezuela"="South America","Guyana"="South America","Suriname"="South America",
    # Africa
    "South Africa"="Africa","Egypt"="Africa","Morocco"="Africa","Tunisia"="Africa","Kenya"="Africa",
    "Tanzania"="Africa","Ethiopia"="Africa","Namibia"="Africa","Algeria"="Africa",
    # Oceania
    "Australia"="Oceania","New Zealand"="Oceania"
  )
  unname(ifelse(cn %in% names(fb), fb[cn], NA_character_))
}

# ---------- Timescale lookup ----------
.timescale_lookup <- local({
  row <- function(label, epoch = NA, period = NA, era = NA) {
    tibble::tibble(label = label, epoch = epoch, period = period, era = era)
  }
  
  # ---- ICS stages/epochs/periods (Cretaceous + Cenozoic) ----
  cret_early <- c("Berriasian","Valanginian","Hauterivian","Barremian","Aptian","Albian")
  cret_late  <- c("Cenomanian","Turonian","Coniacian","Santonian","Campanian","Maastrichtian")
  paleocene  <- c("Danian","Selandian","Thanetian")
  eocene     <- c("Ypresian","Lutetian","Bartonian","Priabonian")
  oligocene  <- c("Rupelian","Chattian")
  miocene    <- c("Aquitanian","Burdigalian","Langhian","Serravallian","Tortonian","Messinian")
  pliocene   <- c("Zanclean","Piacenzian")
  pleist     <- c("Gelasian","Calabrian","Chibanian","Late Pleistocene", "Upper Pleistocene")
  holocene   <- c("Greenlandian","Northgrippian","Meghalayan")
  
  st_rows <- dplyr::bind_rows(
    tibble::tibble(label = cret_early, epoch = "Early Cretaceous", period = "Cretaceous", era = "Mesozoic"),
    tibble::tibble(label = cret_late,  epoch = "Late Cretaceous",  period = "Cretaceous", era = "Mesozoic"),
    tibble::tibble(label = paleocene, epoch = "Paleocene", period = "Paleogene", era = "Cenozoic"),
    tibble::tibble(label = eocene,    epoch = "Eocene",    period = "Paleogene", era = "Cenozoic"),
    tibble::tibble(label = oligocene, epoch = "Oligocene", period = "Paleogene", era = "Cenozoic"),
    tibble::tibble(label = miocene,   epoch = "Miocene",   period = "Neogene",   era = "Cenozoic"),
    tibble::tibble(label = pliocene,  epoch = "Pliocene",  period = "Neogene",   era = "Cenozoic"),
    tibble::tibble(label = pleist,    epoch = "Pleistocene", period = "Quaternary", era = "Cenozoic"),
    tibble::tibble(label = holocene,  epoch = "Holocene",    period = "Quaternary", era = "Cenozoic")
  )
  
  ep_rows <- dplyr::bind_rows(
    row("Early Cretaceous","Early Cretaceous","Cretaceous","Mesozoic"),
    row("Late Cretaceous", "Late Cretaceous", "Cretaceous","Mesozoic"),
    row("Paleocene","Paleocene","Paleogene","Cenozoic"),
    row("Eocene","Eocene","Paleogene","Cenozoic"),
    row("Oligocene","Oligocene","Paleogene","Cenozoic"),
    row("Miocene","Miocene","Neogene","Cenozoic"),
    row("Pliocene","Pliocene","Neogene","Cenozoic"),
    row("Pleistocene","Pleistocene","Quaternary","Cenozoic"),
    row("Holocene","Holocene","Quaternary","Cenozoic")
  )
  
  per_rows <- dplyr::bind_rows(
    row("Cretaceous", period = "Cretaceous", era = "Mesozoic"),
    row("Paleogene",  period = "Paleogene",  era = "Cenozoic"),
    row("Neogene",    period = "Neogene",    era = "Cenozoic"),
    row("Quaternary", period = "Quaternary", era = "Cenozoic")
  )
  
  era_rows <- dplyr::bind_rows(row("Mesozoic", era = "Mesozoic"), row("Cenozoic", era = "Cenozoic"))
  
  syn_rows <- dplyr::bind_rows(
    row("Upper Cretaceous","Late Cretaceous","Cretaceous","Mesozoic"),
    row("Lower Cretaceous","Early Cretaceous","Cretaceous","Mesozoic")
  )
  
  base_ts <- dplyr::bind_rows(st_rows, ep_rows, per_rows, era_rows, syn_rows)
  
  # ---- Regional/biostrat aliases ----
  # New Zealand (GNS timescale)
  nz_low_cret <- c("Korangan", "Urutawan", "Motuan")
  nz_up_cret <- c("Ngaterian", "Arowhanan", "Mangaotanean", "Teratan", "Piripauan", "Haumurian")
  nz_paleo <- c("Teurian")
  nz_eoc <- c("Waipawan", "Mangaorapan", "Heretaungan", "Porangan", "Bortonian", "Kaiatan", "Runangan")
  nz_oligo   <- c("Whaingaroan","Duntroonian","Waitakian")
  nz_miocene <- c("Otaian","Altonian","Clifdenian","Lillburnian","Waiauan","Tongaporutuan","Kapitean")
  nz_plio    <- c("Opoitian","Waipipian","Mangapanian")
  nz_pleist  <- c("Nukumaruan","Castlecliffian")
  nz_holo    <- c("Haweran")
  
  nz_rows <- dplyr::bind_rows(
    tibble::tibble(label = nz_low_cret, epoch = "Lower Cretaceous",   period = "Cretaceous",   era = "Mesozoic"),
    tibble::tibble(label = nz_up_cret, epoch = "Upper Cretaceous",   period = "Cretaceous",   era = "Mesozoic"),
    tibble::tibble(label = nz_paleo, epoch = "Paleocene",   period = "Paleogene",   era = "Cenozoic"),
    tibble::tibble(label = nz_eoc, epoch = "Eocene",   period = "Paleogene",   era = "Cenozoic"),
    tibble::tibble(label = nz_oligo, epoch = "Oligocene",   period = "Paleogene",   era = "Cenozoic"),
    tibble::tibble(label = nz_miocene, epoch = "Miocene",   period = "Neogene",   era = "Cenozoic"),
    tibble::tibble(label = nz_plio,    epoch = "Pliocene",  period = "Neogene",   era = "Cenozoic"),
    tibble::tibble(label = nz_pleist,  epoch = "Pleistocene", period = "Quaternary", era = "Cenozoic"),
    tibble::tibble(label = nz_holo,    epoch = "Holocene",  period = "Quaternary", era = "Cenozoic")
  )
  
  # European Land Mammal Ages — MN (Neogene)
  elma_mn_miocene <- c("MN 1","MN 2","MN 3","MN 4","MN 5","MN 6","MN 7/8","MN 7","MN 8","MN 9","MN 10","MN 11","MN 12","MN 13")
  elma_mn_plio    <- c("MN 14","MN 15","MN 16")
  elma_mn_q       <- c("MN 17")
  mn_rows <- dplyr::bind_rows(
    tibble::tibble(label = elma_mn_miocene, epoch = "Miocene",  period = "Neogene", era = "Cenozoic"),
    tibble::tibble(label = elma_mn_plio,    epoch = "Pliocene", period = "Neogene", era = "Cenozoic"),
    tibble::tibble(label = elma_mn_q,    epoch = "Pleistocene", period = "Quaternary", era = "Cenozoic")
  )
  
  # Mammal Paleogene (MP 1–30)
  mp_paleocene <- paste0("MP ", 1:10)
  mp_eocene    <- paste0("MP ", 11:20)
  mp_oligo     <- paste0("MP ", 21:30)
  mp_rows <- dplyr::bind_rows(
    tibble::tibble(label = mp_paleocene, epoch = "Paleocene", period = "Paleogene", era = "Cenozoic"),
    tibble::tibble(label = mp_eocene,    epoch = "Eocene",    period = "Paleogene", era = "Cenozoic"),
    tibble::tibble(label = mp_oligo,     epoch = "Oligocene", period = "Paleogene", era = "Cenozoic")
  )
  
  # North American Land Mammal Ages (NALMA)
  nalma_up_cret <- c("Aquilian", "Wahweapian", "Judithian", "Kirtlandian", "Edmontonian", "Lancian")
  nalma_paleoc <- c("Puercan","Torrejonian","Tiffanian","Clarkforkian")
  nalma_eocene <- c("Wasatchian","Bridgerian","Uintan","Duchesnean")
  nalma_oligo  <- c("Chadronian","Orellan","Whitneyan","Arikareean")
  nalma_mioc   <- c("Hemingfordian","Barstovian","Clarendonian","Hemphillian")
  nalma_plio  <- c("Blancan")
  nalma_pleisto <- c("Irvingtonian","Rancholabrean")
  nalma_holo <- c("Santarosean", "Saintaugustinean")
  nalma_rows <- dplyr::bind_rows(
    tibble::tibble(label = nalma_up_cret, epoch = "Upper Cretaceous",      period = "Cretaceous",  era = "Mesozoic"),
    tibble::tibble(label = nalma_paleoc, epoch = "Paleocene",   period = "Paleogene",  era = "Cenozoic"),
    tibble::tibble(label = nalma_eocene, epoch = "Eocene",      period = "Paleogene",  era = "Cenozoic"),
    tibble::tibble(label = nalma_oligo,  epoch = "Oligocene",   period = "Paleogene",  era = "Cenozoic"),
    tibble::tibble(label = nalma_mioc,   epoch = "Miocene",     period = "Neogene",    era = "Cenozoic"),
    tibble::tibble(label = nalma_plio, epoch = "Pliocene",    period = "Neogene",    era = "Cenozoic"),
    tibble::tibble(label = nalma_pleisto, epoch = "Pleistocene",    period = "Quaternary",    era = "Cenozoic"),
    tibble::tibble(label = nalma_holo, epoch = "Holocene",    period = "Quaternary",    era = "Cenozoic")
  )
  
  # Central Paratethys regional stages (Miocene)
  paratethys <- c("Egerian","Eggenburgian","Ottnangian","Karpatian","Badenian","Sarmatian","Pannonian","Pontian")
  paratethys_rows <- tibble::tibble(label = paratethys, epoch = "Miocene", period = "Neogene", era = "Cenozoic")
  
  # South American Land Mammal Ages (SALMA)
  salma_paleocene <- c("Tiupampan", "Peligran" ,"Riochican")
  salma_eocene    <- c("Itaboraian, Casamayoran", "Mustersan", "Divisaderan")
  salma_oligo     <- c("Tinguirirican", "Deseadan")
  salma_mioc     <- c("Colhuehuapian","Santacrucian","Friasian","Colloncuran","Laventan","Mayoan","Chasiquian","Huayquerian")
  salma_plio   <- c("Montehermosan","Chapadmalalan")
  salma_pleisto  <- c("Uquian","Ensenadan","Lujanian")
  salma_rows <- dplyr::bind_rows(
    tibble::tibble(label = salma_paleocene, epoch = "Paleocene",  period = "Paleogene", era = "Cenozoic"),
    tibble::tibble(label = salma_eocene,    epoch = "Eocene",     period = "Paleogene", era = "Cenozoic"),
    tibble::tibble(label = salma_oligo,     epoch = "Oligocene",  period = "Paleogene", era = "Cenozoic"),
    tibble::tibble(label = salma_mioc,      epoch = "Miocene",    period = "Neogene",   era = "Cenozoic"),
    tibble::tibble(label = salma_plio,      epoch = "Pliocene",   period = "Neogene",   era = "Cenozoic"),
    tibble::tibble(label = salma_pleisto,   epoch = "Pleistocene",   period = "Quaternary",   era = "Cenozoic")
  )
  
  # Bind everything and standardize the lookup key
  ts <- dplyr::bind_rows(base_ts, nz_rows, mn_rows, mp_rows, nalma_rows, paratethys_rows, salma_rows) %>%
    dplyr::mutate(label_std = tolower(trimws(label)))
  ts
})

# ---------- Normalizer + hierarchy resolver ----------
.normalize_interval <- function(x) {
  s <- tolower(trimws(as.character(x)))
  s <- sub("^(early|late|middle|mid|upper|lower)(est|most)?\\s+", "", s, perl = TRUE)
  s <- gsub("[,;]+", " ", s, perl = TRUE)
  s <- trimws(gsub("\\s+", " ", s))
  s
}

resolve_interval_hierarchy <- function(x) {
  key <- .normalize_interval(x)
  lk  <- .timescale_lookup
  out <- lk[match(key, lk$label_std), c("epoch","period","era")]
  rownames(out) <- NULL
  out
}

# ---------- Label sets for type classification ----------
.ics_stage_set <- c(
  "Berriasian","Valanginian","Hauterivian","Barremian","Aptian","Albian",
  "Cenomanian","Turonian","Coniacian","Santonian","Campanian","Maastrichtian",
  "Danian","Selandian","Thanetian","Ypresian","Lutetian","Bartonian","Priabonian","Rupelian","Chattian",
  "Aquitanian","Burdigalian","Langhian","Serravallian","Tortonian","Messinian","Zanclean","Piacenzian",
  "Gelasian","Calabrian","Chibanian","Late Pleistocene","Greenlandian","Northgrippian","Meghalayan")
.ics_epoch_set  <- c("Early Cretaceous","Late Cretaceous","Paleocene","Eocene","Oligocene","Miocene","Pliocene","Pleistocene","Holocene")
.ics_period_set <- c("Cretaceous","Paleogene","Neogene","Quaternary")
.ics_era_set    <- c("Mesozoic","Cenozoic")
.nz_stage_set <- c(
  "Korangan","Urutawan","Motuan","Ngaterian","Arowhanan","Mangaotanean","Teratan","Piripauan","Haumurian",
  "Teurian","Waipawan","Mangaorapan","Heretaungan","Porangan","Bortonian","Kaiatan","Runangan",
  "Whaingaroan","Duntroonian","Waitakian","Otaian","Altonian","Clifdenian","Lillburnian","Waiauan",
  "Tongaporutuan","Kapitean","Opoitian","Waipipian","Mangapanian","Nukumaruan","Castlecliffian","Haweran")
.paratethys_stage_set <- c("Egerian","Eggenburgian","Ottnangian","Karpatian","Badenian","Sarmatian","Pannonian","Pontian")
.nalma_set <- c(
  "Aquilian","Wahweapian","Judithian","Kirtlandian","Edmontonian","Lancian","Puercan","Torrejonian","Tiffanian","Clarkforkian",
  "Wasatchian","Bridgerian","Uintan","Duchesnean","Chadronian","Orellan","Whitneyan","Arikareean",
  "Hemingfordian","Barstovian","Clarendonian","Hemphillian","Blancan","Irvingtonian","Rancholabrean",
  "Santarosean","Saintaugustinean")
.salma_set <- c(
  "Tiupampan","Peligran","Riochican",
  "Itaboraian","Casamayoran",
  "Mustersan","Divisaderan","Tinguirirican","Deseadan",
  "Colhuehuapian","Santacrucian","Friasian","Colloncuran","Laventan","Mayoan","Chasiquan","Huayquerian",
  "Montehermosan","Chapadmalalan","Uquian","Ensenadan","Lujanian")
.mn_set <- c(paste0("MN ", 1:17), "MN 7/8")
.mp_set <- paste0("MP ", 1:30)

.has_sub_prefix <- function(x) {
  grepl("^(early|late|middle|mid|upper|lower)(est|most)?\\b", tolower(trimws(as.character(x))), perl = TRUE)
}

classify_interval_type <- function(label) {
  if (is.null(label) || is.na(label) || trimws(label) == "") return(NA_character_)
  raw <- as.character(label)
  key <- .normalize_interval(raw)
  in_ci <- function(val, set) tolower(val) %in% tolower(set)
  
  type <- dplyr::case_when(
    in_ci(key, .ics_stage_set)        ~ "stage",
    in_ci(key, .ics_epoch_set)        ~ "epoch",
    in_ci(key, .ics_period_set)       ~ "period",
    in_ci(key, .ics_era_set)          ~ "era",
    in_ci(key, .nalma_set)            ~ "NALMA",
    in_ci(key, .salma_set)            ~ "SALMA",
    in_ci(key, .mn_set)               ~ "ELMA",
    in_ci(key, .mp_set)               ~ "MP Zone",
    in_ci(key, .paratethys_stage_set) ~ "Regional stage",
    in_ci(key, .nz_stage_set)         ~ "NZ stage",
    TRUE                              ~ NA_character_
  )
  
  if (!is.na(type) && .has_sub_prefix(raw)) paste0("sub-", type) else type
}

# ---- Paleocoordinate reconstruction helpers ----
.recon_models <- c("MATTHEWS2016_mantle_ref", "PALEOMAP", "SETON2012", "MULLER2019")

# Create a cache key from the uploaded Collections file
.make_paleo_key <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Create a hash of the relevant columns to use as cache key
  # Use collection IDs, coordinates, and ages
  key_cols <- c("collection_number", "collection_no", "lng", "lat", 
                "longitude", "latitude", "max_ma", "min_ma")
  
  # Get columns that exist
  existing_cols <- intersect(tolower(names(df)), key_cols)
  
  if (length(existing_cols) == 0) return(NULL)
  
  # Create a string representation and hash it
  key_data <- df[, existing_cols, drop = FALSE]
  key_string <- paste(capture.output(str(key_data)), collapse = "")
  digest::digest(key_string, algo = "md5")
}

.reconstruct_one <- function(lon, lat, age_ma, models = .recon_models) {
  # Validate inputs first
  if (is.na(lon) || is.na(lat))  return(list(ok = FALSE, xy = c(NA_real_, NA_real_), model = NA, reason = "Missing lon/lat"))
  if (is.na(age_ma))             return(list(ok = FALSE, xy = c(NA_real_, NA_real_), model = NA, reason = "Missing age"))
  if (!requireNamespace("rgplates", quietly = TRUE)) {
    return(list(ok = FALSE, xy = c(NA_real_, NA_real_), model = NA, reason = "Package 'rgplates' not available"))
  }
  # Try each model in order; record first success or the last error
  last_err <- NULL
  for (m in models) {
    out <- tryCatch(
      {
        rgplates::reconstruct(c(lon, lat), age = age_ma, enumerate = FALSE, model = m)
      },
      error = function(e) e,
      warning = function(w) w
    )
    if (is.matrix(out) && ncol(out) >= 2 && is.finite(out[1,1]) && is.finite(out[1,2])) {
      return(list(ok = TRUE, xy = c(as.numeric(out[1,1]), as.numeric(out[1,2])), model = m, reason = "OK"))
    } else {
      # store a brief reason
      msg <- if (inherits(out, "error") || inherits(out, "warning")) conditionMessage(out) else "Unknown failure"
      last_err <- paste0(m, ": ", msg)
    }
  }
  list(ok = FALSE, xy = c(NA_real_, NA_real_), model = NA, reason = last_err %||% "All models failed")
}

.paleo_mc_median <- function(lon, lat, min_ma, max_ma, n = 30, models = .recon_models) {
  if (is.na(min_ma) && is.na(max_ma))  return(list(ok = FALSE, xy = c(NA_real_, NA_real_), n_ok = 0, reason = "No min/max age", model_used = NA_character_))
  if (is.na(min_ma)) min_ma <- max_ma
  if (is.na(max_ma)) max_ma <- min_ma
  a_min <- as.numeric(min(min_ma, max_ma))
  a_max <- as.numeric(max(min_ma, max_ma))
  if (!is.finite(a_min) || !is.finite(a_max)) return(list(ok = FALSE, xy = c(NA_real_, NA_real_), n_ok = 0, reason = "Non-finite age range", model_used = NA_character_))
  ages <- if (a_min == a_max) rep(a_min, n) else stats::runif(n, min = a_min, max = a_max)
  
  plons <- numeric(0); plats <- numeric(0)
  models_used <- character(0)
  n_ok  <- 0L
  last_reason <- NULL
  
  for (ag in ages) {
    res <- .reconstruct_one(lon, lat, ag, models = models)
    if (isTRUE(res$ok)) {
      plons <- c(plons, res$xy[1]); plats <- c(plats, res$xy[2])
      models_used <- c(models_used, res$model %||% NA_character_)
      n_ok <- n_ok + 1L
    } else {
      last_reason <- res$reason
    }
  }
  
  if (n_ok == 0) {
    return(list(ok = FALSE, xy = c(NA_real_, NA_real_), n_ok = 0, reason = last_reason %||% "All replicates failed", model_used = NA_character_))
  }
  
  # pick the most frequent successful model (ignoring NA)
  model_used <- {
    mu <- na.omit(models_used)
    if (length(mu)) names(sort(table(mu), decreasing = TRUE))[1] else NA_character_
  }
  
  list(
    ok = TRUE,
    xy = c(stats::median(plons, na.rm = TRUE), stats::median(plats, na.rm = TRUE)),
    n_ok = n_ok,
    reason = sprintf("OK (%d/%d replicates)", n_ok, n),
    model_used = model_used
  )
}

# ---------- Assign Paleooceans helpers ------------
# most-frequent non-NA value
.mode1 <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# Age (Ma) -> epoch bucket for ocean rules
.epoch_from_age_ma <- function(age) {
  if (is.na(age)) return(NA_character_)
  if (age <= 0.0117) return("Holocene")
  if (age <= 2.58)   return("Pleistocene")
  if (age <= 5.333)  return("Pliocene")
  if (age <= 23.03)  return("Miocene")
  if (age <= 33.9)   return("Oligocene")
  if (age <= 56)     return("Eocene")
  if (age <= 66)     return("Paleocene")
  if (age <= 100.5)  return("Upper Cretaceous")
  if (age <= 145)    return("Lower Cretaceous")
  if (age <= 161.5)  return("Upper Jurassic")
  if (age <= 174.1)  return("Middle Jurassic")
  if (age <= 201.3)  return("Lower Jurassic")
  NA_character_
}

.classify_ocean <- function(epoch_label, lon, lat) {
  # Guard
  if (is.na(lon) || is.na(lat)) return(NA_character_)
  e <- epoch_label %||% NA_character_
  if (is.na(e)) return(NA_character_)
  
  # Helpers
  between <- function(x, a, b) is.finite(x) && x > a && x < b
  
  # Jurassic
  if (e %in% c("Lower Jurassic","Middle Jurassic","Upper Jurassic")) {
    if (e == "Lower Jurassic") {
      if (between(lon, 0, 120) && between(lat, -40, 45)) return("Tethys")
      else return("Pacific")
    }
    if (e == "Middle Jurassic") {
      if (between(lon, 0, 120) && between(lat, -45, 45)) return("Tethys")
      else return("Pacific")
    }
    if (e == "Upper Jurassic") {
      if (between(lon, 22, 130) && between(lat, -40, 30)) return("Tethys")
      else return("Pacific")
    }
  }
  
  # Lower Cretaceous
  if (e == "Lower Cretaceous") {
    if (between(lon, -70, -30) && between(lat, 30, 70)) return("Western Interior Seaway")
    if (between(lon, 30, 120)  && between(lat, -60, 30)) return("Tethys")
    if (between(lon, -50, 0)   && between(lat, 0, 40))   return("Western Tethys")
    if (between(lon, 0, 30)    && between(lat, 0, 60))   return("Tethys Seaway")
    if (between(lon, 30, 60)   && between(lat, 30, 60))  return("Tethys Seaway")
    if (between(lon, -30, 0)   && between(lat, -60, 0))  return("Atlantic")
    if (between(lon, -55, -40) && between(lat, -20, 0))  return("South American Epicontinental Seas")
    if (lat > 60) return("Arctic")
    return("Pacific")
  }
  
  # Upper Cretaceous
  if (e == "Upper Cretaceous") {
    if (between(lon, 30, 120)  && between(lat, -60, 30)) return("Tethys")
    if (between(lon, 0, 30)    && between(lat, -15, 15)) return("Trans-Saharan Seaway")
    if (between(lon, -60, 0)   && between(lat, 0, 40))   return("Western Tethys")
    if (between(lon, -50, -30) && between(lat, 30, 60))  return("Western Interior Seaway")
    if (between(lon, 0, 30)    && between(lat, 0, 60))   return("Tethys Seaway")
    if (between(lon, 30, 60)   && between(lat, 30, 60))  return("Tethys Seaway")
    if (between(lon, -30, 0)   && between(lat, -60, 0))  return("Atlantic")
    if (between(lon, -55, -40) && between(lat, -25, 0))  return("South American Epicontinental Seas")
    if (lat > 60) return("Arctic")
    return("Pacific")
  }
  
  # Paleocene
  if (e == "Paleocene") {
    if (lon < 0 && lon > -60 && between(lat,-60,60)) return("Atlantic")
    if (between(lon, 0, 120) && between(lat,-60,60)) return("Tethys")
    if (lat > 60)  return("Arctic")
    if (lat < -60) return("Southern")
    return("Pacific")
  }
  
  # Eocene
  if (e == "Eocene") {
    if (between(lon, 0, 120) && between(lat,-60,0))  return("Indian")
    if (between(lon, 0, 120) && between(lat,0,60))   return("Neo-Tethys")
    if (between(lon, -60, 0) && between(lat,-60,60)) return("Atlantic")
    if (lat > 60)  return("Arctic")
    if (lat < -60) return("Southern")
    return("Pacific")
  }
  
  # Oligocene
  if (e == "Oligocene") {
    if (between(lon, 0, 45)   && between(lat,22.5,60)) return("Neo-Tethys")
    if (between(lon, -60, 0)  && between(lat,-60,60))  return("Atlantic")
    if (between(lon, 30, 120) && between(lat,-60,15))  return("Indian")
    if (lat > 60)  return("Arctic")
    if (lat < -60) return("Southern")
    return("Pacific")
  }
  
  # Miocene
  if (e == "Miocene") {
    if (between(lon, 0, 45)   && between(lat,22.5,60)) return("Neo-Tethys")
    if (between(lon, -60, 0)  && between(lat,-60,60))  return("Atlantic")
    if (between(lon, 30, 120) && between(lat,-60,15))  return("Indian")
    if (lat > 60)  return("Arctic")
    if (lat < -60) return("Southern")
    return("Pacific")
  }
  
  # Pliocene / Pleistocene / Holocene
  if (e %in% c("Pliocene","Pleistocene","Holocene")) {
    if (between(lon, 0, 45)   && between(lat,22.5,60)) return("Mediterranean")
    if (between(lon, -60, 0)  && between(lat,-60,60))  return("Atlantic")
    if (between(lon, 30, 120) && between(lat,-60,20))  return("Indian")
    if (lat > 60)  return("Arctic")
    if (lat < -60) return("Southern")
    return("Pacific")
  }
  
  # Fallback
  NA_character_
}

# ---------- Load datasets ----------
col  <- read_csv(here::here("data", "Data_S1.csv"), show_col_types = FALSE, progress = FALSE) %>% drop_dot_cols()
occ  <- read_csv(here::here("data", "Data_S2.csv"), show_col_types = FALSE, progress = FALSE) %>% drop_dot_cols()
occ_original_cols <- names(occ)  # Store original column names before processing
refs <- read_csv(here::here("data", "Data_S3.csv"), show_col_types = FALSE, progress = FALSE) %>% drop_dot_cols()

# ---------- Load Taxonomy Lookup ----------
taxonomy_lookup <- readxl::read_excel(
  here::here("data", "Lookup_Taxonomy_version.30.3.xlsx"),
  .name_repair = "minimal"
) %>%
  select(Superorder, Order, Family, Genus, 
         starts_with("Synonym.")) %>%
  filter(!is.na(Genus))

# Build a lookup table: genus/synonym -> canonical genus with taxonomy
build_taxonomy_map <- function(lookup_df) {
  map <- list()
  
  for (i in seq_len(nrow(lookup_df))) {
    row <- lookup_df[i, ]
    canonical <- row$Genus
    info <- list(
      genus = canonical,
      family = row$Family,
      order = row$Order,
      superorder = row$Superorder
    )
    
    # Add canonical genus
    map[[tolower(canonical)]] <- info
    
    # Add synonyms
    for (syn_col in c("Synonym.1", "Synonym.2", "Synonym.3", "Synonym.4", "Synonym.5")) {
      syn <- row[[syn_col]]
      if (!is.na(syn) && syn != "") {
        map[[tolower(syn)]] <- info
      }
    }
  }
  map
}

taxonomy_map <- build_taxonomy_map(taxonomy_lookup)

# Function to clean identified_name and extract genus
clean_identified_name <- function(name) {
  if (is.na(name) || name == "") return(list(modified = NA, genus_extracted = NA))
  
  original <- name
  
  # Remove quotes
  name <- gsub('"', '', name)
  
  # Remove leading qualifiers like "n. gen.", "cf.", etc.
  name <- gsub("^\\s*n\\.\\s*gen\\.\\s*", "", name, ignore.case = TRUE)
  name <- gsub("^\\s*cf\\.\\s*", "", name, ignore.case = TRUE)
  name <- gsub("^\\s*aff\\.\\s*", "", name, ignore.case = TRUE)
  
  # Remove trailing/inline qualifiers
  name <- gsub("\\s+n\\.\\s*sp\\..*$", "", name, ignore.case = TRUE)
  name <- gsub("\\s+n\\.\\s*gen\\..*$", "", name, ignore.case = TRUE)
  name <- gsub("\\s+cf\\.\\s+", " ", name, ignore.case = TRUE)
  name <- gsub("\\s+aff\\.\\s+", " ", name, ignore.case = TRUE)
  name <- gsub("\\s+sp\\.\\s*\\d*$", "", name, ignore.case = TRUE)
  name <- gsub("\\s+sp\\.$", "", name, ignore.case = TRUE)
  name <- gsub("\\s+indet\\.?$", "", name, ignore.case = TRUE)
  name <- gsub("\\s*\\?\\s*", " ", name)
  name <- gsub("\\s*\\(.*?\\)\\s*", " ", name)  # Remove parenthetical content
  name <- gsub("\\s+\\d+$", "", name)  # Remove trailing numbers
  name <- gsub("\\s+(non vu|R)$", "", name, ignore.case = TRUE)
  
  # Clean up whitespace
  
  name <- trimws(gsub("\\s+", " ", name))
  
  # Extract genus (first word)
  words <- strsplit(name, "\\s+")[[1]]
  genus_extracted <- if (length(words) > 0) words[1] else NA
  
  # Modified name is the cleaned version
  modified <- if (nchar(name) > 0) name else NA
  
  list(modified = modified, genus_extracted = genus_extracted)
}

# Function to look up taxonomy for a genus
lookup_taxonomy <- function(genus) {
  if (is.na(genus) || genus == "") {
    return(list(accepted_genus = NA, family = NA, order = NA, superorder = NA, matched = FALSE))
  }
  
  key <- tolower(trimws(genus))
  
  if (key %in% names(taxonomy_map)) {
    info <- taxonomy_map[[key]]
    return(list(
      accepted_genus = info$genus,
      family = info$family,
      order = info$order,
      superorder = info$superorder,
      matched = TRUE
    ))
  }
  
  list(accepted_genus = NA, family = NA, order = NA, superorder = NA, matched = FALSE)
}

# Vectorized function to process all names
process_taxonomy_batch <- function(identified_names) {
  n <- length(identified_names)
  
  result <- data.frame(
    modified_identified_name = character(n),
    accepted_name = character(n),
    genus = character(n),
    family = character(n),
    order = character(n),
    superorder = character(n),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(n)) {
    cleaned <- clean_identified_name(identified_names[i])
    tax <- lookup_taxonomy(cleaned$genus_extracted)
    
    result$modified_identified_name[i] <- cleaned$modified %||% NA_character_
    result$genus[i] <- tax$accepted_genus %||% cleaned$genus_extracted %||% "Unknown"
    result$family[i] <- tax$family %||% "Unknown"
    result$order[i] <- tax$order %||% "Unknown"
    result$superorder[i] <- tax$superorder %||% "Unknown"
    
    # accepted_name: use modified name with accepted genus if found
    if (!is.na(tax$accepted_genus) && !is.na(cleaned$modified)) {
      # Replace original genus with accepted genus in the modified name
      words <- strsplit(cleaned$modified, "\\s+")[[1]]
      if (length(words) > 0) {
        words[1] <- tax$accepted_genus
        result$accepted_name[i] <- paste(words, collapse = " ")
      } else {
        result$accepted_name[i] <- tax$accepted_genus
      }
    } else {
      result$accepted_name[i] <- cleaned$modified %||% NA_character_
    }
  }
  
  result
}

# Ensure reference fields exist
if (!has_col(occ,  "reference_key"))  occ$reference_key  <- NA_character_
if (!has_col(col,  "reference_key"))  col$reference_key  <- NA_character_
if (!has_col(refs, "reference_key")) refs$reference_key <- NA_character_
if (!has_col(occ,  "reference_display"))  occ$reference_display  <- occ$reference %||% NA_character_
if (!has_col(col,  "reference_display"))  col$reference_display  <- col$reference %||% NA_character_
if (!has_col(refs, "reference_display")) refs$reference_display <- NA_character_

# ---------- Occurrence prep ----------
occ <- occ %>% mutate(
  name_curated = dplyr::coalesce(accepted_name, modified_identified_name, identified_name),
  name_raw     = dplyr::coalesce(identified_name, modified_identified_name, accepted_name),
  name_updated = !is.na(accepted_name) & accepted_name != identified_name,
  # Source by prefix of occurrence_number: PBDB_ -> PBDB, else (if present) Literature
  source = dplyr::case_when(
    !is.na(occurrence_number) & startsWith(occurrence_number, "PBDB_") ~ "PBDB",
    !is.na(occurrence_number)                                          ~ "Literature",
    TRUE                                                               ~ "Literature" # assume lit if missing id
  ),
  age_range_calc = ifelse(!is.na(max_ma) & !is.na(min_ma), max_ma - min_ma, NA_real_),
  age_range_any  = dplyr::coalesce(age_range, age_range_calc)
)

# Robust coll_id for occurrences
if ("collection_number" %in% names(occ)) {
  occ <- occ %>% mutate(coll_id = as.character(collection_number))
} else if ("collection_no" %in% names(occ)) {
  occ <- occ %>% mutate(coll_id = as.character(collection_no))
} else {
  # If neither exists, check if coll_id already exists, otherwise set to NA
  if (!"coll_id" %in% names(occ)) {
    occ <- occ %>% mutate(coll_id = NA_character_)
  }
}

# ---------- Collections prep ----------
# Robust coll_id
if ("collection_number" %in% names(col) && "collection_number" %in% names(col)) {
  col <- col %>% mutate(coll_id = dplyr::coalesce(as.character(collection_number),
                                                  as.character(collection_number)))
} else if ("collection_number" %in% names(col)) {
  col <- col %>% mutate(coll_id = as.character(collection_number))
} else if ("collection_number" %in% names(col)) {
  col <- col %>% mutate(coll_id = as.character(collection_number))
} else {
  col <- col %>% mutate(coll_id = NA_character_)
}

# Age range for collections (if missing)
if (!"age_range" %in% names(col)) {
  col <- col %>% mutate(age_range = ifelse(!is.na(max_ma) & !is.na(min_ma), max_ma - min_ma, NA_real_))
}

# Collection source by prefix of collection number (PBDB_/L_)
# Use collection_number if present else coll_id
col <- col %>%
  mutate(
    .coll_num_for_source = dplyr::coalesce(as.character(collection_number), as.character(coll_id)),
    collection_source = dplyr::case_when(
      !is.na(.coll_num_for_source) & startsWith(.coll_num_for_source, "PBDB_") ~ "PBDB",
      !is.na(.coll_num_for_source) & startsWith(.coll_num_for_source, "L_")    ~ "Literature",
      TRUE                                                                      ~ "Literature" # default
    )
  ) %>%
  select(-.coll_num_for_source)

# ---------- Color palette ----------
source_levels <- c("PBDB","Literature","PBDB_U")
occ$source <- factor(occ$source, levels = source_levels)
col$collection_source <- factor(col$collection_source, levels = source_levels)

pal_source <- colorFactor(
  palette = c("#b56a9c", "#037c6e", "#FF6B6B"),  # PBDB, Literature, PBDB_U (light blue)
  levels  = source_levels
)

# ---------- Filter choices ----------
epoch_choices_occ   <- us(c(occ$early_epoch,  occ$late_epoch))
period_choices_occ  <- us(c(occ$early_period, occ$late_period))
epoch_choices_col   <- us(c(col$early_epoch,  col$late_epoch))
period_choices_col  <- us(c(col$early_period, col$late_period))

continent_choices_occ  <- us(occ$continent)
paleoocean_choices_occ <- us(occ$paleoocean)
continent_choices_col  <- us(col$continent)
paleoocean_choices_col <- us(col$paleoocean)

country_choices_occ <- if ("country" %in% names(occ)) us(occ$country) else character(0)
country_choices_col <- if ("country" %in% names(col)) us(col$country) else character(0)

order_choices      <- us(occ$order)
superorder_choices <- us(occ$superorder)
family_choices     <- us(occ$family)
rank_choices       <- us(occ$rank)
status_choices     <- us(occ$status)

# Create taxonomic hierarchy mapping for cascading filters
# Superorder -> Orders mapping
superorder_to_orders <- occ %>%
  filter(!is.na(superorder), !is.na(order)) %>%
  distinct(superorder, order) %>%
  group_by(superorder) %>%
  summarize(orders = list(sort(unique(order))), .groups = 'drop')

# Convert to named list (more robust than deframe)
if (nrow(superorder_to_orders) > 0) {
  superorder_to_orders <- setNames(superorder_to_orders$orders, superorder_to_orders$superorder)
} else {
  superorder_to_orders <- list()
}

# Order -> Families mapping
order_to_families <- occ %>%
  filter(!is.na(order), !is.na(family)) %>%
  distinct(order, family) %>%
  group_by(order) %>%
  summarize(families = list(sort(unique(family))), .groups = 'drop')

# Convert to named list
if (nrow(order_to_families) > 0) {
  order_to_families <- setNames(order_to_families$families, order_to_families$order)
} else {
  order_to_families <- list()
}

# Superorder -> Families mapping (direct, for when order is not selected)
superorder_to_families <- occ %>%
  filter(!is.na(superorder), !is.na(family)) %>%
  distinct(superorder, family) %>%
  group_by(superorder) %>%
  summarize(families = list(sort(unique(family))), .groups = 'drop')

# Convert to named list
if (nrow(superorder_to_families) > 0) {
  superorder_to_families <- setNames(superorder_to_families$families, superorder_to_families$superorder)
} else {
  superorder_to_families <- list()
}

source_levels_occ      <- source_levels
source_levels_col_all  <- source_levels
