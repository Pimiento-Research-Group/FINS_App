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
library(httr2)

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
  if (length(epochs_sel)) {
    out <- out %>% filter(
      (early_epoch %in% epochs_sel) | 
        (late_epoch %in% epochs_sel) |
        (is.na(early_epoch) & is.na(late_epoch))  # Keep rows with no epoch data
    )
  }
  if (length(periods_sel)) {
    out <- out %>% filter(
      (early_period %in% periods_sel) | 
        (late_period %in% periods_sel) |
        (is.na(early_period) & is.na(late_period))  # Keep rows with no period data
    )
  }
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

# Locality ID assignment
# A "locality" is a unique latitude + longitude + min_ma + max_ma combination.
# New rows matching an already-loaded locality reuse its ID; genuinely new localities
# get the next integer after the current maximum. Rows missing any key value get NA.
# `existing` is the currently loaded collections (NULL = none, e.g. replace mode).
assign_locality_ids <- function(new_df, existing, digits = 5) {
  key_cols <- c("latitude", "longitude", "min_ma", "max_ma")
  
  make_key <- function(d) {
    if (is.null(d) || nrow(d) == 0 || !all(key_cols %in% names(d))) return(character(0))
    lat <- suppressWarnings(as.numeric(d[["latitude"]]))
    lng <- suppressWarnings(as.numeric(d[["longitude"]]))
    mn  <- suppressWarnings(as.numeric(d[["min_ma"]]))
    mx  <- suppressWarnings(as.numeric(d[["max_ma"]]))
    ok  <- is.finite(lat) & is.finite(lng) & is.finite(mn) & is.finite(mx)
    ifelse(ok, paste(round(lat, digits), round(lng, digits),
                     round(mn, digits),  round(mx, digits), sep = "|"), NA_character_)
  }
  
  existing_map <- integer(0); max_id <- 0L
  if (!is.null(existing) && "locality_id" %in% names(existing) && nrow(existing) > 0) {
    ex_key <- make_key(existing)
    ex_id  <- suppressWarnings(as.integer(as.character(existing[["locality_id"]])))
    valid  <- !is.na(ex_key) & !is.na(ex_id)
    if (any(valid)) existing_map <- tapply(ex_id[valid], ex_key[valid], function(x) x[1])
    if (any(!is.na(ex_id))) max_id <- max(ex_id, na.rm = TRUE)
  }
  
  new_key <- make_key(new_df)
  out_id  <- rep(NA_integer_, nrow(new_df))
  batch_map <- integer(0); next_id <- max_id
  
  for (i in seq_len(nrow(new_df))) {
    k <- new_key[i]
    if (is.na(k)) next
    if (k %in% names(existing_map))      out_id[i] <- existing_map[[k]]
    else if (k %in% names(batch_map))    out_id[i] <- batch_map[[k]]
    else { next_id <- next_id + 1L; batch_map[[k]] <- next_id; out_id[i] <- next_id }
  }
  
  new_df$locality_id <- out_id
  new_df
}

# Duplicate marking for newly added occurrences
# Marks each new occurrence "duplicate"/"unique" for a given identifier, considering
# both the preloaded occurrences and earlier rows in the same batch. The first time a
# (accepted_name + id) combination appears — and it isn't already in the preloaded set —
# it is "unique"; every later occurrence of that combination is "duplicate".
# id_col = "collection_number" resolves to coalesce(collection_number, coll_id).
mark_occurrence_duplicates <- function(new_df, existing_df, id_col) {
  key <- function(d) {
    if (is.null(d) || nrow(d) == 0) return(character(0))
    nm <- if ("accepted_name" %in% names(d)) as.character(d[["accepted_name"]])
    else rep(NA_character_, nrow(d))
    id <- if (identical(id_col, "collection_number")) {
      cn <- if ("collection_number" %in% names(d)) as.character(d[["collection_number"]])
      else rep(NA_character_, nrow(d))
      ci <- if ("coll_id" %in% names(d)) as.character(d[["coll_id"]])
      else rep(NA_character_, nrow(d))
      dplyr::coalesce(cn, ci)
    } else if (id_col %in% names(d)) as.character(d[[id_col]]) else rep(NA_character_, nrow(d))
    paste(nm, id, sep = "\u001f")
  }
  new_key <- key(new_df)
  ex_key  <- key(existing_df)
  ifelse((new_key %in% ex_key) | duplicated(new_key), "duplicate", "unique")
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
  salma_eocene    <- c("Itaboraian", "Casamayoran", "Mustersan", "Divisaderan")
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
  "Colhuehuapian","Santacrucian","Friasian","Colloncuran","Laventan","Mayoan","Chasiquian","Huayquerian",
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
.recon_models_v3 <- c(golonka  = "GOLONKA",
                      paleomap = "PALEOMAP",
                      seton    = "SETON2012",
                      matthews = "MATTHEWS2016_mantle_ref",
                      muller   = "MULLER2019")
# coalesce priority for the composite coordinate used in ocean assignment (script 3)
.recon_coalesce_order <- c("golonka","seton","paleomap","matthews","muller")

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

# Reconstruct one point across a vector of ages for one model -> n x 2 matrix (long, lat)
.reconstruct_ages <- function(lon, lat, ages, model_name) {
  pt  <- data.frame(longitude = lon, latitude = lat)
  res <- tryCatch(rgplates::reconstruct(pt, age = ages, model = model_name),
                  error = function(e) NULL)
  if (is.null(res)) return(matrix(NA_real_, length(ages), 2))
  m <- tryCatch(do.call(rbind, lapply(res, function(r) {
    r <- as.data.frame(r)
    if (nrow(r) < 1 || ncol(r) < 2) c(NA_real_, NA_real_) else as.numeric(r[1, 1:2])
  })), error = function(e) NULL)
  if (is.null(m) || nrow(m) != length(ages)) matrix(NA_real_, length(ages), 2) else m
}

# Full per-collection result: 10 per-model median coords + composite modal paleoocean.
# `ages` is the pre-sampled vector (sampling/seed handled by the caller, as offline).
.paleo_reconstruct_collection <- function(lon, lat, ages) {
  mk <- names(.recon_models_v3)
  coord_keys <- as.vector(rbind(paste0("paleolong_", mk), paste0("paleolat_", mk)))
  res <- setNames(as.list(rep(NA_real_, length(coord_keys))), coord_keys)
  res$paleoocean <- NA_character_; res$ok_coords <- FALSE; res$ok_ocean <- FALSE
  if (is.na(lon) || is.na(lat) || is.null(ages) || !length(ages) || all(is.na(ages))) return(res)
  if (!requireNamespace("rgplates", quietly = TRUE)) return(res)
  
  per_model <- lapply(.recon_models_v3, function(mn) .reconstruct_ages(lon, lat, ages, mn))
  names(per_model) <- mk
  
  for (nm in mk) {
    mm <- per_model[[nm]]
    res[[paste0("paleolong_", nm)]] <- stats::median(mm[, 1], na.rm = TRUE)
    res[[paste0("paleolat_",  nm)]] <- stats::median(mm[, 2], na.rm = TRUE)
  }
  res$ok_coords <- any(is.finite(unlist(res[coord_keys])))
  
  # composite coordinate per age sample (coalesce across models), then classify + mode
  n <- length(ages); clong <- rep(NA_real_, n); clat <- rep(NA_real_, n)
  for (j in seq_len(n)) for (nm in .recon_coalesce_order) {
    v <- per_model[[nm]][j, ]
    if (is.finite(v[1]) && is.finite(v[2])) { clong[j] <- v[1]; clat[j] <- v[2]; break }
  }
  keep <- is.finite(clong) & is.finite(clat)
  if (any(keep)) {
    ep <- vapply(ages[keep], .epoch_from_age_ma, character(1))
    oc <- mapply(.classify_ocean, ep, clong[keep], clat[keep], USE.NAMES = FALSE)
    om <- .mode1(oc)
    if (!is.na(om)) { res$paleoocean <- om; res$ok_ocean <- TRUE }
  }
  res
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
  if (age <= 174.7)  return("Middle Jurassic")
  if (age <= 201.4)  return("Lower Jurassic")
  NA_character_
}

.classify_ocean <- function(epoch_label, lon, lat) {
  if (is.na(lon) || is.na(lat)) return(NA_character_)
  e <- epoch_label %||% NA_character_
  if (is.na(e)) return("Pacific")
  btw <- function(x, a, b) is.finite(x) && x > a && x < b
  
  if (e == "Lower Jurassic")  { if (btw(lon,0,120)  && btw(lat,-40,45)) return("Tethys"); return("Pacific") }
  if (e == "Middle Jurassic") { if (btw(lon,0,120)  && btw(lat,-45,45)) return("Tethys"); return("Pacific") }
  if (e == "Upper Jurassic")  { if (btw(lon,22,130) && btw(lat,-40,30)) return("Tethys"); return("Pacific") }
  
  if (e == "Lower Cretaceous") {
    if (btw(lon,-70,-30) && btw(lat,30,70))  return("Western Interior Seaway")
    if (btw(lon,30,120)  && btw(lat,-60,30)) return("Tethys")
    if (btw(lon,-50,0)   && btw(lat,0,40))   return("Western Tethys")
    if (btw(lon,0,30)    && btw(lat,0,60))   return("Tethys Seaway")
    if (btw(lon,30,60)   && btw(lat,30,60))  return("Tethys Seaway")
    if (btw(lon,-30,0)   && btw(lat,-60,0))  return("Atlantic")
    if (btw(lon,-55,-40) && btw(lat,-20,0))  return("South American Epicontinental Seas")
    if (lat > 60) return("Arctic"); return("Pacific")
  }
  if (e == "Upper Cretaceous") {
    if (btw(lon,30,120)  && btw(lat,-60,30)) return("Tethys")
    if (btw(lon,0,30)    && btw(lat,-15,15)) return("Trans-Saharan Seaway")
    if (btw(lon,-60,0)   && btw(lat,0,40))   return("Western Tethys")
    if (btw(lon,-50,-30) && btw(lat,30,60))  return("Western Interior Seaway")
    if (lat > 60) return("Arctic"); return("Pacific")
  }
  if (e == "Paleocene") {
    if (lat > 60)  return("Arctic")
    if (lat < -60) return("Southern")
    if (btw(lon,0,120) && btw(lat,-60,60)) return("Tethys"); return("Pacific")
  }
  if (e == "Eocene") {
    if (btw(lon,0,120) && btw(lat,-60,0)) return("Indian")
    if (btw(lon,0,120) && btw(lat,0,60))  return("Neo-Tethys")
    if (btw(lon,-60,0) && btw(lat,-60,60)) return("Atlantic"); return("Pacific")
  }
  if (e == "Oligocene" || e == "Miocene") {
    if (btw(lon,0,45)   && btw(lat,22.5,60)) return("Neo-Tethys")
    if (btw(lon,-60,0)  && btw(lat,-60,60))  return("Atlantic")
    if (btw(lon,30,120) && btw(lat,-60,15))  return("Indian"); return("Pacific")
  }
  if (e %in% c("Pliocene","Pleistocene","Holocene")) {
    if (btw(lon,0,45)   && btw(lat,22.5,60)) return("Mediterranean")
    if (btw(lon,-60,0)  && btw(lat,-60,60))  return("Atlantic")
    if (btw(lon,30,120) && btw(lat,-60,20))  return("Indian"); return("Pacific")
  }
  "Pacific"
}
# ---------- Load datasets ----------
col  <- read_csv(here::here("data", "collections_norm.csv"), show_col_types = FALSE, progress = FALSE) %>% drop_dot_cols()
occ  <- read_csv(here::here("data", "occurrences_norm.csv"), show_col_types = FALSE, progress = FALSE) %>% drop_dot_cols()
occ_original_cols <- names(occ)  # Store original column names before processing
refs <- read_csv(here::here("data", "references_norm.csv"), show_col_types = FALSE, progress = FALSE) %>% drop_dot_cols()

# ---------- Load Taxonomy Lookup ----------
taxonomy_lookup <- readxl::read_excel(
  here::here("data", "Lookup_Taxonomy.xlsx"),
  .name_repair = "minimal"
) %>%
  select(Superorder, Order, Family, Genus, 
         starts_with("Synonym.")) %>%
  filter(!is.na(Genus))

# ---------- Load Extant Taxa Lookup ----------
extant_species <- readxl::read_excel(
  here::here("data", "extant_taxa.xlsx"),
  sheet = "Species",
  .name_repair = "minimal"
)

extant_genera <- readxl::read_excel(
  here::here("data", "extant_taxa.xlsx"),
  sheet = "Genus",
  .name_repair = "minimal"
)

extant_families <- readxl::read_excel(
  here::here("data", "extant_taxa.xlsx"),
  sheet = "Family",
  .name_repair = "minimal"
)

# Create lowercase lookup vectors for matching
# Species: check which column contains species names (binomial names)
extant_species_list <- tolower(trimws(extant_species[[1]]))

# Genera: check which column contains genus names
extant_genera_list <- tolower(trimws(extant_genera[[1]]))

# Families: find column containing family names (ending in -dae or -idae)
# The family names are not in column 1, need to find the right column
family_col_idx <- which(sapply(extant_families, function(col) {
  any(grepl("dae$", col, ignore.case = TRUE), na.rm = TRUE)
}))[1]

if (!is.na(family_col_idx)) {
  extant_families_list <- tolower(trimws(extant_families[[family_col_idx]]))
} else {
  # Fallback to column 3 based on typical structure
  extant_families_list <- tolower(trimws(extant_families[[3]]))
}
extant_families_list <- extant_families_list[!is.na(extant_families_list) & extant_families_list != ""]

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
# Function to look up taxonomy for a genus
lookup_taxonomy <- function(genus, tax_map = NULL) {
  if (is.na(genus) || genus == "") {
    return(list(accepted_genus = NA, family = NA, order = NA, superorder = NA, matched = FALSE))
  }
  
  # Use provided map or default
  if (is.null(tax_map)) {
    tax_map <- taxonomy_map
  }
  
  key <- tolower(trimws(genus))
  
  if (key %in% names(tax_map)) {
    info <- tax_map[[key]]
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
process_taxonomy_batch <- function(identified_names, tax_map = NULL) {
  
  n <- length(identified_names)
  
  result <- data.frame(
    modified_identified_name = character(n),
    accepted_name = character(n),
    genus = character(n),
    family = character(n),
    order = character(n),
    superorder = character(n),
    rank = character(n),
    status = character(n),
    genus_status = character(n),
    stringsAsFactors = FALSE
  )
  
  # Define known higher taxa
  superorder_names <- c("Batoidea", "Galeomorphii", "Squalomorphii")
  clade_names <- c("Selachii")
  subcohort_names <- c("Neoselachii")
  
  # Extinct order
  extinct_orders <- c("Synechodontiformes")
  
  for (i in seq_len(n)) {
    cleaned <- clean_identified_name(identified_names[i])
    tax <- lookup_taxonomy(cleaned$genus_extracted, tax_map)
    
    result$modified_identified_name[i] <- cleaned$modified %||% NA_character_
    
    # Check if this is a genus/species level name that failed lookup
    # (we'll determine rank first to know how to handle it)
    name_to_check <- cleaned$modified %||% ""
    words_for_rank <- strsplit(trimws(name_to_check), "\\s+")[[1]]
    word_count_for_rank <- length(words_for_rank)
    first_word_for_rank <- if (word_count_for_rank > 0) words_for_rank[1] else ""
    has_sp_for_rank <- any(grepl("^sp\\.?$", words_for_rank, ignore.case = TRUE))
    
    # Determine if this appears to be genus/species level (not family/order/superorder)
    is_higher_rank <- grepl("dae$", first_word_for_rank, ignore.case = TRUE) ||  # family
      grepl("formes$", first_word_for_rank, ignore.case = TRUE) ||  # order
      first_word_for_rank %in% c("Batoidea", "Galeomorphii", "Squalomorphii", "Selachii", "Neoselachii")
    
    is_genus_species_level <- !is_higher_rank && word_count_for_rank >= 1
    
    # If taxonomy lookup failed and this looks like genus/species level, flag it
    if (!tax$matched && is_genus_species_level) {
      result$genus[i] <- "Unknown"
      result$family[i] <- "Unknown"
      result$order[i] <- "Unknown"
      result$superorder[i] <- "Unknown"
      result$accepted_name[i] <- "Unknown"  # Flag as not in taxonomy list
    } else if (tax$matched) {
      result$genus[i] <- tax$accepted_genus
      result$family[i] <- tax$family %||% "Unknown"
      result$order[i] <- tax$order %||% "Unknown"
      result$superorder[i] <- tax$superorder %||% "Unknown"
      
      # Determine accepted_name with validated genus
      if (!is.na(tax$accepted_genus) && !is.na(cleaned$modified)) {
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
    } else {
      # Higher-rank identification that didn't need genus lookup
      result$genus[i] <- "Unknown"
      result$family[i] <- "Unknown"
      result$order[i] <- "Unknown"
      result$superorder[i] <- "Unknown"
      result$accepted_name[i] <- cleaned$modified %||% NA_character_
    }
    
    # Determine rank based on accepted_name
    # Skip if already flagged as Unknown (unrecognized name)
    if (result$accepted_name[i] == "Unknown") {
      result$rank[i] <- "Unknown"
    } else {
      name_to_check <- result$accepted_name[i]
      if (is.na(name_to_check) || name_to_check == "") {
        result$rank[i] <- "Unknown"
      } else {
        words <- strsplit(trimws(name_to_check), "\\s+")[[1]]
        word_count <- length(words)
        first_word <- if (word_count > 0) words[1] else ""
        has_sp <- any(grepl("^sp\\.?$", words, ignore.case = TRUE))
        
        if (word_count >= 2 && !has_sp) {
          result$rank[i] <- "species"
        } else if (word_count >= 2 && has_sp) {
          result$rank[i] <- "genus"
        } else if (word_count == 1) {
          if (first_word %in% subcohort_names) {
            result$rank[i] <- "subcohort"
          } else if (first_word %in% clade_names) {
            result$rank[i] <- "clade"
          } else if (first_word %in% superorder_names) {
            result$rank[i] <- "superorder"
          } else if (grepl("formes$", first_word, ignore.case = TRUE)) {
            result$rank[i] <- "order"
          } else if (grepl("dae$", first_word, ignore.case = TRUE)) {
            result$rank[i] <- "family"
          } else {
            result$rank[i] <- "genus"
          }
        } else {
          result$rank[i] <- "Unknown"
        }
      }
    }
    
    # Determine status and genus_status based on rank and extant lists
    current_rank <- result$rank[i]
    
    # Fix taxonomy fields based on actual rank (when taxonomy lookup failed)
    if (!tax$matched) {
      name_for_rank <- trimws(result$accepted_name[i] %||% cleaned$genus_extracted %||% "")
      
      if (current_rank == "family") {
        # The "genus" extracted is actually a family name
        result$genus[i] <- "Unknown"
        result$family[i] <- name_for_rank
        
        # Try to look up order/superorder from taxonomy_lookup based on family
        family_rows <- taxonomy_lookup[tolower(taxonomy_lookup$Family) == tolower(name_for_rank), ]
        if (nrow(family_rows) > 0) {
          result$order[i] <- family_rows$Order[1]
          result$superorder[i] <- family_rows$Superorder[1]
        } else {
          result$order[i] <- "Unknown"
          result$superorder[i] <- "Unknown"
        }
        
      } else if (current_rank == "order") {
        # The "genus" extracted is actually an order name
        result$genus[i] <- "Unknown"
        result$family[i] <- "Unknown"
        result$order[i] <- name_for_rank
        
        # Try to look up superorder from taxonomy_lookup based on order
        order_rows <- taxonomy_lookup[tolower(taxonomy_lookup$Order) == tolower(name_for_rank), ]
        if (nrow(order_rows) > 0) {
          result$superorder[i] <- order_rows$Superorder[1]
        } else {
          result$superorder[i] <- "Unknown"
        }
        
      } else if (current_rank == "superorder") {
        # The "genus" extracted is actually a superorder name
        result$genus[i] <- "Unknown"
        result$family[i] <- "Unknown"
        result$order[i] <- "Unknown"
        result$superorder[i] <- name_for_rank
        
      } else if (current_rank == "clade" || current_rank == "subcohort") {
        # Higher-level groupings
        result$genus[i] <- "Unknown"
        result$family[i] <- "Unknown"
        result$order[i] <- "Unknown"
        result$superorder[i] <- "Unknown"
        
      } else if (current_rank == "genus" || current_rank == "species" || current_rank == "Unknown") {
        # Genus not found in lookup - keep extracted genus, mark rest as Unknown
        if (is.na(result$genus[i]) || result$genus[i] == "") {
          result$genus[i] <- cleaned$genus_extracted %||% "Unknown"
        }
        if (is.na(result$family[i]) || result$family[i] == "") {
          result$family[i] <- "Unknown"
        }
        if (is.na(result$order[i]) || result$order[i] == "") {
          result$order[i] <- "Unknown"
        }
        if (is.na(result$superorder[i]) || result$superorder[i] == "") {
          result$superorder[i] <- "Unknown"
        }
      }
    }
    
    # Prepare lowercase versions for status checks
    accepted_name_lower <- tolower(trimws(result$accepted_name[i] %||% ""))
    genus_lower <- tolower(trimws(result$genus[i] %||% ""))
    family_lower <- tolower(trimws(result$family[i] %||% ""))
    order_name <- result$order[i] %||% ""
    
    # Skip status determination if name is unrecognized
    if (result$accepted_name[i] == "Unknown") {
      result$status[i] <- "NA"
      result$genus_status[i] <- "NA"
    } else if (current_rank == "species") {
      # Check species against extant_species_list
      if (accepted_name_lower %in% extant_species_list) {
        result$status[i] <- "extant"
      } else {
        result$status[i] <- "extinct"
      }
      # Check genus for genus_status
      if (genus_lower %in% extant_genera_list) {
        result$genus_status[i] <- "extant"
      } else {
        result$genus_status[i] <- "extinct"
      }
      
    } else if (current_rank == "genus") {
      # Check genus against extant_genera_list
      if (genus_lower %in% extant_genera_list) {
        result$status[i] <- "extant"
      } else {
        result$status[i] <- "extinct"
      }
      # genus_status same as status for genus-level
      result$genus_status[i] <- result$status[i]
      
    } else if (current_rank == "family") {
      # For family-level IDs, check the family field against extant_families_list
      if (family_lower %in% extant_families_list) {
        result$status[i] <- "extant"
      } else {
        result$status[i] <- "extinct"
      }
      # genus_status is NA for family-level
      result$genus_status[i] <- "NA"
      
    } else if (current_rank == "order") {
      # All orders extant except Synechodontiformes
      if (order_name %in% extinct_orders) {
        result$status[i] <- "extinct"
      } else {
        result$status[i] <- "extant"
      }
      # genus_status is NA for order-level
      result$genus_status[i] <- "NA"
      
    } else if (current_rank %in% c("superorder", "clade", "subcohort")) {
      # All superorders/clades/subcohorts are extant
      result$status[i] <- "extant"
      # genus_status is NA
      result$genus_status[i] <- "NA"
      
    } else {
      # Unknown rank
      result$status[i] <- "Unknown"
      result$genus_status[i] <- "NA"
    }
  }
  
  result
}

# All names the default lookup recognises (genera, synonyms, and the higher taxa in
# the Superorder/Order/Family columns), lowercased — for verifiability checks.
# Full set of names a lookup recognises (genera, synonyms, families, orders, superorders), lowercased.
build_known_names <- function(lookup_df) {
  cols <- intersect(c("Superorder", "Order", "Family", "Genus",
                      grep("^Synonym", names(lookup_df), value = TRUE)),
                    names(lookup_df))
  vals <- tolower(trimws(unlist(lapply(cols, function(cc) as.character(lookup_df[[cc]])),
                                use.names = FALSE)))
  unique(vals[!is.na(vals) & vals != ""])
}
taxonomy_known_names <- build_known_names(taxonomy_lookup)

# Uncertainty markers in identified_name (ported from the Python rules).
.uncertainty_pattern <- paste(
  "aff\\.",            # affinity
  "cfr?\\.",           # cf. / cfr.
  "\\?",               # question mark
  "ex\\.?\\s*gr\\.",   # ex gr. / ex. gr.
  "\\bgr\\.",          # bare 'gr.'
  "[\"'\u2018\u2019\u201c\u201d]",  # straight + curly quotes
  sep = "|"
)

# uncertain    : identified_name contains an uncertainty marker
# unverifiable : leading taxon of modified_identified_name not in the lookup
# valid        : everything else
classify_taxonomy_validation <- function(identified_name, modified_identified_name,
                                         known_names = taxonomy_known_names) {
  idn <- ifelse(is.na(identified_name), "", as.character(identified_name))
  mod <- ifelse(is.na(modified_identified_name), "", as.character(modified_identified_name))
  uncertain <- grepl(.uncertainty_pattern, idn, perl = TRUE, ignore.case = TRUE)
  lead <- tolower(vapply(strsplit(trimws(mod), "\\s+"),
                         function(w) if (length(w) && nzchar(w[1])) w[1] else "",
                         character(1)))
  in_lookup <- nzchar(lead) & lead %in% known_names
  ifelse(uncertain, "uncertain", ifelse(in_lookup, "valid", "unverifiable"))
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
source_levels <- c("PBDB","Literature","U")
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
