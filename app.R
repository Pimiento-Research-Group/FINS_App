# app.R — FINS Explorer (CSV-only): Welcome, Occurrences, Collections, References

library(shiny)
library(dplyr)
library(readr)
library(DT)
library(leaflet)
library(markdown)   # to render README.md
library(tidyr)
library(rgplates)

options(shiny.maxRequestSize = 50 * 1024^2)  # allow up to 50MB uploads

# ---------- helpers ----------
`%||%` <- function(a, b) ifelse(is.na(a), b, a)
us <- function(x) sort(unique(na.omit(x)))
drop_dot_cols <- function(df) df %>% select(-matches("^\\.\\.\\d+$"), everything())
has_col <- function(df, nm) nm %in% names(df)

# Epoch/Period filters: keep row if EITHER early_* OR late_* matches a selected value
overlaps_time <- function(df, epochs_sel, periods_sel) {
  out <- df
  if (length(epochs_sel))  out <- out %>% filter((early_epoch %in% epochs_sel)  | (late_epoch %in% epochs_sel))
  if (length(periods_sel)) out <- out %>% filter((early_period %in% periods_sel) | (late_period %in% periods_sel))
  out
}
apply_geog <- function(df, continents, paleo) {
  out <- df
  if (length(continents)) out <- out %>% filter(continent %in% continents)
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
    "Ecuador"="South America","Venezuela"="South America","Guyana"="South America","Suriname"="South America"
    ,
    # Africa
    "South Africa"="Africa","Egypt"="Africa","Morocco"="Africa","Tunisia"="Africa","Kenya"="Africa",
    "Tanzania"="Africa","Ethiopia"="Africa","Namibia"="Africa","Algeria"="Africa",
    # Oceania
    "Australia"="Oceania","New Zealand"="Oceania"
  )
  unname(ifelse(cn %in% names(fb), fb[cn], NA_character_))
}

.timescale_lookup <- local({
  row <- function(label, epoch = NA, period = NA, era = NA) {
    tibble::tibble(label = label, epoch = epoch, period = period, era = era)
  }
  
  # ---- ICS stages/epochs/periods you already had (Cretaceous + Cenozoic) ----
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
  
  # ---- Regional/biostrat aliases (epoch/period/era mapping) ----
  # New Zealand (GNS timescale; map to ICS epoch/period) (Rain et al. 2015, N. Z. J. Geol. Geophys.)
  nz_low_cret <- c("Korangan", "Urutawan", "Motuan")                                            # Lower Cretaceous / Cretaceous (small part of Motuan overlaps with Upper Cretaceous)
  nz_up_cret <- c("Ngaterian", "Arowhanan", "Mangaotanean", "Teratan", "Piripauan", "Haumurian") # Upper Cretaceous / Cretaceous
  nz_paleo <- c("Teurian")                                                                      # Paleocene / Paleogene
  nz_eoc <- c("Waipawan", "Mangaorapan", "Heretaungan", "Porangan", "Bortonian", "Kaiatan", "Runangan") # Eocene / Paleogene
  nz_oligo   <- c("Whaingaroan","Duntroonian","Waitakian")                                      # Oligocene / Paleogene (small part of Waitakian overlaps with Aquitanian)
  nz_miocene <- c("Otaian","Altonian","Clifdenian","Lillburnian","Waiauan","Tongaporutuan","Kapitean")   # Miocene / Neogene
  nz_plio    <- c("Opoitian","Waipipian","Mangapanian")                                          # Pliocene / Neogene (small part of Mangapanian overlaps with Pleistiocene)
  nz_pleist  <- c("Nukumaruan","Castlecliffian")                                                 # Pleistocene / Quaternary
  nz_holo    <- c("Haweran")                                                                      # often used regionally for latest Pleistocene–Holocene; map to Holocene epoch
  
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
  
  # European Land Mammal Ages — MN (Neogene) → Miocene/Pliocene (Neogene) (Agustí et al. 2001, Earth-Science Reviews)
  elma_mn_miocene <- c("MN 1","MN 2","MN 3","MN 4","MN 5","MN 6","MN 7/8","MN 7","MN 8","MN 9","MN 10","MN 11","MN 12","MN 13")
  elma_mn_plio    <- c("MN 14","MN 15","MN 16")
  elma_mn_q       <- c("MN 17")
  mn_rows <- dplyr::bind_rows(
    tibble::tibble(label = elma_mn_miocene, epoch = "Miocene",  period = "Neogene", era = "Cenozoic"),
    tibble::tibble(label = elma_mn_plio,    epoch = "Pliocene", period = "Neogene", era = "Cenozoic"),
    tibble::tibble(label = elma_mn_q,    epoch = "Pleistocene", period = "Quaternary", era = "Cenozoic")
  )
  
  # Mammal Paleogene (MP 1–30) → Paleogene (epoch varies; map conservatively by epoch blocks)
  mp_paleocene <- paste0("MP ", 1:10)
  mp_eocene    <- paste0("MP ", 11:20)
  mp_oligo     <- paste0("MP ", 21:30)
  mp_rows <- dplyr::bind_rows(
    tibble::tibble(label = mp_paleocene, epoch = "Paleocene", period = "Paleogene", era = "Cenozoic"),
    tibble::tibble(label = mp_eocene,    epoch = "Eocene",    period = "Paleogene", era = "Cenozoic"),
    tibble::tibble(label = mp_oligo,     epoch = "Oligocene", period = "Paleogene", era = "Cenozoic")
  )
  
  # North American Land Mammal Ages (NALMA) — epoch-level mapping (Alroy 2000, Paleobiology; Fowler 2017, Plos One)
  nalma_up_cret <- c("Aquilian", "Wahweapian", "Judithian", "Kirtlandian", "Edmontonian", "Lancian")
  nalma_paleoc <- c("Puercan","Torrejonian","Tiffanian","Clarkforkian")
  nalma_eocene <- c("Wasatchian","Bridgerian","Uintan","Duchesnean")
  nalma_oligo  <- c("Chadronian","Orellan","Whitneyan","Arikareean")
  nalma_mioc   <- c("Hemingfordian","Barstovian","Clarendonian","Hemphillian")
  nalma_plio  <- c("Blancan") # small part of Blancan overlaps with Pleistocene
  nalma_pleisto <- c("Irvingtonian","Rancholabrean")
  nalma_holo <- c("Santarosean", "Saintaugustinean")
  nalma_rows <- dplyr::bind_rows(
    tibble::tibble(label = nalma_up_cret, epoch = "Upper Cretaceous",      period = "Cretaceous",  era = "Mesozoic"),
    tibble::tibble(label = nalma_eocene, epoch = "Paleocene",   period = "Paleogene",  era = "Cenozoic"),
    tibble::tibble(label = nalma_eocene, epoch = "Eocene",      period = "Paleogene",  era = "Cenozoic"),
    tibble::tibble(label = nalma_oligo,  epoch = "Oligocene",   period = "Paleogene",  era = "Cenozoic"),
    tibble::tibble(label = nalma_mioc,   epoch = "Miocene",     period = "Neogene",    era = "Cenozoic"),
    tibble::tibble(label = nalma_plio, epoch = "Pliocene",    period = "Neogene",    era = "Cenozoic"),
    tibble::tibble(label = nalma_pleisto, epoch = "Pleistocene",    period = "Quaternary",    era = "Cenozoic"),
    tibble::tibble(label = nalma_holo, epoch = "Holocene",    period = "Quaternary",    era = "Cenozoic")
  )
  
  
  # Central Paratethys regional stages (Miocene) — map to Miocene/Neogene (Kováč et al. 2018, Geologica Carpathica)
  paratethys <- c("Egerian","Eggenburgian","Ottnangian","Karpatian","Badenian","Sarmatian","Pannonian","Pontian")
  paratethys_rows <- tibble::tibble(label = paratethys, epoch = "Miocene", period = "Neogene", era = "Cenozoic")
  
  # South American Land Mammal Ages (SALMA) — epoch-level mappingn (Flynn & Swisher 1996, in Geochronology, Time Scales and Global Stratigraphic Correlation )
  salma_paleocene <- c("Tiupampan", "Peligran" ,"Riochican")     # part of Riochican overlaps wiht Eocene
  salma_eocene    <- c("Itaboraian, Casamayoran", "Mustersan", "Divisaderan")
  salma_oligo     <- c("Tinguirirican", "Deseadan")                     # part of Tinguirirican overlaps with Eocene; part of Deseadan overlaps with Miocene
  salma_mioc     <- c("Colhuehuapian","Santacrucian","Friasian","Colloncuran","Laventan","Mayoan","Chasiquian","Huayquerian")
  salma_plio   <- c("Montehermosan","Chapadmalalan") # part of Montehermosan overlaps with Miocene
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

# ---------- Normalizer + hierarchy resolver (unchanged usage) ----------
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
  "Gelasian","Calabrian","Chibanian","Late Pleistocene","Greenlandian","Northgrippian","Meghalayan"
)
.ics_epoch_set  <- c("Early Cretaceous","Late Cretaceous","Paleocene","Eocene","Oligocene","Miocene","Pliocene","Pleistocene","Holocene")
.ics_period_set <- c("Cretaceous","Paleogene","Neogene","Quaternary")
.ics_era_set    <- c("Mesozoic","Cenozoic")

.nz_stage_set <- c(
  "Korangan","Urutawan","Motuan","Ngaterian","Arowhanan","Mangaotanean","Teratan","Piripauan","Haumurian",
  "Teurian","Waipawan","Mangaorapan","Heretaungan","Porangan","Bortonian","Kaiatan","Runangan",
  "Whaingaroan","Duntroonian","Waitakian","Otaian","Altonian","Clifdenian","Lillburnian","Waiauan",
  "Tongaporutuan","Kapitean","Opoitian","Waipipian","Mangapanian","Nukumaruan","Castlecliffian","Haweran"
)
.paratethys_stage_set <- c("Egerian","Eggenburgian","Ottnangian","Karpatian","Badenian","Sarmatian","Pannonian","Pontian")
.nalma_set <- c(
  "Aquilian","Wahweapian","Judithian","Kirtlandian","Edmontonian","Lancian","Puercan","Torrejonian","Tiffanian","Clarkforkian",
  "Wasatchian","Bridgerian","Uintan","Duchesnean","Chadronian","Orellan","Whitneyan","Arikareean",
  "Hemingfordian","Barstovian","Clarendonian","Hemphillian","Blancan","Irvingtonian","Rancholabrean",
  "Santarosean","Saintaugustinean"
)
.salma_set <- c(
  "Tiupampan","Peligran","Riochican",
  "Itaboraian","Casamayoran",
  "Mustersan","Divisaderan","Tinguirirican","Deseadan",
  "Colhuehuapian","Santacrucian","Friasian","Colloncuran","Laventan","Mayoan","Chasiquian","Huayquerian",
  "Montehermosan","Chapadmalalan","Uquian","Ensenadan","Lujanian"
)
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



# ---------- load datasets (prefer *_norm if present) ----------
path_occ  <- if (file.exists("data/Data_S2_norm.csv")) "data/Data_S2_norm.csv" else "data/Data_S2.csv"
path_col  <- if (file.exists("data/Data_S1_norm.csv")) "data/Data_S1_norm.csv" else "data/Data_S1.csv"
path_refs <- if (file.exists("data/Data_S3_norm.csv")) "data/Data_S3_norm.csv" else "data/Data_S3.csv"

occ  <- read_csv(path_occ,  show_col_types = FALSE, progress = FALSE) %>% drop_dot_cols()
col  <- read_csv(path_col,  show_col_types = FALSE, progress = FALSE) %>% drop_dot_cols()
refs <- read_csv(path_refs, show_col_types = FALSE, progress = FALSE) %>% drop_dot_cols()

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
if ("collection_no" %in% names(occ) && "collection_number" %in% names(occ)) {
  occ <- occ %>% mutate(coll_id = dplyr::coalesce(as.character(collection_no),
                                                  as.character(collection_number)))
} else if ("collection_no" %in% names(occ)) {
  occ <- occ %>% mutate(coll_id = as.character(collection_no))
} else if ("collection_number" %in% names(occ)) {
  occ <- occ %>% mutate(coll_id = as.character(collection_number))
} else {
  occ <- occ %>% mutate(coll_id = NA_character_)
}

# ---------- Collections prep ----------
# Robust coll_id
if ("collection_number" %in% names(col) && "collection_no" %in% names(col)) {
  col <- col %>% mutate(coll_id = dplyr::coalesce(as.character(collection_number),
                                                  as.character(collection_no)))
} else if ("collection_number" %in% names(col)) {
  col <- col %>% mutate(coll_id = as.character(collection_number))
} else if ("collection_no" %in% names(col)) {
  col <- col %>% mutate(coll_id = as.character(collection_no))
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

occ$source <- factor(occ$source, levels = source_levels)                 # ok if PBDB_U absent in occ
col$collection_source <- factor(col$collection_source, levels = source_levels)

pal_source <- colorFactor(
  palette = c("#b56a9c", "#037c6e", "#80c7ff"),  # PBDB, Literature, PBDB_U (light blue)
  levels  = source_levels
)


# ---------- choices ----------
epoch_choices_occ   <- us(c(occ$early_epoch,  occ$late_epoch))
period_choices_occ  <- us(c(occ$early_period, occ$late_period))
epoch_choices_col   <- us(c(col$early_epoch,  col$late_epoch))
period_choices_col  <- us(c(col$early_period, col$late_period))

continent_choices_occ  <- us(occ$continent)
paleoocean_choices_occ <- us(occ$paleoocean)
continent_choices_col  <- us(col$continent)
paleoocean_choices_col <- us(col$paleoocean)

order_choices      <- us(occ$order)
superorder_choices <- us(occ$superorder)
family_choices     <- us(occ$family)
rank_choices       <- us(occ$rank)
status_choices     <- us(occ$status)

source_levels_occ      <- source_levels
source_levels_col_all  <- source_levels

# ---------- UI ----------
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
  "FINS app ",
  id = "main_tabs",
  
  # ---- WELCOME / README ----
  tabPanel("Welcome",
           fluidPage(
             h2("FINS -FossIl NeoselachianS- app"),
             p("This app allows users to explore and add data to the FINS dataset, which consists of fossil occurrences, each linked to a collection and reference.  ",
               ),
             tags$hr(),
             fluidRow(
               column(6, wellPanel(
                 h4("About"),
                 uiOutput("readme_ui_about")
               )),
               column(6, wellPanel(
                 h4("Guide to adding new data"),
                 uiOutput("readme_ui_guide")
               )),
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
               helpText("Type any number (e.g., 7.5) or pick a preset. Use “Any” to disable.")
               ,
               
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
               helpText("Type any number (e.g., 10) or pick a preset. Use “Any” to disable."),
               
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

 # ---- ADD DATA -----
  tabPanel("Add data",
           sidebarLayout(
             sidebarPanel(
               h4("Upload PBDB Collections"),
               fileInput("pbdb_col_file", "Choose PBDB Collections CSV",
                         accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
               checkboxGroupInput(
                 "pbdb_col_paleo",
                 "Paleocoordinate and paleoocean addition",
                 choices = c("Compute paleocoordinates (30 replicates)" = "compute"),
                 #choices = c("Compute paleooceans (30 replicates)" = "compute"),
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

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  # Use rv$occ / rv$col / rv$refs everywhere instead of raw occ/col/refs
  rv <- reactiveValues(occ = occ, col = col, refs = refs)
  
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
  
  # Align an uploaded frame to a template (FINS schema): add missing cols as NA and order like template
  align_to_template <- function(df_new, template_df) {
    # add missing columns with NA of an appropriate type (fallback to character)
    for (nm in setdiff(names(template_df), names(df_new))) {
      # try to match NA type from template
      tpl_col <- template_df[[nm]]
      if (is.numeric(tpl_col))      df_new[[nm]] <- NA_real_
      else if (is.integer(tpl_col)) df_new[[nm]] <- NA_integer_
      else if (is.logical(tpl_col)) df_new[[nm]] <- NA
      else                          df_new[[nm]] <- NA_character_
    }
    # keep all uploaded extra columns too — put them after the template columns
    ordered <- c(names(template_df), setdiff(names(df_new), names(template_df)))
    df_new[, ordered, drop = FALSE]
  }
  
  # pick the first existing, non-empty column from a list of candidates
  coalesce_col <- function(df, candidates) {
    for (nm in candidates) {
      if (nm %in% names(df)) return(df[[nm]])
    }
    rep(NA_character_, nrow(df))
  }

  # ---- Welcome ----
  output$welcome_counts <- renderUI({
    n_occ  <- nrow(occ)
    n_col  <- nrow(col)
    n_refs <- nrow(refs)
    HTML(sprintf("
      <ul style='margin-left: -20px;'>
        <li><b>Occurrences</b>: %s</li>
        <li><b>Collections</b>: %s</li>
        <li><b>References</b>: %s</li>
      </ul>",
                 format(n_occ, big.mark=","), format(n_col, big.mark=","), format(n_refs, big.mark=",")))
  })
  output$readme_ui_about <- renderUI({
    readme_path <- "data/README_about.md"
    if (file.exists(readme_path)) {
      HTML(markdown::markdownToHTML(readme_path, fragment.only = TRUE))
    } else {
      tags$div(
        p("Place a ", code("README_about.md"), " file in the app directory to display a project guide here."),
        tags$ul(
          tags$li("Include data sources, curation rules, and a field glossary."),
          tags$li("Document normalization logic for reference keys (S1–S3).")
        )
      )
    }
  })
  
  output$readme_ui_guide <- renderUI({
    readme_path <- "data/README_guide.md"
    if (file.exists(readme_path)) {
      HTML(markdown::markdownToHTML(readme_path, fragment.only = TRUE))
    } else {
      tags$div(
        p("Place a ", code("README_guide.md"), " file in the app directory to display a project guide here."),
      )
    }
  })
  
  # ----- Occurrences -----
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
    if (isTRUE(input$collapse_dups_occ) && nm %in% names(out)) out <- out %>% distinct(.data[[nm]], .keep_all = TRUE)
   # --- Uncertain nomenclature (based on identified_name) ---
      idn <- out$identified_name
    idn_str <- ifelse(is.na(idn), "", idn)
    
    # Detect: aff.  |  cf.  |  ?  |  "  (case-insensitive; perl regex)
    uncertain_pat <- '(?i)\\baff\\.|\\bcf\\.|\\?|"'
    uncertain <- grepl(uncertain_pat, idn_str, perl = TRUE)
    
    # Exclude uncertain if requested
    if ("Exclude" %in% (input$uncertain_mode_occ %||% character(0))) {
      out <- out[!uncertain, , drop = FALSE]
      # Recompute helper vectors after subsetting
      idn <- out$identified_name
      idn_str <- ifelse(is.na(idn), "", idn)
      uncertain <- grepl(uncertain_pat, idn_str, perl = TRUE)
    }
    
    # Flag uncertain if requested (adds a user-facing column)
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
  
  output$map_occ <- renderLeaflet({ leaflet() %>% addTiles() })
  outputOptions(output, "map_occ", suspendWhenHidden = FALSE)
  
  observe({
    pts <- occ_filtered() %>% filter(!is.na(latitude), !is.na(longitude))
    if (nrow(pts) == 0) { leafletProxy("map_occ") %>% clearMarkers() %>% clearControls(); return() }
    leafletProxy("map_occ") %>% clearMarkers() %>% clearControls() %>%
      addCircleMarkers(
        data = pts, lng = ~longitude, lat = ~latitude,
        radius = 4, opacity = 0.9, fillOpacity = 0.6,
        color = ~pal_source(source),
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
    show_cols <- c("collection_number","collection_no","name_display","order","superorder","family","genus","rank","status",
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
  
  # ----- Collections -----
  apply_age_thresh_col <- function(df, choice) {
    if (choice == "any" || !"age_range" %in% names(df)) return(df)
    thr <- as.numeric(choice)
    df %>% filter(!is.na(age_range) & age_range <= thr)
  }
  
  occ_coll_ids_filtered <- reactive({ unique(occ_filtered()$coll_id %||% NA_character_) })
  
  # Pool BEFORE source filter (for dynamic source choices)
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
  
  # Dynamic source choices for Collections (PBDB/Literature that are present)
  observeEvent(list(input$sync_col_with_occ, input$epochs_col, input$periods_col,
                    input$age_thresh_col, input$continent_col, input$paleocean_col), {
                      pool <- col_pool()
                      present <- intersect(source_levels_col_all, us(pool$collection_source))
                      if (length(present) == 0) present <- source_levels_col_all[0]  # none present
                      new_selected <- intersect(input$source_filter_col %||% character(0), present)
                      if (length(new_selected) == 0) new_selected <- present
                      updateCheckboxGroupInput(session, "source_filter_col",
                                               choices = present, selected = new_selected, inline = TRUE)
                    }, ignoreInit = FALSE)
  
  # Apply source filter AFTER dynamic choices
  col_filtered <- reactive({
    out <- col_pool()
    if ("collection_source" %in% names(out) && length(input$source_filter_col)) {
      out <- out %>% filter(collection_source %in% input$source_filter_col)
    }
    out
  })
  
  output$map_col <- renderLeaflet({ leaflet() %>% addTiles() })
  outputOptions(output, "map_col", suspendWhenHidden = FALSE)
  observe({
    pts <- col_filtered() %>% filter(!is.na(latitude), !is.na(longitude))
    if (nrow(pts) == 0) { leafletProxy("map_col") %>% clearMarkers() %>% clearControls(); return() }
    leafletProxy("map_col") %>% clearMarkers() %>% clearControls() %>%
      addCircleMarkers(
        data = pts, lng = ~longitude, lat = ~latitude,
        radius = 5, opacity = 0.9, fillOpacity = 0.6,
        color = ~pal_source(collection_source),
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
  
  # Fossil types (S1) bar
  output$plot_types_col <- renderPlot({
    df <- col_filtered()
    if (!"fossil_type" %in% names(df)) {
      plot.new(); title("Fossil types: 'fossil_type' column not found in Data_S1"); return()
    }
    
    tab <- df %>%
      mutate(fossil_type = ifelse(is.na(fossil_type), "", fossil_type)) %>%
      tidyr::separate_rows(fossil_type, sep = ",") %>%   # ← split on commas
      mutate(
        fossil_type = trimws(fossil_type),
        fossil_type = dplyr::na_if(fossil_type, "")
      ) %>%
      mutate(fossil_type = ifelse(is.na(fossil_type), "Unknown", fossil_type)) %>%
      count(fossil_type, name = "n", sort = TRUE)
    
    if (nrow(tab) == 0) { plot.new(); title("Fossil types: no data"); return() }
    
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
        c(NA, NA, "#e8f4ff")  # only uploaded get light-blue background
      )
    )
  })
  
  # ----- References (linked via reference_key) -----
  # Keys from the current Occurrences / Collections selections
  ref_keys_from_occ <- reactive({
    rf <- occ_filtered()
    if ("reference_key" %in% names(rf)) unique(na.omit(rf$reference_key)) else character(0)
  })
  ref_keys_from_col <- reactive({
    cf <- col_filtered()
    if ("reference_key" %in% names(cf)) unique(na.omit(cf$reference_key)) else character(0)
  })
  
  # Pool of references (sync toggle respected)
  refs_pool <- reactive({
    if (!isTRUE(input$sync_refs_with_occ)) return(refs)
    keys <- union(ref_keys_from_occ(), ref_keys_from_col())
    if (length(keys) == 0 || !"reference_key" %in% names(refs)) return(refs[0, ])
    refs %>% filter(reference_key %in% keys)
  })
  
  # Dynamic language choices
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
  
  # Final filtered refs
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
  
  # ---- Read PBDB CSV with a free-form preamble; auto-detect header row and delimiter.----
  read_pbdb_csv <- function(file, header = TRUE, delim_choice = "Auto") {
    if (is.null(file)) return(NULL)
    path  <- file$datapath
    lines <- readr::read_lines(path, skip_empty_rows = FALSE)
    
    n <- length(lines)
    if (!n) return(data.frame())
    
    # 1) Find the header line by counting known PBDB tokens on each line
    header_tokens <- c("collection_no","collection_number","collection_name",
                       "lng","lat","early_interval","late_interval","max_ma","min_ma","cc","state")
    
    count_tokens <- function(s) {
      s <- tolower(s)
      sum(vapply(header_tokens, function(tok) {
        grepl(paste0("\\b", tok, "\\b"), s)
      }, logical(1)))
    }
    
    token_hits <- vapply(lines, count_tokens, integer(1))
    header_idx <- which(token_hits >= 3)   # need at least 3 distinct known tokens on the line
    header_idx <- if (length(header_idx)) header_idx[1] else NA_integer_
    
    # 2) Fallback: look for an explicit "Records:" marker and use the next non-empty line
    if (is.na(header_idx)) {
      rec_idx <- which(grepl("^\\s*Records\\s*:\\s*$", lines, ignore.case = TRUE))
      if (length(rec_idx)) {
        j <- rec_idx[1] + 1
        while (j <= n && grepl("^\\s*$", lines[j])) j <- j + 1
        header_idx <- if (j <= n) j else NA_integer_
      }
    }
    
    # If still unknown, best-effort: assume first line is header
    if (is.na(header_idx)) header_idx <- 1L
    
    header_line <- lines[header_idx]
    
    # 3) Choose delimiter
    if (!is.null(delim_choice) && !identical(delim_choice, "Auto")) {
      delim <- switch(delim_choice, "," = ",", ";" = ";", "\t" = "\t", NULL)
    } else {
      if (grepl(",", header_line))      delim <- ","
      else if (grepl("\t", header_line)) delim <- "\t"
      else if (grepl(";", header_line))  delim <- ";"
      else                               delim <- ","   # default to CSV
    }
    
    skip_n <- header_idx - 1L
    loc    <- readr::locale(encoding = "UTF-8")
    
    # 4) Read from the detected header line
    if (identical(delim, "\t")) {
      df <- readr::read_tsv(path, skip = skip_n, col_names = header,
                            locale = loc, show_col_types = FALSE, progress = FALSE, guess_max = 10000)
    } else {
      df <- readr::read_delim(path, delim = delim, skip = skip_n, col_names = header,
                              locale = loc, show_col_types = FALSE, progress = FALSE, guess_max = 10000)
    }
    
    # 5) Drop fully-empty rows (in case of trailing blanks)
    if (nrow(df)) {
      empty_row <- apply(df, 1, function(r) all(is.na(r) | r == ""))
      df <- df[!empty_row, , drop = FALSE]
    }
    
    df
  }
  
  
  # Heuristic mojibake repair for common UTF-8/Latin-1 mishaps (H√∂ver -> Höver)
  repair_encoding <- function(x) {
    if (!is.character(x)) return(x)
    # Try Latin-1 reinterpretation round-trip
    x2 <- iconv(iconv(x, from = "UTF-8", to = "latin1", sub = ""), from = "latin1", to = "UTF-8", sub = "")
    # Common artifacts fallback fixes
    repl <- c("√∂"="ö","√©"="é","√±"="ñ","√º"="º","√®"="®","√†"="†","√™"="™","√§"="§",
              "√•"="•","√£"="£","√°"="°","√≠"="≠","√ú"="ú","√å"="å","√Ö"="Ö","√Ä"="Ä")
    for (k in names(repl)) x2 <- gsub(k, repl[[k]], x2, fixed = TRUE)
    x2
  }
  repair_df_encoding <- function(df) {
    df %>% mutate(across(where(is.character), repair_encoding))
  }
  
  ensure_cols <- function(df, cols) {
    for (nm in cols) {
      if (!nm %in% names(df)) df[[nm]] <- NA_character_
    }
    df
  }

  
  # PBDB -> FINS Collections transformer (keeps only the requested columns)
  process_pbdb_collections <- function(df, keep_only_needed = TRUE) {
    names(df) <- tolower(names(df))
    needed <- c("collection_no","formation","lng","lat","collection_name","collection_aka",
                "early_interval","late_interval","max_ma","min_ma","cc","state",
                "latlng_basis","geogcomments")
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
    
    out <- tibble::tibble(
      collection_number   = ifelse(!is.na(df[["collection_no"]]),
                                   paste0("PBDB_", df[["collection_no"]]),
                                   NA_character_),
      formation           = df[["formation"]],
      longitude           = suppressWarnings(as.numeric(df[["lng"]])),
      latitude            = suppressWarnings(as.numeric(df[["lat"]])),
      collection_name     = df[["collection_name"]],
      collection_aka      = df[["collection_aka"]],
      early_interval      = df[["early_interval"]],
      late_interval       = df[["late_interval"]],
      max_ma              = suppressWarnings(as.numeric(df[["max_ma"]])),
      min_ma              = suppressWarnings(as.numeric(df[["min_ma"]])),
      country             = df[["cc"]],
      state               = df[["state"]],
      collection_comments = join_comments(df[["latlng_basis"]], df[["geogcomments"]]),
      collection_source   = factor("PBDB_U", levels = c("PBDB","Literature","PBDB_U"))
    )
    
    if (!keep_only_needed) {
      extras <- df[, setdiff(names(df), names(out)), drop = FALSE]
      extras <- tibble::as_tibble(lapply(extras, function(x) if (is.list(x)) sapply(x, as.character) else x))
      out <- dplyr::bind_cols(out, extras)
    }
    out
  }
  
  # PBDB Occurrences
  
  process_pbdb_occurrences <- function(df, keep_only_needed = TRUE) {
    # normalize header names
    names(df) <- tolower(names(df))
    
    # columns we may reference below (add as NA if missing)
    needed <- c(
      "occurrence_no","occurrence_id",
      "collection_no","collection_number",
      "identified_name","taxon_name","accepted_name",
      "genus","family","order","superorder","rank","status",
      "lng","lat","early_interval","late_interval","max_ma","min_ma", "reference"
    )
    df <- ensure_cols(df, needed)
    
    # Build occurrence_number from occurrence_no (fallback to occurrence_id), always PBDB_ prefixed
    occ_no_raw <- if (!all(is.na(df[["occurrence_no"]]))) df[["occurrence_no"]] else df[["occurrence_id"]]
    occ_no_chr <- suppressWarnings(as.character(occ_no_raw))
    occurrence_number <- ifelse(!is.na(occ_no_chr) & occ_no_chr != "",
                                paste0("PBDB_", occ_no_chr), NA_character_)
    
    # IMPORTANT: Build coll_id STRICTLY from the occurrences file's collection_no (fallback to collection_number)
    colno_raw <- if (!all(is.na(df[["collection_no"]]))) df[["collection_no"]] else df[["collection_number"]]
    colno_chr <- suppressWarnings(as.character(colno_raw))
    coll_id <- ifelse(!is.na(colno_chr) & colno_chr != "",
                      paste0("PBDB_", colno_chr), NA_character_)
  
    
    out <- tibble::tibble(
      occurrence_number = occurrence_number,
      coll_id           = coll_id,                 # <- from collection_no in the occurrences file
      collection_no     = coll_id,
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
  
  
  # ---------- PBDB Collections upload ----------
  pbdb_col_raw <- reactive({
    req(input$pbdb_col_file)
    read_pbdb_csv(input$pbdb_col_file, header = TRUE, delim_choice = "Auto")
  })
  
  pbdb_col_processed <- reactive({
    df_raw <- pbdb_col_raw()
    validate(need(!is.null(df_raw), "Upload a Collections CSV to preview."))
    
    if (isTRUE(input$pbdb_col_fixenc)) {
      df_raw <- repair_df_encoding(df_raw)
    }
    
    # --- normalize names & ensure needed raw cols exist ---
    df <- df_raw
    names(df) <- tolower(names(df))
    needed <- c("collection_no","formation","lng","lat","collection_name","collection_aka",
                "early_interval","late_interval","max_ma","min_ma","cc","state","latlng_basis","geogcomments", "ref_author")
    df <- ensure_cols(df, needed)
    
    # --- helpers ---
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
    
    # late_interval: fill from early_interval if missing
    late_final <- dplyr::coalesce(df[["late_interval"]], df[["early_interval"]])
    m_late_from_early <- is.na(df[["late_interval"]]) & !is.na(df[["early_interval"]])
    
    # collection_number from collection_no
    coll_num <- ifelse(!is.na(df[["collection_no"]]), paste0("PBDB_", df[["collection_no"]]), NA_character_)
    m_collection_number <- !is.na(df[["collection_no"]]) & !is.na(coll_num)
    
    # comments
    comments <- join_comments(df[["latlng_basis"]], df[["geogcomments"]])
    m_collection_comments <- !is.na(comments) & ( (!is.na(df[["latlng_basis"]]) & df[["latlng_basis"]] != "") |
                                                    (!is.na(df[["geogcomments"]]) & df[["geogcomments"]] != "") )
    
    # numeric coercions
    max_ma_num <- suppressWarnings(as.numeric(df[["max_ma"]]))
    min_ma_num <- suppressWarnings(as.numeric(df[["min_ma"]]))
    age_range  <- ifelse(!is.na(max_ma_num) & !is.na(min_ma_num),
                         round(max_ma_num - min_ma_num, 2), NA_real_)
    m_age_range <- !is.na(age_range)
    
    # continent from country code (cc)
    # country: expand ISO2 codes -> full names
    cc_raw   <- df[["cc"]]
    country  <- expand_country_codes(cc_raw)
    m_country <- !is.na(cc_raw) & cc_raw != "" & !is.na(country) & (toupper(trimws(as.character(cc_raw))) != country)
    
    # continent from full country name
    continent <- country_to_continent(country)
    m_continent <- !is.na(country) & country != "" & !is.na(continent)
    
    # latitudinal band from latitude
    lat_num <- suppressWarnings(as.numeric(df[["lat"]]))
    lat_band <- dplyr::case_when(
      is.na(lat_num)                      ~ NA_character_,
      lat_num >=  23.436                  ~ "Northern Temperate",
      lat_num <= -23.436                  ~ "Southern Temperate",
      TRUE                                ~ "Tropical"
    )
    m_lat_band <- !is.na(lat_band)
    
    # interval type from early_interval (keeps 'sub-' if present)
    time_interval_type <- vapply(df[["early_interval"]], classify_interval_type, character(1))
    m_time_type <- !is.na(time_interval_type)
    
    # uploaded source tag
    collection_source <- factor("PBDB_U", levels = c("PBDB","Literature","PBDB_U"))
    m_collection_source <- TRUE  # every uploaded row is tagged
    
    # --- derive epoch/period/era from early_/late_interval ---
    ee <- resolve_interval_hierarchy(df[["early_interval"]])
    le <- resolve_interval_hierarchy(late_final)
    
    # --- paleocoordinates (optional; off by default) ---
    # lat/lon numerics (robust to header variants)
    # lat/lon numerics (robust to header variants)
    lat_num <- suppressWarnings(as.numeric(coalesce_col(df, c("lat", "latitude"))))
    lon_num <- suppressWarnings(as.numeric(coalesce_col(df, c("lng", "lon", "longitude"))))
    
    # --- paleocoordinates (optional; off by default) ---
    paleolon   <- rep(NA_real_, length(lat_num))
    paleolat   <- rep(NA_real_, length(lat_num))
    m_paleo    <- rep(FALSE,     length(lat_num))
    paleo_note <- rep(NA_character_, length(lat_num))   # ← reason per row
    
    do_paleo <- isTRUE("compute" %in% (input$pbdb_col_paleo %||% character(0)))
    
    if (do_paleo) {
      if (!requireNamespace("rgplates", quietly = TRUE)) {
        showNotification("rgplates not available — paleocoordinates skipped.", type = "warning", duration = 8)
      } else {
        withProgress(message = "Reconstructing paleocoordinates…", value = 0, {
          n <- length(lat_num)
          for (i in seq_len(n)) {
            res <- .paleo_mc_median(
              lon    = lon_num[i],
              lat    = lat_num[i],
              min_ma = max_ma_num[i],   # (PBDB uses max/min as older/younger; your code already set these)
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
        
        # Popup summary
        ok <- sum(m_paleo, na.rm = TRUE); n <- length(m_paleo)
        showNotification(sprintf("Paleocoordinates computed for %d/%d rows.", ok, n),
                         type = if (ok > 0) "message" else "warning", duration = 8)
        
        # Show a modal with the top issues (first 15 failures)
        if (ok < n) {
          fail_idx <- which(!m_paleo)
          # pull a useful identifier if available
          id_vec <- coalesce_col(df, c("collection_no", "collection_number", "collection_name"))
          diag_tbl <- data.frame(
            row = fail_idx,
            collection = as.character(id_vec[fail_idx]),
            reason = paleo_note[fail_idx],
            lon = lon_num[fail_idx],
            lat = lat_num[fail_idx],
            min_ma = max_ma_num[fail_idx],
            max_ma = min_ma_num[fail_idx],
            stringsAsFactors = FALSE
          )
          diag_tbl <- head(diag_tbl, 15)
          showModal(modalDialog(
            title = sprintf("Paleocoord diagnostics (%d failed)", length(fail_idx)),
            div(style = "max-height:60vh; overflow:auto;",
                renderTable(diag_tbl, striped = TRUE, bordered = TRUE, spacing = "s")),
            easyClose = TRUE, size = "l"
          ))
        }
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
    

    # --- build output frame ---
    out <- tibble::tibble(
      collection_number   = coll_num,
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
      paleolongitude      = paleolon
    )
    
    # mask columns (same length as out)
    out$m_collection_number    <- m_collection_number
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

    
    out
  })
  
  pbdb_col_aligned <- reactive({
    out <- align_to_template(pbdb_col_processed(), rv$col)
    # ensure mask columns survive align (they won’t be in template, align keeps extras at the end)
    out
  })
  
  output$pbdb_col_preview <- renderDT({
    df <- pbdb_col_aligned()
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    
    # Show up to 100 rows
    show <- head(df, 100)
    
    # Build the table
    dt <- DT::datatable(
      show,
      options = list(
        pageLength = 10, scrollX = TRUE,
        # hide mask columns
        columnDefs = list(
          list(targets = grep("^m_", names(show)) - 1L, visible = FALSE)
        )
      ),
      rownames = FALSE
    )
    
    # Per-cell coloring using valueColumns masks
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
    
    color_cell("collection_number",   "m_collection_number")
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
    
    dt
  })
  
  output$pbdb_col_status <- renderUI({
    tags$div(style="font-size:12px;margin-top:6px;",
             tags$span(style="display:inline-block;width:14px;height:14px;background:#fff7e6;border:1px solid #f2e1cc;margin-right:6px;"),
             "Cells highlighted = filled/derived by the app"
    )
  })
  
  
  output$pbdb_col_status <- renderUI({
    n <- nrow(pbdb_col_aligned())
    ok <- sum(pbdb_col_aligned()[["m_paleocoords"]] %||% FALSE, na.rm = TRUE)
    need_pkg <- !requireNamespace("rgplates", quietly = TRUE)
    HTML(paste0(
      "<div style='font-size:12px;margin-top:6px;'>",
      "<span style='display:inline-block;width:14px;height:14px;background:#fff7e6;border:1px solid #f2e1cc;margin-right:6px;'></span>",
      "Cells highlighted = filled/derived by the app",
      do_paleo <- "compute" %in% (input$pbdb_col_paleo %||% character(0)),
      if (do_paleo)
        sprintf("<br/>Paleocoords: %s/%s rows succeeded.", ok, n) else "",
      if (need_pkg)
        "<br/><span style='color:#a33;'>Note: 'rgplates' not available—paleocoordinates will be NA.</span>" else "",
      "</div>"
    ))
  })
  

  # ---------- PBDB Occurrences upload ----------
  pbdb_occ_raw <- reactive({
    req(input$pbdb_occ_file)
    read_pbdb_csv(input$pbdb_occ_file, header = TRUE, delim_choice = "Auto")
  })
  
  pbdb_occ_processed <- reactive({
    df <- pbdb_occ_raw()
    validate(need(!is.null(df), "Upload an Occurrences CSV to preview."))
    if (isTRUE(input$pbdb_occ_fixenc)) {
      df <- repair_df_encoding(df)
    }
    process_pbdb_occurrences(df, keep_only_needed = TRUE)  # or expose a toggle if you want
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
  
}

shinyApp(ui, server)
