# Download the Mexico-specific SENASICA New World screwworm case layer
# Source: SENASICA Geovisualizador_GBG, layer 1 ("Indole")
# Outputs:
#   SENASICA_Mexico_NWS_cases.csv
#   SENASICA_Mexico_NWS_cases.geojson
#   SENASICA_Mexico_NWS_wildlife_cases.csv
#   SENASICA_Mexico_NWS_wildlife_species_summary.csv
#
# Required packages: httr2, jsonlite, dplyr, purrr, readr, sf, stringr

pkgs <- c("httr2", "jsonlite", "dplyr", "purrr", "readr", "sf", "stringr")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing)
}

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(sf)
library(stringr)

layer_url <- paste0(
  "https://dj.senasica.gob.mx/mapas/rest/services/GEOMATICA/",
  "Geovisualizador_GBG/MapServer/1"
)
query_url <- paste0(layer_url, "/query")

message("1. Getting all feature IDs from SENASICA...")

id_resp <- request(query_url) |>
  req_url_query(
    where = "1=1",
    returnIdsOnly = "true",
    f = "json"
  ) |>
  req_retry(max_tries = 4) |>
  req_perform()

id_json <- resp_body_json(id_resp, simplifyVector = TRUE)

if (!is.null(id_json$error)) {
  stop("SENASICA returned an error while requesting IDs: ", id_json$error$message)
}

ids <- sort(unique(id_json$objectIds))
if (!length(ids)) stop("No feature IDs were returned by the SENASICA layer.")
message("   Found ", length(ids), " records.")

# Use modest chunks because this layer advertises a 2,000-record maximum
# and does not advertise pagination support.
chunks <- split(ids, ceiling(seq_along(ids) / 500))

fetch_chunk <- function(x, i, n) {
  message("   Downloading chunk ", i, " of ", n, "...")

  r <- request(query_url) |>
    req_url_query(
      objectIds = paste(x, collapse = ","),
      outFields = "*",
      returnGeometry = "false",
      f = "json"
    ) |>
    req_retry(max_tries = 4) |>
    req_perform()

  z <- resp_body_json(r, simplifyVector = FALSE)
  if (!is.null(z$error)) {
    stop("SENASICA returned an error in chunk ", i, ": ", z$error$message)
  }

  if (!length(z$features)) return(tibble())

  # jsonlite::fromJSON on each attributes object keeps inconsistent ArcGIS
  # field encodings from breaking row-binding. Everything starts as character;
  # selected numeric/date fields are converted after all chunks are combined.
  map_dfr(z$features, function(feat) {
    a <- feat$attributes
    tibble::as_tibble_row(lapply(a, function(v) {
      if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)
    }))
  })
}

parts <- map2(chunks, seq_along(chunks), ~fetch_chunk(.x, .y, length(chunks)))
raw <- bind_rows(parts)

message("2. Standardizing fields...")

parse_arcgis_date <- function(x) {
  x <- as.character(x)
  out <- rep(as.Date(NA), length(x))

  # ArcGIS dates are normally epoch milliseconds.
  is_ms <- !is.na(x) & str_detect(x, "^-?[0-9]{11,}$")
  if (any(is_ms)) {
    out[is_ms] <- as.Date(
      as.POSIXct(as.numeric(x[is_ms]) / 1000,
                 origin = "1970-01-01", tz = "UTC")
    )
  }

  # Also tolerate already-formatted date strings if the service changes encoding.
  is_text <- !is.na(x) & !is_ms & nzchar(x)
  if (any(is_text)) {
    suppressWarnings({
      parsed <- as.Date(x[is_text], tryFormats = c(
        "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d"
      ))
    })
    out[is_text] <- parsed
  }
  out
}

num_fields <- intersect(
  c("FID", "POBLACION", "ENFERMOS", "LATITUD", "LONGITUD", "SEMANA", "ANIO"),
  names(raw)
)

date_fields <- intersect(c("FECHA", "notificaci", "validac"), names(raw))

cases <- raw
for (nm in num_fields) cases[[nm]] <- suppressWarnings(as.numeric(cases[[nm]]))
for (nm in date_fields) cases[[nm]] <- parse_arcgis_date(cases[[nm]])

# Preserve useful Spanish field names, but add a few readable aliases at the end.
cases <- cases |>
  mutate(
    source = "SENASICA Geovisualizador_GBG MapServer/1",
    source_url = layer_url
  ) |>
  arrange(FECHA, FID)

message("3. Writing complete Mexico case table...")
write_csv(cases, "SENASICA_Mexico_NWS_cases.csv", na = "")
