# Download the complete SENASICA New World screwworm regional outbreak layer
# Source: Focos de GBG en Mexico y Centroamerica (2022-2026)

base_url <- paste0(
  "https://dj.senasica.gob.mx/mapas/rest/services/GEOMATICA/",
  "Focos_GBG_2022_2026/MapServer/0/query"
)

# Packages
pkgs <- c("httr2", "jsonlite", "sf", "dplyr", "readr")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

library(httr2)
library(jsonlite)
library(sf)
library(dplyr)
library(readr)

# 1. Get all feature IDs. ArcGIS allows returnIdsOnly without the 2000-record cap.
id_resp <- request(base_url) |>
  req_url_query(
    where = "1=1",
    returnIdsOnly = "true",
    f = "json"
  ) |>
  req_perform()

id_json <- resp_body_json(id_resp, simplifyVector = TRUE)
ids <- id_json$objectIds

if (is.null(ids) || length(ids) == 0) stop("No records returned from SENASICA.")
message("Records found: ", length(ids))

# 2. Download records in chunks to stay below the service limit.
chunks <- split(ids, ceiling(seq_along(ids) / 1000))

fetch_chunk <- function(x) {
  resp <- request(base_url) |>
    req_url_query(
      objectIds = paste(x, collapse = ","),
      outFields = "*",
      returnGeometry = "true",
      outSR = "4326",
      f = "geojson"
    ) |>
    req_perform()

  txt <- resp_body_string(resp)
  dat <- st_read(txt, quiet = TRUE)

  # SENASICA returns Fecha_Inic with inconsistent types across chunks.
  # Convert it to character before binding so bind_rows() can combine all chunks.
  if ("Fecha_Inic" %in% names(dat)) {
    dat$Fecha_Inic <- as.character(dat$Fecha_Inic)
  }

  dat
}

nws <- bind_rows(lapply(chunks, fetch_chunk))

# 3. Standardize Fecha_Inic after all chunks have been combined.
# Keep the raw value so the source formatting can be checked later if needed.
if ("Fecha_Inic" %in% names(nws)) {
  nws <- nws |>
    mutate(
      Fecha_Inic_raw = Fecha_Inic,
      Fecha_Inic = case_when(
        # ArcGIS millisecond timestamps
        grepl("^[0-9]{11,}$", Fecha_Inic) ~ as.character(
          as.Date(
            as.POSIXct(
              as.numeric(Fecha_Inic) / 1000,
              origin = "1970-01-01",
              tz = "UTC"
            )
          )
        ),
        TRUE ~ Fecha_Inic
      ),
      Fecha_Inic = as.Date(Fecha_Inic)
    )
}

# 4. Save a spatial GeoJSON and a flat CSV with longitude/latitude.
st_write(nws, "SENASICA_NWS_2022_2026.geojson", delete_dsn = TRUE, quiet = TRUE)

coords <- st_coordinates(nws)
nws_csv <- nws |>
  st_drop_geometry() |>
  mutate(longitude_geometry = coords[, 1], latitude_geometry = coords[, 2])

write_csv(nws_csv, "SENASICA_NWS_2022_2026.csv", na = "")

message("Saved:")
message("  SENASICA_NWS_2022_2026.geojson")
message("  SENASICA_NWS_2022_2026.csv")
