# ==============================================================================
# Title: E
# Author: Daniel Solorzano-Jones
# Last Updated: 11/03/26
# Description: Create figures for SENASICA NWS data
# ==============================================================================

#Install required packages
pkgs <- c("tidyverse", "here","janitor")
install.packages(pkgs[!pkgs %in% installed.packages()[, "Package"]])

#Load packages
library(tidyverse)
library(here)
library(janitor)

#Load raw data (edit to match your file path)
data_dir <- "G:/.shortcut-targets-by-id/1sBr0mNtmtJnFm2funklbTepJgKsmnYie/New World Screwworm/datasets"

glw_raw <- read_csv(file.path(data_dir, "GLW2020_ADM2_merged.csv"))
nws_raw <- read_csv(file.path(data_dir, "SENASICA_NWS_2022_2026_with_unit_id.csv"))

#Standardizing Data

#Remove empty columns in glw
glw_clean <- glw_raw |> 
  select(-c(state,adm_level)) |> 
  rename(
    country = country_iso3,
    adm2 = unit_name
  )

#Standardize nws and translate column names, then tidy the remaining names
std_names <- function(x) {
  lx   <- str_to_lower(x)
  type <- case_when(
    str_starts(lx, "sus")  ~ "suscep",
    str_starts(lx, "caso") ~ "cases",
    TRUE                   ~ NA_character_
  )
  stem <- str_remove(lx, "^(suscep|susc|sus|casos|caso)_?")
  host <- if_else(str_starts(stem, "aves"), str_sub(stem, 1, 5), str_sub(stem, 1, 4))
  if_else(is.na(type), x, str_c(type, "_", host))
}

nws_std <- nws_raw |>
  select(-c(Abbrev, ID, fecha_mapa,Fecha_Inic_raw)) |>
  rename_with(std_names) |>
  janitor::clean_names()  |> 
  rename(
    country = pais,
    adm1 = provincia,
    adm2 = localidad,
    lat = latitud,
    long = longitud,
    date = fecha_inic,
    month_year = mapa_omsa,
    tot_cases = total_caso,
    total = suma,
    year = ano
  )

#nws QA/QC
nws_std |> 
  filter(lat == 0 | long == 0)

#Find unit_ids in nws that are NOT in glw
not_in_glw <- nws_std |> 
  anti_join(glw_clean, by = "unit_id")
  nrow(not_in_glw)
  
#Find unit_ids in glw that are NOT in nws
not_in_nws <- glw_clean |> 
  anti_join(nws_std, by = "unit_id")
  nrow(not_in_nws)

#glw row uniqueness 
glw_clean |> 
  count(unit_id, species) |> 
  filter(n > 1)
  
# Check 1: expect 12 host groups, each with a suscep_ and cases_ column
names(nws_std) |> str_subset("^(suscep|cases)_")

# Check 2: host labels should match between the two sets
hosts_susc  <- names(nws_std) |> str_subset("^suscep_") |> str_remove("^suscep_")
hosts_casos <- names(nws_std) |> str_subset("^cases_")  |> str_remove("^cases_")
setdiff(hosts_susc, hosts_casos)   # character(0)
setdiff(hosts_casos, hosts_susc)   # character(0)

# Check 3: do host columns add up to the total column?
anyNA(nws_std$tot_cases)
nws_std |>
  mutate(host_sum = rowSums(across(starts_with("cases_"), \(x) replace_na(x, 0))),
         diff     = host_sum - tot_cases) |>
  filter(diff != 0)

anyNA(nws_std$tot_suscep)
nws_std |>
  mutate(host_sum = rowSums(across(starts_with("suscep_"), \(x) replace_na(x, 0))),
         diff     = host_sum - tot_suscep) |>
  filter(diff != 0)

# Reshape (tot_suscep and tot_cases don't match the pattern, so they're excluded)
nws_long <- nws_std |>
  pivot_longer(
    cols = matches("^(suscep|cases)_"),
    names_to = c(".value", "host"),
    names_pattern = "^(suscep|cases)_(.*)$"
  ) |>
  mutate(across(c(suscep, cases), \(x) replace_na(x, 0)))

# Check 4: these should match
sum(nws_long$cases)
sum(nws_std$tot_cases, na.rm = TRUE)

#Label hosts
host_labels <- c(cani = "Canine", equi = "Equine", porc = "Swine", bovi = "Bovine",
                 ovin = "Ovine", capr = "Caprine", feli = "Feline", bufa = "Buffalo",
                 cone = "Rabbit", silv = "Wildlife", avesc = "Livestock Birds",
                 avess = "Wild Birds")

#Check for unmapped hosts
sum(is.na(nws_long$host))

nws_long <- nws_long |> mutate(host = unname(host_labels[host]))

#Sanity checks
nws_std |> 
  group_by(country) |> 
  summarize(individuals = sum(tot_cases))
nws_long |> 
  group_by(host) |> 
  summarize(affected_indivs = sum(cases))

#Figures

# 1. Cases by country ---------------------------------------------------
nws_long |>
  group_by(country) |>                                           
  summarize(cases = sum(cases)) |>
  ggplot(aes(reorder(country, cases), cases)) +
  geom_col() + coord_flip() +
  labs(x = NULL, y = "Cases")
ggsave(here("output/figures/cases_by_country.png"), width = 7, height = 5)

# 2. Cases by host (all countries) --------------------------------------
nws_long |>
  group_by(host) |>
  summarize(cases = sum(cases)) |>
  ggplot(aes(reorder(host, cases), cases)) +
  geom_col() + coord_flip() +
  labs(x = NULL, y = "Cases")
ggsave(here("output/figures/cases_by_host.png"), width = 7, height = 5)

# 3. Cases by host within each country ----------------------------------
nws_long |>
  group_by(country, host) |>
  summarize(cases = sum(cases), .groups = "drop") |>
  ggplot(aes(reorder(host, cases), cases)) +
  geom_col() + coord_flip() +
  facet_wrap(~country, scales = "free") +
  labs(x = NULL, y = "Cases")
ggsave(here("output/figures/cases_by_host_by_country.png"), width = 10, height = 7)

# 4. Cases per susceptible animal, by host --------------------
# Only meaningful if Suscep = animals at risk.
nws_long |>
  group_by(host) |>
  summarize(rate = sum(cases) / sum(suscep)) |>
  ggplot(aes(reorder(host, rate), rate)) +
  geom_col() + coord_flip() +
  labs(x = NULL, y = "Cases per susceptible animal")
ggsave(here("output/figures/cases_per_suscept_animal_by_host.png"), width = 7, height = 5)

# 5. Host preference index --------------------

# Crosswalk: GLW species -> NWS host labels
glw_xwalk <- c(cattle = "Bovine", buffaloes = "Buffalo", sheep = "Ovine",
               goats = "Caprine", pigs = "Swine", chickens = "Livestock Birds")

# Top 3 countries by total cases (one row per outbreak table)
top3 <- nws_std |>
  group_by(country) |>
  summarize(total_cases = sum(tot_cases)) |>
  slice_max(total_cases, n = 3, with_ties = FALSE) |>
  pull(country)

#Use: % of cases by host, within country (matched hosts only
use_df <- nws_long |>
  filter(country %in% top3, host %in% unname(glw_xwalk)) |>
  group_by(country, host) |>
  summarize(cases = sum(cases), .groups = "drop") |>
  group_by(country) |>
  mutate(use_pct = 100 * cases / sum(cases)) |>
  ungroup()

#Availability: % of livestock by host, in units that had outbreaks
unit_country <- nws_std |>
  filter(!is.na(unit_id)) |>
  distinct(unit_id, country)            # one row per unit, so no duplication

avail_df <- glw_clean |>
  select(-country) |>                   # drop ISO3 to avoid country.x / country.y
  inner_join(unit_country, by = "unit_id") |>
  filter(country %in% top3) |>
  mutate(host = unname(glw_xwalk[species])) |>
  group_by(country, host) |>
  summarize(abundance = sum(total_count, na.rm = TRUE), .groups = "drop") |>
  group_by(country) |>
  mutate(avail_pct = 100 * abundance / sum(abundance)) |>
  ungroup()

#Combine, compute preference index, plot
pref_df <- use_df |>
  inner_join(avail_df, by = c("country", "host")) |>
  mutate(pref_index = use_pct / avail_pct)

ggplot(pref_df, aes(avail_pct, use_pct, label = host)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(shape = 1, size = 3) +
  geom_text(vjust = -0.8, size = 3, check_overlap = TRUE) +
  facet_wrap(~country) +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(x = "Host availability (% of livestock in outbreak units)",
       y = "Host use (% of cases)") +
  theme_bw()
ggsave(here("output/figures/host_preference_top3.png"), width = 10, height = 4)