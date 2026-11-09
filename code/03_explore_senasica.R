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

#Load data (edit to match your file path)
data_dir <- "G:/.shortcut-targets-by-id/1sBr0mNtmtJnFm2funklbTepJgKsmnYie/New World Screwworm/datasets"

glw_raw <- read_csv(file.path(data_dir, "GLW2020_ADM2_merged.csv"))
nws_raw <- read_csv(file.path(data_dir, "SENASICA_NWS_2022_2026_with_unit_id.csv"))

#Remove empty rows
glw_clean <- glw_raw |> 
  select(-c(state,adm_level,unit_id)) |> 
  rename(
    country = country_iso3,
    adm2 = unit_name
  )



#Standardizing Data

#Standardize and translate column names, then tidy the remaining names
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
  
# Check 1: expect 12 host groups, each with a suscep_ and cases_ column
names(nws_std) |> str_subset("^(suscep|cases)_")

# Check 2: host labels should match between the two sets
hosts_susc  <- names(nws_std) |> str_subset("^suscep_") |> str_remove("^suscep_")
hosts_casos <- names(nws_std) |> str_subset("^cases_")  |> str_remove("^cases_")
setdiff(hosts_susc, hosts_casos)   # character(0)
setdiff(hosts_casos, hosts_susc)   # character(0)

# Check 3: do host columns add up to the total column?
nws_std |>
  mutate(host_sum = rowSums(across(starts_with("cases_"), \(x) replace_na(x, 0))),
         diff     = host_sum - tot_cases) |>
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

nws_long <- nws_long |> mutate(host = unname(host_labels[host]))


#Figures
nws_mex <- nws_long |> 
  group_by(country) |> 
  summarize(individuals = sum(tot_cases))

# 1. Cases by country ---------------------------------------------------
nws_long |>
  group_by(country) |>                                           
  summarize(total_cases = sum(tot_cases)) |>
  ggplot(aes(reorder(country, total_cases), total_cases)) +
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
