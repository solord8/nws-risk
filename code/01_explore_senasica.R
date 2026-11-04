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
nws <- read_csv(file.path(data_dir, "SENASICA_NWS_2022_2026.csv"))

#Standardizing Data

std_names <- function(x) {
  lx   <- str_to_lower(x)
  type <- case_when(
    str_starts(lx, "sus")  ~ "suscep",
    str_starts(lx, "caso") ~ "casos",
    TRUE                   ~ NA_character_
  )
  stem <- str_remove(lx, "^(suscep|susc|sus|casos|caso)_?")
  host <- if_else(str_starts(stem, "aves"), str_sub(stem, 1, 5), str_sub(stem, 1, 4))
  if_else(is.na(type), x, str_c(type, "_", host))
}

# Standardize host columns first, then tidy the remaining names
nws_std <- nws |>
  rename_with(std_names) |>
  janitor::clean_names()

# Check 1: expect 12 host groups, each with a suscep_ and casos_ column
names(nws_std) |> str_subset("^(suscep|casos)_")

# Check 2: host labels should match between the two sets
hosts_susc  <- names(nws_std) |> str_subset("^suscep_") |> str_remove("^suscep_")
hosts_casos <- names(nws_std) |> str_subset("^casos_")  |> str_remove("^casos_")

# Check 3: do host columns add up to the total column?
nws_std |>
  mutate(host_sum = rowSums(across(starts_with("casos_"), \(x) replace_na(x, 0))),
         diff     = host_sum - total_caso) |>
  filter(diff != 0)

# Reshape (tot_suscep and total_caso don't match the pattern, so they're excluded)
nws_long <- nws_std |>
  pivot_longer(
    cols = matches("^(suscep|casos)_"),
    names_to = c(".value", "host"),
    names_pattern = "^(suscep|casos)_(.*)$"
  ) |>
  mutate(across(c(suscep, casos), \(x) replace_na(x, 0)))

# Check 4: these should match
sum(nws_long$casos)
sum(nws_std$total_caso, na.rm = TRUE)

host_labels <- c(cani = "Canine", equi = "Equine", porc = "Swine", bovi = "Bovine",
                 ovin = "Ovine", capr = "Caprine", feli = "Feline", bufa = "Buffalo",
                 cone = "Rabbit", silv = "Wildlife", avesc = "Livestock Birds",
                 avess = "Wild Birds")

nws_long <- nws_long |> mutate(host = unname(host_labels[host]))

#Figures

# 1. Cases by country ---------------------------------------------------
nws_long |>
  group_by(pais) |>                                           
  summarize(cases = sum(casos)) |>
  ggplot(aes(reorder(pais, cases), cases)) +
  geom_col() + coord_flip() +
  labs(x = NULL, y = "Cases")
ggsave(here("output/figures/cases_by_country.png"), width = 7, height = 5)

# 2. Cases by host (all countries) --------------------------------------
nws_long |>
  group_by(host) |>
  summarize(cases = sum(casos)) |>
  ggplot(aes(reorder(host, cases), cases)) +
  geom_col() + coord_flip() +
  labs(x = NULL, y = "Cases")
ggsave(here("output/figures/cases_by_host.png"), width = 7, height = 5)

# 3. Cases by host within each country ----------------------------------
nws_long |>
  group_by(pais, host) |>
  summarize(cases = sum(casos), .groups = "drop") |>
  ggplot(aes(reorder(host, cases), cases)) +
  geom_col() + coord_flip() +
  facet_wrap(~pais, scales = "free") +
  labs(x = NULL, y = "Cases")
ggsave(here("output/figures/cases_by_host_by_country.png"), width = 10, height = 7)
