# Packages-----

library(targets)
library(tarchetypes)

# Fonctions-----

source("editeurs/functions.R")

# Options-----

options(tidyverse.quiet = TRUE)

tar_option_set(packages = c("tidyverse", "uwot", "readxl",
                            "RcppHNSW", "rnndescent"))

# Pipeline-----

tar_plan(
  
  ## Files ----
  
  ## Icar ----
  
  barbat2005 = readODS::read_ods("barbat2005_calving_interval.ods") |> 
    janitor::clean_names() |> 
    filter(annee < 2002) |> 
    rename(Montbéliarde = mo,
           Normande = no,
           Holstein = ph,
           year = annee) |>  
    pivot_longer(cols = 2:4,
                 names_to = "breed",
                 values_to = "avg_calving_interval_days"),
  
  fertility = readxl::read_excel("data/5 - Health, Fertility & Longevity – All recorded cows.xlsx") |> 
    janitor::clean_names() |> 
    mutate(breed = if_else(breed == "Prim Holstein" & country == "France",
                           "Holstein",
                           breed)),
  
  yield = readxl::read_excel("data/4.3 - Main breeds - all recorded cows.xlsx") |> 
    janitor::clean_names(),
  
  fertility_1981_2024_plot = fertility |> 
    filter(country == "France",
           breed %in% c("Holstein", "Montbéliarde", "Normande")) |> 
    select(year, breed, avg_calving_interval_days) |> 
    bind_rows(barbat2005) |> 
    ggplot(aes(x = year,
               y = avg_calving_interval_days,
               color = breed)) +
    geom_line(key_glyph = "timeseries") +
    scale_y_continuous(labels = scales::label_glue("{x} jours")) +
    labs(x = "",
         y = "") +
    legend_inside(),
  
  yield_fertility_plot = fertility |> 
    inner_join(yield, by = join_by(country, year, breed)) |> 
    filter(country == "France",
           breed == "Holstein") |> 
    ggplot(aes(x = milk_yield_per_recorded_cow_kg,
               y = avg_calving_interval_days,
               color = year)) +
    geom_point() +
    geom_path() +
    ggrepel::geom_label_repel(aes(label = year)) +
    scale_colour_gradient(low = "#E7F0FF", high = "#035B8F") +
    guides(colour = "none"),
  
  yield_rec_fr_icar = readxl::read_excel("data/4.1 - Milk yield – all recorded cows.xlsx") |>
    janitor::clean_names() |> 
    filter(country == "France") |> 
    select(year,
           milk_yield_per_recorded_cow_kg,
           milk_per_cow_in_305_days_kg),
  
  verrier2010 = tibble(year = c(1970, 1980, 1990, 2000),
                       milk_yield_per_recorded_cow_kg = c(3500, 4500, 6000, 7100)),
  
  yield_rec_fr = bind_rows(verrier2010,
                           yield_rec_fr_icar,
                           .id = "source"),
  
  yield_rec_fr_plot = ggplot(yield_rec_fr,
                             aes(x = year,
                                 y = milk_yield_per_recorded_cow_kg)) +
    geom_line() +
    geom_point(data = yield_rec_fr |> filter(source == 1)) +
    scale_y_continuous(labels = scales::label_glue("{x} kg")) +
    labs(x = "",
         y = "")
  
)
