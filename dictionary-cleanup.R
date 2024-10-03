# Preamble ----
library(shiny)
library(rvest)
library(tidyverse)
library(chromote)
library(curl)
library(reticulate)

# Import data
oblast_structure_urls <- readr::read_csv("oblast_structure_urls.csv", show_col_types = FALSE)
rayon_structure_urls <- readr::read_csv("rayon_structure_urls.csv", show_col_types = FALSE)

# Functions ----

# Import to_case from Python
use_virtualenv("r-reticulate")
source_python("bio-extractor/to_case.py")

# Oblast cleanup ----

cleaned_oblast_dictionary <- oblast_structure_urls |>
    rowwise() |>
    mutate(
        entity_name = stringr::str_remove_all(entity_name, "Акимат"),
        entity_name =
            if_else(
                stringr::str_detect(entity_name, "город"),
                to_case_sentence(entity_name, "masc"),
                to_case_sentence(entity_name, "femn")
            ),
        entity_name = if_else(entity_name == "Атырауска область", "Атырауская область", entity_name),
        oblast_name_proper = stringr::str_squish(entity_name),
        oblast_text_r = entity_text
    ) |>
    ungroup()

oblasts_for_rayons <- cleaned_oblast_dictionary |>
    select(oblast_name_proper, oblast_text_r)

# Rayon cleanup ----

cleaned_comb_dictionary <- rayon_structure_urls |>
    rowwise() |>
    left_join(oblasts_for_rayons, by = c("oblast_name" = "oblast_text_r")) |>
    mutate(
        entity_name = stringr::str_remove(
            entity_name,
            if_else(oblast_name_proper == "Атырауская область",
                "Атырауской области",
                to_case_sentence(oblast_name_proper,
                    gender = "femn", case = "gent"
                )
            )
        ),
        entity_name = stringr::str_remove_all(
            entity_name,
            "Акимат|Аппарат акима"
        ),
        entity_name = stringr::str_replace(
            entity_name,
            "города", "город"
        ),
        entity_name = if_else(
            stringr::str_detect(entity_name, "город|\\bрайон\\s[:graph:]+"),
            entity_name,
            to_case_sentence(entity_name, "masc")
        ),
        rayon_name_proper = stringr::str_squish(entity_name),
        embedded_oblast = stringr::str_extract(entity_text, "\\b\\w+(?=-)"),
        embedded_rayon = stringr::str_remove(entity_text, embedded_oblast),
        embedded_rayon = stringr::str_remove(embedded_rayon, "-"),
    ) |>
    select(-c(entity_name, structure_url))

readr::write_csv(cleaned_comb_dictionary, "cleaned_comb_dictionary.csv")
