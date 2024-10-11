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
subdiv_structure_urls <- readr::read_csv("subdiv_structure_urls.csv", show_col_types = FALSE)

# Functions ----

# Import to_case from Python
#' Requires a Python environment with the `pymorphy2` library installed.
use_virtualenv("r-reticulate")
source_python("bio-extractor/to_case.py")

#' This includes two functions, `to_case` and `to_case_sentence`. Both
#' take a string (of one word and multiple, respectively) as well as the
#' arguments `case` (default = nomn) and `gender` (default = masc).
#' The functions return the original string in the new case and gender.

# Oblast cleanup ----

#' Clean the list of oblasts and their URLs, removing non-ATD name elements
#' and putting the oblast names in the nominative case.
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

# Select the relavant columns for adding oblast names to the rayon dictionary
oblasts_for_rayons <- cleaned_oblast_dictionary |>
    select(oblast_name_proper, oblast_text_r)

# Rayon cleanup ----

#' Clean the list of rayons and their URLs, removing non-ATD name elements etc
cleaned_comb_dictionary <- rayon_structure_urls |>
    # Using the to_case_sentence function requires rowwise
    rowwise() |>
    # Add proper oblast names by joining based on entity x
    left_join(oblasts_for_rayons, by = c("oblast_name" = "oblast_text_r")) |>
    mutate(
        # Remove the oblast name from the entity name
        entity_name = stringr::str_remove(
            entity_name,
            if_else(oblast_name_proper == "Атырауская область",
                "Атырауской области",
                to_case_sentence(oblast_name_proper,
                    gender = "femn", case = "gent"
                )
            )
        ),
        # Removes the words "Акимат" and "Аппарат акима" from the entity name
        entity_name = stringr::str_remove_all(
            entity_name,
            "Акимат|Аппарат акима"
        ),
        # Removes the word "город" from the entity name
        entity_name = stringr::str_replace(
            entity_name,
            "города", "город"
        ),
        # Puts the entity name in the nominative case
        entity_name = if_else(
            # Ensuring that the entity name is not already in the nominative
            stringr::str_detect(entity_name, "город|\\bрайон\\s[:graph:]+"),
            entity_name,
            to_case_sentence(entity_name, "masc")
        ),
        #  Rename and clean the entity_name column to rayon_name_proper
        rayon_name_proper = stringr::str_squish(entity_name),
        # Relevant parts of the entity string
        embedded_oblast = stringr::str_extract(entity_text, "\\b\\w+(?=-)"),
        embedded_rayon = stringr::str_remove(entity_text, embedded_oblast),
        embedded_rayon = stringr::str_remove(embedded_rayon, "-"),
    ) |>
    # Remove unused columns
    select(-c(entity_name, structure_url))

# Write the cleaned rayon dictionary to a CSV
readr::write_csv(cleaned_comb_dictionary, "bio-extractor/dictionaries/cleaned_rayon_dictionary.csv")

# Select the relevant columns for adding rayon names to the subdiv dictionary
rayons_for_subdivs <- cleaned_comb_dictionary |>
    select(entity_text, rayon_name_proper, oblast_name_proper)

# Subdiv cleanup ----
subdiv_obl_rayon <- subdiv_structure_urls |>
    mutate(
        #' In some cases, the rayon name is the same as the entity text because of
        #' problems with the scraping. This tries to correct this before it reuslts
        #' in NAs during the merge by removing the last part of the entity x-x[-x]
        #' structure
        rayon_name = case_when(
            rayon_name == entity_text ~ str_remove(rayon_name, "-[:alpha:]+$"),
            .default = rayon_name
        )
    ) |>
    # Merge the rayon names into the subdiv dictionary by joining on entity x-x
    left_join(rayons_for_subdivs, by = c("rayon_name" = "entity_text")) |>
    # Add an ID column for merging after NA handling
    mutate(id = row_number())

# Split out the subdivisions that have no oblast name and try to find one
subdiv_no_obl_rayon <- subdiv_obl_rayon |>
    filter(is.na(oblast_name_proper)) |>
    select(-oblast_name_proper) |>
    # Extract the oblast name from the entity [x]-x-x
    mutate(
        obl_entity_text = str_extract(entity_text, "^\\w+(?=-)"),
        # Correct some oblast names to match the dictionary format
        obl_entity_text = case_when(
            obl_entity_text == "vko" ~ "akimvko",
            obl_entity_text == "zhetysu" ~ "zhetysu-oblysy",
            obl_entity_text == "kostanai" ~ "kostanay",
            obl_entity_text == "turkistan" ~ "ontustik",
            obl_entity_text == "kzh" ~ "sko",
            obl_entity_text == "enbekshikazakh" ~ "almobl",
            obl_entity_text == "balkhash" ~ "almobl",
            obl_entity_text == "ile" ~ "almobl",
            obl_entity_text == "karasay" ~ "almobl",
            obl_entity_text == "kegen" ~ "almobl",
            obl_entity_text == "raiymbek" ~ "almobl",
            obl_entity_text == "talgar" ~ "almobl",
            obl_entity_text == "uigur" ~ "almobl",
            obl_entity_text == "qonaev" ~ "almobl",
            obl_entity_text == "akimat" ~ "atyrau",
            .default = obl_entity_text
        )
    ) |>
    # Join with the dictionary by the oblast entity string
    left_join(
        oblasts_for_rayons,
        by = join_by(obl_entity_text == oblast_text_r)
    ) |>
    # Filter out the rows with oblasts that were still not found
    filter(!is.na(oblast_name_proper)) |>
    select(-obl_entity_text)

# Remerge and clean strings
cleaned_subdiv_dictionary <- subdiv_obl_rayon |>
    # Update with newly found oblasts
    rows_update(
        subdiv_no_obl_rayon,
        by = "id" # By the ID column
    ) |>
    # to_case_sentence requires rowwise
    rowwise() |>
    mutate(
        # Clean the entity/subdivision titles
        entity_name = stringr::str_remove_all(entity_name, "Акимат"),
        entity_name = stringr::str_remove_all(entity_name, regex("Аппарат акима",
            ignore_case = TRUE
        )),
        entity_name = stringr::str_remove_all(entity_name, "ГУ"),
        entity_name = stringr::str_remove_all(entity_name, "\""),
        entity_name = stringr::str_remove_all(entity_name, "Государственное учреждение"),
        entity_name = stringr::str_remove_all(entity_name, "«|»"),
        entity_name = stringr::str_remove_all(entity_name, "^К\\s"),
        # If there is not already a rayon name, try to extract one from the entity name
        rayon_name_proper = case_when(
            is.na(rayon_name_proper) ~
                # Take any words between "сельского округа" and "района" and assume it is the rayon name
                if_else(str_detect(entity_name, "(?<=сельского округа\\s)\\w+(?=\\sрайона)"),
                    paste(
                        # Put it in the nominative case
                        to_case_sentence(
                            str_extract(entity_name, "(?<=сельского округа\\s)\\w+(?=\\sрайона)")
                        ), "район", # And add rayon to the end
                        collapse = " "
                    ), NA_character_ # If there is no match, return NA
                ),
            .default = rayon_name_proper # If there is already a rayon name, return it
        ),
        entity_name = case_when(
            # If there are rayon/oblast names, remove it from the entity name
            !is.na(rayon_name_proper) & !is.na(oblast_name_proper) ~
                # Put oblast/rayon names in the appropriate gender of the genitive before removing
                case_when(
                    # Put oblast names in the femine genitive before removing
                    str_detect(oblast_name_proper, "область") ~ str_remove_all(
                        entity_name,
                        paste0(
                            to_case_sentence(rayon_name_proper, case = "gent"),
                            "|", to_case_sentence(oblast_name_proper, gender = "femn", case = "gent")
                        )
                    ),
                    # And gorods in the masculine genitive
                    str_detect(oblast_name_proper, "город") ~ str_remove_all(
                        entity_name,
                        paste0(
                            to_case_sentence(rayon_name_proper, case = "gent"),
                            "|", to_case_sentence(oblast_name_proper, case = "gent")
                        )
                    ),
                ),
            # If there is only an oblast name, put it in the genitive and remove it
            is.na(rayon_name_proper) & !is.na(oblast_name_proper) ~
                case_when(
                    str_detect(oblast_name_proper, "область") ~ str_remove_all(
                        entity_name,
                        to_case_sentence(oblast_name_proper, gender = "femn", case = "gent")
                    ),
                    str_detect(oblast_name_proper, "город") ~ str_remove_all(
                        entity_name,
                        to_case_sentence(oblast_name_proper, case = "gent")
                    ),
                    .default = entity_name
                ),
            .default = entity_name
        ),
        # Manual corrections
        entity_name = stringr::str_remove_all(entity_name, regex("Восточно-Казахстанской области|Северо-Казахстанской области",
            ignore_case = TRUE
        )),
        # Convert the entity name to the nominative case
        entity_name = case_when(
            str_detect(entity_name, "сельского округа|город|с.о.|поселка") ~
                to_case_sentence(entity_name, "masc"), # Masculine entities
            str_detect(entity_name, "села") ~
                to_case_sentence(entity_name, "neut"), # Neuter entities
            .default = entity_name
        ),
        # Final cleaning
        entity_name = str_squish(entity_name),
        rayon_name_proper = str_squish(rayon_name_proper),
        oblast_name_proper = str_squish(oblast_name_proper)
    ) |>
    # Select the relevant columns and rename entity_name to subdiv_name_proper
    select(entity_text, base_url, subdiv_name_proper = entity_name, rayon_name_proper, oblast_name_proper) |>
    # Count the number of times each entity is represented
    group_by(entity_text) |>
    mutate(n = n()) |>
    #' Where there is n > 1, in the usage stage check and try the full
    #' depth of the URL to see if there is a better match. If there is,
    #' then use that. If not, then use the first match
    #' (with no subdiv_name_proper).
    ungroup()

# Write the cleaned subdiv dictionary to a CSV
readr::write_csv(cleaned_subdiv_dictionary, "bio-extractor/dictionaries/cleaned_subdiv_dictionary.csv")
