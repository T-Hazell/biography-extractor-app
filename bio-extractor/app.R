# This Shiny application is designed to scrape, format, and check biographies of akims (regional governors in Kazakhstan).
# It extracts relevant sections from the provided URLs and formats them using fine-tuned OpenAI models.
#
# Libraries:
# - shiny: For building the web application.
# - rvest: For web scraping.
# - tidyverse: For data manipulation.
# - chromote: For controlling Chrome browser.
# - curl: For making HTTP requests.
#
# Functions:
# - LinkToCareerText: Extracts the main content from a given URL.
# - CheckForYandexHeaderTell: Checks if the content is from Yandex cache.
# - AkimNameFinder: Extracts the name of the akim from the content.
# - MainTextExtractor: Extracts the main text content from the input.
# - CareerTextExtractor: Extracts the career section from the input.
# - BasicInformationTextExtractor: Extracts the basic information and education section from the input.
# - BirthDateExtractor: Extracts the birth date from the input.
# - BirthDateTextExtractor: Extracts the birth date text from the input.
# - ATDFromLink: Extracts the administrative-territorial division from the URL.
# - ATDFromLink2: Extracts the administrative-territorial division from the URL (less computationally intensive).
# - GPTBiographyPrompter: Sends the input to the OpenAI API and returns the formatted response.
#
# Prompts:
# - system_prompt_work: Instructions for splitting biographies into individual positions.
# - system_prompt_study: Instructions for splitting biographies into individual educational experiences.
#
# UI:
# - A sidebar for input fields and buttons.
# - A main panel for displaying the formatted biography table and raw GPT responses.
#
# Server:
# - Handles URL submission, scraping, and extraction of relevant sections.
# - Sends the extracted sections to the OpenAI API for formatting.
# - Displays the formatted biography table and raw GPT responses.

#' TODO:
#' - Fine-tune model on birth place
#' - Fine-tune model on military service
#' - Link cleaning before checks
#' - Add a 'loading' spinner
#' - Add a 'copy to clipboard' button
#' - Server-side improvements
#' - Error handling of dead urls :(

# Preamble ----

library(shiny)
library(rvest)
library(tidyverse)
library(chromote)
library(curl)
library(reticulate)

# These rows are required for running on a deployed shiny server :)
# Sys.setenv(CHROMOTE_CHROME = "/srv/shiny-server/bio-extractor/chrome/linux-129.0.6668.70/chrome-linux64/chrome")
#
# chromote::set_default_chromote_object(
#     chromote::Chromote$new(chromote::Chrome$new(
#         args = c(
#             "--disable-gpu",
#             "--no-sandbox",
#             "--disable-dev-shm-usage", # required bc the target easily crashes
#             c("--force-color-profile", "srgb")
#         )
#     ))
# )

# Read the API key from a file
api_key <- read.table("bio-extractor/api_key.txt") |>
    as.character()

# Read in subdivision name dictionaries
cleaned_subdiv_dictionary <- read_csv("bio-extractor/dictionaries/cleaned_subdiv_dictionary.csv", show_col_types = FALSE)
cleaned_comb_dictionary <- read_csv("bio-extractor/dictionaries/cleaned_rayon_dictionary.csv", show_col_types = FALSE)

# Functions ----

# Import to_case from Python
use_virtualenv("r-reticulate")
source_python("bio-extractor/to_case.py")

# Define the LinkToCareerText function
#' Extracts and returns the HTML content from a given link.
#'
#' This function takes a URL link as input, reads the HTML content of the page,
#' and extracts specific elements based on the URL pattern. It handles different
#' cases for URLs from "gov.kz" and "yandexwebcache.net", and a general case for
#' other URLs. The extracted HTML content is returned as a single character string.
#'
#' @param link A character string representing the URL of the page to extract content from.
#' @return A character string containing the extracted HTML content.
LinkToCareerText <- function(link) {
    page <- link |>
        read_html_live()

    # Wait, or rvest stops
    Sys.sleep(2)

    # TODO: Clean string for conditionals

    # Main gov.kz case
    if (str_starts(link, "https://www.gov.kz/")) {
        html_content <- page |>
            rvest::html_element("div") |>
            rvest::html_elements(xpath = "//main")

        # Yandex case
    } else if (str_starts(link, "https://yandexwebcache.net/")) {
        html_content <- page |>
            rvest::html_elements("div")

        # Other cases
    } else {
        html_content <- page |>
            rvest::html_element("main")
    }

    html_content <- html_content |>
        as.character() |>
        paste0(collapse = "") #  Ensures that element*s* are not returned as a list
    #' This code is horrible-ish, but rvest does not (consistently) preserve the
    #' structure of the nodeset, and this is one way to keep it.
    #' Read back in with rvest::read_html().

    # Return the content
    return(html_content)
}

# Define the CheckForYandexHeaderTell function
#' Check for Yandex Cache Header
#'
#' This function checks if the first `div` element in the provided HTML input
#' has an `id` attribute with the value "yandex-cache-hdr". It is used to
#' determine if the HTML content is from a Yandex cache.
#'
#' @param input A character string containing the HTML content to be checked.
#'
#' @return A logical value indicating whether the first `div` element has an
#' `id` attribute equal to "yandex-cache-hdr". Returns `TRUE` if it does,
#' `FALSE` otherwise.
#'
#' @import rvest
CheckForYandexHeaderTell <- function(input) {
    # Scrape the first div element
    first_div <- input |>
        rvest::read_html() |>
        rvest::html_element("div")

    # Check if the id attribute of the first div is "yandex-cache-hdr"
    id_attr <- rvest::html_attr(first_div, "id")

    if (is.na(id_attr)) {
        id_attr <- FALSE
    }

    return(!is.null(first_div) && !is.na(id_attr) && id_attr == "yandex-cache-hdr")
}

# Define the AkimNameFinder function
#' AkimNameFinder
#'
#' This function extracts a name from the provided input HTML content. It handles two different
#' types of HTML structures based on the presence of a Yandex header.
#' - If the input is not empty and does not equal "No biography found":
#'   - If the input contains a Yandex header, it extracts the name from the `meta` tag with the
#'     property `og:title`.
#'   - Otherwise, it extracts the name from the first `h2` element.
#'   - If the extracted name is `NULL` or does not contain exactly three words, it returns "No name found".
#' - If the input is empty or equals "No biography found", it returns "No name found".
#'
#' @param input A character string containing HTML content.
#'
#' @return A character string representing the extracted name. If no name is found or the input
#' is invalid, it returns "No name found".
AkimNameFinder <- function(input) {
    if (length(input) != 0 && input != "No biography found") {
        if (CheckForYandexHeaderTell(input)) {
            text_output <- input |>
                rvest::read_html() |>
                rvest::html_element("div[style='position:relative']") |>
                rvest::html_element("meta[property='og:title']") |>
                rvest::html_attr("content")
        } else {
            text_output <- input |>
                rvest::read_html() |>
                rvest::html_element("h2") |>
                rvest::html_text()

            if (is.null(text_output) || str_count(text_output, "\\S+") != 3) {
                text_output <- "No name found"
            }
        }
    } else {
        text_output <- "No name found"
    }

    return(text_output)
}

# Define the MainTextExtractor function
#' MainTextExtractor
#'
#' This function extracts the main text content from an HTML input. It checks for the
#' presence of a Yandex header andprocesses the input accordingly.
#'
#' @param input A character string containing the HTML content to be processed.
#' @return A character string containing the extracted text.
MainTextExtractor <- function(input) {
    if (CheckForYandexHeaderTell(input)) {
        text_output <- input |>
            rvest::read_html() |>
            rvest::html_element("div[style='position:relative']") |>
            rvest::html_text() |>
            paste(collapse = "\n")
    } else {
        text_output <- input |>
            rvest::read_html() |>
            rvest::html_elements(xpath = "//main") |>
            rvest::html_text() |>
            paste(collapse = "\n")
    }

    return(text_output)
}

# Define the CareerTextExtractor function
#' CareerTextExtractor
#'
#' This function extracts career-related text from a given input string.
#' It first checks if the input is not empty and does not contain the phrase "No biography found".
#' If the input is valid, it uses the `MainTextExtractor` function to process the input.
#' Then, it searches for the keyword "Карьера" (which means "Career" in Russian) in the processed text.
#' If the keyword is found, it extracts the text starting from "Карьера" to the end.
#' If the input is invalid, it returns `NA_character_`.
#'
#' @param input A character string containing the text to be processed.
#' @return A character string containing the extracted career-related text, or `NA_character_` if the input is invalid.
CareerTextExtractor <- function(input) {
    if (length(input) != 0 && input != "No biography found") {
        text_output <- MainTextExtractor(input)

        if (stringr::str_detect(text_output, "Карьера")) {
            text_output <- stringr::str_extract(text_output,
                pattern = "Карьера\\s(?s).*"
            )
        }
        return(text_output)
    } else {
        return(NA_character_)
    }
}

# Define the BasicInformationTextExtractor function
#' Extract Basic Information Text from Input
#'
#' This function extracts the basic information text from the given input string.
#' It checks if the input is not empty and does not contain "No biography found".
#' If the input is valid, it extracts the main text and searches for specific
#' patterns indicating the presence of "Карьера" and "Общая информация, образование".
#' If both patterns are found, it extracts the text between these patterns.
#'
#' @param input A character vector containing the input text.
#' @return A character vector containing the extracted basic information text,
#' or `NA_character_` if the input is invalid or the patterns are not found.
BasicInformationTextExtractor <- function(input) {
    if (length(input) != 0 && input != "No biography found") {
        text_output <- MainTextExtractor(input)

        if (stringr::str_detect(text_output, "Карьера") & stringr::str_detect(text_output, "Общая информация, образование")) {
            BI_text <- stringr::str_extract(text_output,
                pattern = "Общая информация, образование\\s(?s).*(?=Карьера)"
            )

            return(BI_text)
        } else {
            return(NA_character_)
        }
    } else {
        return(NA_character_)
    }
}

# Define the BirthDateExtractor function
#' Extracts the birth date from a given text input.
#'
#' This function searches for specific birthday-related terms in the input text
#' and attempts to extract and format the birth date if found. It expects plain text input.
#' If the input is in HTML format, run the MainTextExtractor first to convert it to plain text.
#'
#' @param input A character string containing the text from which to extract the birth date.
#' @return A character string representing the formatted birth date in "dd.mm.yyyy" format,
#'         the raw date match if parsing fails, or NA if no date is found or input is invalid.
BirthDateExtractor <- function(input) {
    if (length(input) != 0 && input != "No biography found") {
        #' Expects plain text input, rather than html
        #' Run MainTextExtractor first if needed
        birthday_terms <- c("Дата рождения", "родился", "родилась", "родился в", "родилась в")
        combined_pattern <- paste(birthday_terms, collapse = "|")

        if (stringr::str_detect(input, regex(combined_pattern, ignore_case = TRUE))) {
            date_match <- stringr::str_extract(input,
                pattern = regex(paste(paste0("(?<=", birthday_terms, "\\s)\\d{1,2}\\s\\w{1,10}\\s\\d{4}"), collapse = "|"),
                    ignore_case = TRUE
                )
            )

            parsed_date <- tryCatch(
                {
                    parse_date_time(date_match, orders = "d B Y", locale = "ru_RU")
                },
                error = function(e) {
                    return(NA_character_)
                }
            )

            if (!is.na(parsed_date)) {
                formatted_date <- format(parsed_date, "%d.%m.%Y")
                return(formatted_date)
            } else {
                return(date_match)
            }
        } else {
            return(NA_character_)
        }
    } else {
        return(NA_character_)
    }
}

# Define the BirthDateTextExtractor function
#' BirthDateTextExtractor
#'
#' This function extracts birth date information from a given input string.
#' It searches for specific birthday-related terms and patterns within the input text.
#'
#' @param input A character string containing the text to be searched for birth date information.
#' @return A character string containing the extracted birth date information if found, otherwise NA.
BirthDateTextExtractor <- function(input) {
    birthday_terms <- c("Дата рождения", "родился", "родилась", "родился в", "родилась в")
    combined_pattern <- paste(birthday_terms, collapse = "|")
    combined_pattern_god <- paste(paste0(birthday_terms, "\\s\\d{1,2}\\s\\w{1,10}\\s\\d{4}\\sгод\\w*[^.]*\\."), collapse = "|")

    if (stringr::str_detect(input, regex(combined_pattern, ignore_case = TRUE))) {
        input <- stringr::str_replace_all(input, "(?<=с|г)\\.", "+") # Hacky as
        text_output <- stringr::str_extract(input, regex(combined_pattern_god, ignore_case = TRUE))
        text_output <- stringr::str_replace_all(text_output, "(?<=с|г)\\+", "\\.") # Hacky as part 2
        return(text_output)
    } else {
        return(NA_character_)
    }
}

#' Extract Territorial Division (ATD) From Link
#'
#' This function extracts the oblast, rayon, and subdistrict information
#' from a given gov.kz link.
#'
#' @param link A character string representing the URL from which to extract
#' the territorial division information.
#'
#' @return A list containing the extracted oblast, rayon, and subdistrict information.
#' The list has the following structure:
#' \describe{
#'   \item{oblast}{The name of the oblast (region).}
#'   \item{rayon}{The name of the rayon (district).}
#'   \item{subdistrict}{The name of the subdistrict (if applicable).}
#' }
#'
#' @details
#' The function first constructs a list of distinct oblast names
#' from a pre-defined dictionary. It then checks if the provided
#' URL follows the expected pattern for gov.kz links. If the URL
#' matches the pattern, the function extracts the entity name
#' from the URL and determines whether it follows a three-part or
#' two-part pattern. Based on the pattern, it extracts the oblast,
#' rayon, and subdistrict names as they appear (in Latin script) in
#' the url. The function also looks up the full names of the rayon
#' and oblast from a dictionary.
# Define the ATDFromLink function
ATDFromLink <- function(link) {
    # List of distinct oblast url names (x -- e.g., almobl)
    oblast_list1 <- cleaned_comb_dictionary |>
        distinct(embedded_oblast) |>
        filter(!is.na(embedded_oblast) &
            embedded_oblast != "sauran" &
            embedded_oblast != "akimat")

    oblast_list <- cleaned_comb_dictionary |>
        distinct(oblast_name) |>
        rename(embedded_oblast = oblast_name) |>
        bind_rows(oblast_list1) |>
        distinct(embedded_oblast) |>
        pull(embedded_oblast)

    # Manually add kostanai to the list - it's not in the dictionary but sometimes appears
    oblast_list <- c(oblast_list, "kostanai")

    # Initialize the output list
    tp_split <- list(
        "oblast" = NA_character_,
        "rayon" = NA_character_,
        "subdistrict" = NA_character_
    )

    # Check the url follows the expected pattern for gov.kz links
    if (stringr::str_detect(link, "gov.kz")) {
        # If it does, extract the entity name from the url (x-x or x-x-x)
        entity <- stringr::str_extract(
            link,
            pattern = "(?<=https://www.gov.kz/memleket/entities/).*?(?=/about/structure/)"
        )

        # If the entity is not empty
        if (!is.na(entity)) {
            # Initialize the oblast and rayon variables
            rayon <- NA_character_
            oblast <- NA_character_

            # Check if the entity follows three-part or two-part pattern
            if (stringr::str_detect(entity, "\\w+-\\w+-\\w+")) {
                # Three part pattern case
                three_phase <- stringr::str_replace_all(entity, pattern = "-", " ")

                # If x-x-x, strings are the oblast, rayon, and subdistrict respectively
                oblast <- stringr::str_split_i(three_phase, pattern = " ", 1)
                rayon <- stringr::str_split_i(three_phase, pattern = " ", 2)
                subdistrict <- stringr::str_split_i(three_phase, pattern = " ", 3)

                # Store the extracted values in the output list
                tp_split <- list("oblast" = oblast, "rayon" = rayon, "subdistrict" = subdistrict)
            } else if (stringr::str_detect(entity, "\\w+-\\w+")) {
                # Two part pattern case
                two_phase <- stringr::str_replace_all(entity, pattern = "-", " ")

                # If x-x, strings are [typically] the oblast and rayon respectively
                if (stringr::str_split_i(two_phase, pattern = " ", 1) %in% oblast_list) {
                    oblast <- stringr::str_split_i(two_phase, pattern = " ", 1)
                    rayon <- stringr::str_split_i(two_phase, pattern = " ", 2)
                    subdistrict <- NA_character_

                    # Store the extracted values in the output list
                    tp_split <- list("oblast" = oblast, "rayon" = rayon, "subdistrict" = NA_character_)
                }
            }

            # If the extracted values are not empty
            if (!is.na(rayon) && !is.na(oblast)) {
                # Filter the dictionary to match on rayon grouped by oblast
                dictionary_row <- filter(cleaned_comb_dictionary, embedded_rayon == rayon & oblast_name == oblast)

                # Extract the full name of the rayon
                rayon_proper <- dictionary_row |>
                    pull(rayon_name_proper)

                # Record it if it's not empty
                if (length(rayon_proper) != 0) {
                    tp_split$rayon <- rayon_proper
                }

                # Extract the full name of the oblast
                oblast_proper <- dictionary_row |>
                    pull(oblast_name_proper)

                # Record it if it's not empty
                if (length(oblast_proper) != 0) {
                    tp_split$oblast <- oblast_proper
                }
            }
        }
    }

    # Return the extracted values
    return(tp_split)
}

#' Extract Administrative-Territorial Division (ATD) Names from a gov.kz Link
#'
#' This function extracts the names of administrative-territorial divisions
#' (oblast, rayon, subdistrict) from a given gov.kz link. It is a less
#' computationally-intensive version of another function, `ATDFromLink`, and
#' invokes `ATDFromLink` only if this function leaves any fields empty.
#'
#' @param link A character string representing the URL to be processed.
#'
#' @return A list containing the names of the oblast, rayon, and subdistrict
#' extracted from the link. If the extraction is unsuccessful, the respective
#' fields will contain `NA_character_`.
#'
#' @details The function first checks if the URL follows the expected pattern
#' for gov.kz links. It then attempts to extract the entity name from the URL
#' and matches it against a pre-cleaned subdivision dictionary. If a match is
#' found, the function extracts the oblast, rayon, and subdistrict names. If
#' the entity is not found or if further refinement is needed, the function
#' attempts to match on a more specific department code. If any fields remain
#' empty, the function invokes `ATDFromLink` to fill in the missing values.
#'
#' @examples
#' \dontrun{
#' link <- "https://www.gov.kz/memleket/entities/some-entity/about/structure/departments/leadership/12345"
#' atd_names <- ATDFromLink2(link)
#' print(atd_names)
#' }
ATDFromLink2 <- function(link) {
    # Initialize the output list
    atd_names <- list("oblast" = NA_character_, "rayon" = NA_character_, "subdistrict" = NA_character_)

    # Check if the URL follows the expected pattern for gov.kz links
    if (stringr::str_detect(link, "gov.kz")) {
        # Extract the entity name from the URL (x-x or x-x-x)
        entity <- stringr::str_extract(
            link,
            pattern = "(?<=www.gov.kz/memleket/entities/).*?(?=/about/)"
        )

        # If the entity is not empty
        if (!is.na(entity)) {
            # Filter the dictionary to match on the entity text
            results <- cleaned_subdiv_dictionary |>
                filter(entity_text == entity) |>
                #' Take only one result; If n_{entity name} > 1, a
                #' later step finds the exact subdivision name
                slice(1)

            # If the entity is not found in the dictionary
            if (nrow(results) == 0) {
                # Apply the ATDFromLink function
                atd_names <- ATDFromLink(link)
            } else if (pull(results, n) == 1) {
                # If n = 1, extract the oblast, rayon, and subdistrict names
                atd_names$oblast <- results$oblast_name_proper
                atd_names$rayon <- results$rayon_name_proper
                atd_names$subdistrict <- results$subdiv_name_proper
                # If n > 1, try to find the exact subdivision name using a more specific link
            } else {
                # In particular, we try and match on a 4/5 digit department code
                number_code_link <- str_extract(link, "about/structure/departments/leadership/\\d+/.+$")
                number_code_link <- str_remove(number_code_link, "\\?lang.*|%3Flang.*")

                results_code <- cleaned_subdiv_dictionary |>
                    filter(str_detect(base_url, number_code_link))

                # If that works, extract the oblast, rayon, and subdistrict names
                if (nrow(results_code == 1)) {
                    atd_names$oblast <- results_code$oblast_name_proper
                    atd_names$rayon <- results_code$rayon_name_proper
                    atd_names$subdistrict <- results_code$subdiv_name_proper
                    #' If that doesn't help, extract the oblast and rayon names and
                    #' leave the subdistrict empty
                } else {
                    atd_names$oblast <- results$oblast_name_proper
                    atd_names$rayon <- results$rayon_name_proper
                    atd_names$subdistrict <- NA_character_
                }
            }
        }

        # If rayon or subdistrict is empty, apply the ATDFromLink function
        if (is.na(atd_names$rayon) || is.na(atd_names$subdistrict) && nrow(results) != 0) {
            atd_names_check <- ATDFromLink(link)

            # If subdistrict is missing and ATDFromLink finds it, record it
            if (!is.na(atd_names_check$subdistrict) && is.na(atd_names$subdistrict)) {
                atd_names$subdistrict <- atd_names_check$subdistrict
            }

            # If rayon is missing and ATDFromLink finds it, record it
            if (!is.na(atd_names_check$rayon) && is.na(atd_names$rayon)) {
                atd_names$rayon <- atd_names_check$rayon

                # If subdistrict is not missing, remove the rayon name from it
                if (!is.na(atd_names$subdistrict)) {
                    r_for_removal <- to_case_sentence(atd_names_check$rayon, case = "gent")
                    atd_names$subdistrict <- str_squish(str_remove(atd_names$subdistrict, r_for_removal))
                }
            }

            # If oblast is missing and ATDFromLink finds it, record it
            if (!is.na(atd_names_check$oblast) && is.na(atd_names$oblast)) {
                atd_names$oblast <- atd_names_check$oblast
            }
        }
    }

    # Return the extracted values
    return(atd_names)
}

# Define the GPTBiographyPrompter function
#' This function sends a prompt to the OpenAI API and retrieves a response.
#' It handles rate limiting by checking the number of remaining requests and tokens,
#' and waits if they are below a certain threshold.
#'
#' @param prompt A string containing the user's prompt to be sent to the API.
#' @param system A string containing the system's instructions for the API.
#' @param model A string specifying the model type (e.g., "gpt-3.5-turbo", "gpt-4o").
#' @return A string containing the content of the response from the OpenAI API.
GPTBiographyPrompter <- function(prompt, system, model) {
    # Response is the raw JSON returned by the API
    response <- httr::RETRY(
        "POST",
        # Relevant input URL
        url = "https://api.openai.com/v1/chat/completions",
        # Authorization key
        httr::add_headers(
            Authorization = paste(
                "Bearer",
                # Private API key follows
                api_key
            )
        ),
        # Content is JSON
        httr::content_type_json(),
        encode = "json",
        body = list(
            # Function takes the model type (gpt-3.5-turbo, gpt-4, ....)
            model = model,
            # How random the result is [0, 2]
            temperature = 1,
            # Instructions to the server
            messages = list(
                list(role = "system", content = system),
                list(role = "user", content = prompt)
            )
        ),
        times = 3 # Number of exponential retries
    )

    # Check openAI's response for the number of remaining requests/tokes plus time until reset
    rate_limit_remaining_requests <- httr::headers(response)$`x-ratelimit-remaining-requests`
    rate_limit_remaining_tokens <- httr::headers(response)$`x-ratelimit-remaining-tokens`
    rate_limit_reset_requests <- httr::headers(response)$`x-ratelimit-reset-requests`
    rate_limit_reset_tokens <- httr::headers(response)$`x-ratelimit-reset-tokens`

    # Note and wait if they are low
    if (as.numeric(rate_limit_remaining_requests) < 10) {
        warning(
            stringr::str_glue(
                "Warning! Only {rate_limit_remaining_requests} requests left for {model}. {rate_limit_reset_requests} until reset"
            )
        )
        Sys.sleep(10)
    }

    if (as.numeric(rate_limit_remaining_tokens) < 5000) {
        warning(
            stringr::str_glue(
                "Warning! Only {rate_limit_remaining_tokens} tokens left for {model}. {rate_limit_reset_tokens} until reset"
            )
        )
        Sys.sleep(10)
    }

    # Extract the message from openAI's response
    httr::content(response)$choices[[1]]$message$content
}

# GPT model set-up ----

system_prompt_work <- 'You receive biographies and split them into individual positions. Return the split biographies in the following csv format:
start_date, end_date, place_name, organisation, position, full_original_text
"dd.mm.yyyy", "dd.mm.yyyy", "string", "string", "string", "string"
"dd.mm.yyyy", "dd.mm.yyyy", "string", "string", "string", "string"

For missing data, return "NA". If the month is not clear, dates should be "yyyy". For place_name, organisation, and position, return the original text. full_original_text is the full entry for that position.
Do not geuss place if it is not explicitly stated.'

system_prompt_study <- 'You receive biographies and split them into individual educational experiences. Only consider study at higher education institutions, not any work experience. Return the split biographies in the following csv format:
start_date, end_date, place_name, organisation, position, full_original_text
"dd.mm.yyyy", "dd.mm.yyyy", "string", "string", "string", "string"
"dd.mm.yyyy", "dd.mm.yyyy", "string", "string", "string", "string"

For missing data, return "NA". For place_name and organisation return the original text. For position, return "Студент", with the speciality in brackets, if given. full_original_text is the full entry for that position.
Do not geuss place if it is not explicitly stated.'

# UI set-up ----

# Instructions for the user
instructions <-
    "<small>
<p>
    This app is built to make the task of scraping, formatting, and checking akims' biographies easier. To use it:
</p>

<ol>
    <li>
        <strong>[Optional]</strong> Paste in the web address of a biography on <strong>gov.kz</strong> or <strong>yandexwebcache.net</strong>
        (for other links, it may fail to extract text or split it into sections) and click 'submit url'. The app scrapes the page and
        attempts to separate the name, career, and education sections for submission to the correct formatting model. It tries to
        autofill the relevant section with the text of these sections. This usually takes about 10 seconds. If the biography does not
        match the typical patterns of a biography page, it inserts all the text into the top input box and you'll have to seperate it manually. It uses a dictionary of administrative-territorial divisions to try to extract the correct names of the subdivisions.
    </li>

    <li>
        Insert/check the biography and <strong>[optional]</strong> akim name into the input boxes, and click submit. Wait a little bit while our
        fine-tuned openAI models try to format the biography. The 'career' box is sent to the model trained on work entries. The 'basic
        info/education' box sends it to the model trained on study entries. If all goes well, the app reads in the model's response as a CSV
        and presents it as a table, which can just be copy and pasted into Google Sheets. Please check the results!
    </li>
</ol>
<p>
    <strong>Future tasks:</strong> Fine-tune/include models for birth place, military service. Improve career extraction fine-tuning.
</p>
</small>
"

# Shiny app ----

# Define UI for the Shiny app
ui <- fluidPage(
    # Page title
    titlePanel("Biography scraping and formatting interface"),
    # Sidebar, with instructions and inpiut boxes
    sidebarLayout(
        sidebarPanel(
            htmlOutput("instructions"),
            textInput("url", "Enter URL:", value = ""),
            actionButton("url_submit", "Submit URL"),
            textInput("akim_subdistrict", "Subdistrict (с.о., п.о., села и т.д.)", value = ""),
            textInput("akim_rayon", "Rayon (Район или ГОЗ)", value = ""),
            textInput("akim_oblast", "Oblast (Область или ГРЗ)", value = ""),
            textInput("akim_name_input", "Name", value = ""),
            textInput("akim_birth_date_input", "Birth date", value = ""),
            textAreaInput("biography_input", "Enter career (Карьера) section of biography:", value = "", rows = 20),
            textAreaInput("basicinfo_input", "Enter basic information/education (Общая информация, образование) section of biography:", value = "", rows = 15),
            actionButton("bio_submit", "Submit biography")
        ),
        # Main panel, to display outputs (formatted biography table and raw GPT responses)
        mainPanel(
            tableOutput("gpt_table"),
            htmlOutput("gpt_response")
        )
    )
)
# Define server logic for the Shiny app
server <- function(input, output, session) {
    # Create reactive variables
    akim_name_reactive <- reactiveVal(NULL)

    akim_subdistrict_reactive <- reactiveVal(NULL)
    akim_rayon_reactive <- reactiveVal(NULL)
    akim_oblast_reactive <- reactiveVal(NULL)

    akim_birth_date_reactive <- reactiveVal(NULL)
    akim_birth_date_text_reactive <- reactiveVal(NULL)

    # Immediately render instructions
    output$instructions <- renderUI({
        HTML(instructions)
    })

    # When the user submits a url:
    observeEvent(input$url_submit, {
        # Check there is actually a url in the input before proceeding
        req(input$url)
        # Attempt to store the url's html content
        html_content <- tryCatch(
            {
                LinkToCareerText(input$url)
            },
            # Return "No biography found" if the scraping fails
            error = function(e) {
                return("No biography found")
            }
        )

        # Extract the 'career' section, or the whole biography if no career section is found
        biography <- CareerTextExtractor(html_content)
        #  If there's nothing at all, return "No biography found"
        if (is.na(biography)) {
            biography <- "No biography found"
        }

        # Initialise the basic info section as an empty string
        basic_info <- ""

        # If the biography contains a career section, try to extract basic info
        if (!is.na(biography) && str_detect(biography, "Карьера")) {
            basic_info <- BasicInformationTextExtractor(html_content)

            # If nothing is found, keep basic_info as an empty string
            if (is.na(basic_info)) {
                basic_info <- ""
            }
        }
        # Extract the akim's name
        akim_name <- AkimNameFinder(html_content)

        # Update the reactive variable
        akim_name_reactive(akim_name)

        atd <- ATDFromLink2(input$url)

        if (is.na(atd$subdistrict)) {
            atd$subdistrict <- ""
        }

        if (is.na(atd$rayon)) {
            atd$rayon <- ""
        }

        if (is.na(atd$oblast)) {
            atd$oblast <- ""
        }

        akim_subdistrict <- atd$subdistrict
        akim_rayon <- atd$rayon
        akim_oblast <- atd$oblast

        akim_subdistrict_reactive(akim_subdistrict)
        akim_rayon_reactive(akim_rayon)
        akim_oblast_reactive(akim_oblast)

        # Extract the akim's birth date
        if (basic_info != "") {
            # Run the functions on the basic info section, if it exists
            akim_birth_date <- BirthDateExtractor(basic_info)
            akim_birth_date_text <- BirthDateTextExtractor(basic_info)

            # Update the reactive variables
            akim_birth_date_reactive(akim_birth_date)
            akim_birth_date_text_reactive(akim_birth_date_text)
        } else {
            # If it doesn't exist, run them on the biography section
            akim_birth_date <- BirthDateExtractor(biography)
            akim_birth_date_text <- BirthDateTextExtractor(biography)

            # Update the reactive variables
            akim_birth_date_reactive(akim_birth_date)
            akim_birth_date_text_reactive(akim_birth_date_text)
        }

        # Update the input boxes with the extracted text
        updateTextAreaInput(session, "biography_input", value = biography)
        updateTextAreaInput(session, "basicinfo_input", value = basic_info)
        updateTextAreaInput(session, "akim_name_input", value = akim_name)
        updateTextAreaInput(session, "akim_subdistrict", value = akim_subdistrict)
        updateTextAreaInput(session, "akim_rayon", value = akim_rayon)
        updateTextAreaInput(session, "akim_oblast", value = akim_oblast)
        updateTextAreaInput(session, "akim_birth_date_input", value = akim_birth_date)
    })

    # When the user submits a biography:
    observeEvent(input$bio_submit, {
        # Check there is actually a biography in the input before proceeding
        req(input$biography_input)

        #  Send the career section to the work model
        gpt_response_work <- GPTBiographyPrompter(input$biography_input,
            system = system_prompt_work,
            model = "ft:gpt-4o-2024-08-06:personal:corrected-pilot-bio-work2:ABKpSsvl"
        )

        # If the basic info section is not empty, send it to the study model
        if (nzchar(input$basicinfo_input)) {
            gpt_response_study <- GPTBiographyPrompter(input$basicinfo_input,
                system = system_prompt_study,
                model = "ft:gpt-4o-2024-08-06:personal:pilot-bio-study:ADC2E7TU"
            )
        } else {
            # If it is empty, set the response to NA
            gpt_response_study <- NA_character_
        }

        # Try to read the career model responses as a csv
        gpt_table <- tryCatch(
            {
                read_csv(gpt_response_work, show_col_types = FALSE) # Quiet
            },
            error = function(e) {
                # If that fails, return an error message
                return(data.frame(Error = "Invalid CSV format returned for work section."))
            }
        )

        # Add an 'event' column to the table, set it to work for the career section
        gpt_table <- gpt_table |>
            mutate(event = "work", .before = 1) |>
            mutate_all(as.character)

        # Try there was a study section, try to read the study model responses as a csv
        if (!is.na(gpt_response_study)) {
            gpt_table_study <- tryCatch(
                {
                    read_csv(gpt_response_study, show_col_types = FALSE)
                },
                error = function(e) {
                    return(data.frame(Error = "Invalid CSV format returned for study section."))
                }
            )

            # Add an 'event' column to the table, set it to study for the study section
            gpt_table_study <- gpt_table_study |>
                mutate(event = "study", .before = 1) |>
                mutate_all(as.character)

            # Add the study section to the main table
            gpt_table <- bind_rows(gpt_table_study, gpt_table)
        }

        # If there is not yet a name in the name reactive variable, set it to the input
        if (is.null(akim_name_reactive())) {
            akim_name_reactive(input$akim_name_input)
        }

        # If there is a name in the reactive variable, store it
        akim_name <- akim_name_reactive()

        # If there is not yet a birth date in the reactive variable, set it to the input
        akim_birth_date <- input$akim_birth_date_input

        # If there is birth date original text in the reactive variable, store it
        akim_birth_date_text <- akim_birth_date_text_reactive()

        # If not, set it to NA
        if (is.null(akim_birth_date_text)) {
            akim_birth_date_text <- NA_character_
        }

        if (is.null(akim_subdistrict_reactive())) {
            akim_subdistrict_reactive(input$akim_subdistrict)
        }
        akim_subdistrict <- akim_subdistrict_reactive()

        if (is.null(akim_rayon_reactive())) {
            akim_rayon_reactive(input$akim_rayon)
        }
        akim_rayon <- akim_rayon_reactive()

        if (is.null(akim_oblast_reactive())) {
            akim_oblast_reactive(input$akim_oblast)
        }
        akim_oblast <- akim_oblast_reactive()

        # If there is a birth date, add it to the table
        if (nrow(gpt_table) > 0 && !is.null(akim_birth_date) && akim_birth_date != "") {
            birth_date_table <- tibble::tibble(
                start_date = akim_birth_date,
                event = "born",
                end_date = NA_character_, # Explicit column order
                place_name = NA_character_,
                organisation = NA_character_,
                position = NA_character_,
                full_original_text = akim_birth_date_text
            ) |>
                mutate_all(as.character)

            gpt_table <- bind_rows(birth_date_table, gpt_table)
        }


        gpt_table <- gpt_table |>
            mutate(FIO = akim_name, .before = 1) |>
            mutate(oblast = akim_oblast, .before = 1) |>
            mutate(rayon = akim_rayon, .before = 1) |>
            mutate(subdistrict = akim_subdistrict, .before = 1)



        # If there is a name, add it to the table as a column
        # if (nrow(gpt_table) > 0 && !is.null(akim_name) && length(akim_name) > 0) {
        #     gpt_table <- gpt_table |>
        #         mutate(FIO = akim_name, .before = 1)
        # } else if (nrow(gpt_table) > 0 && (is.null(akim_name) || length(akim_name) == 0)) {
        #     # If there is no name, use an empty string for the column
        #     gpt_table <- gpt_table |>
        #         mutate(FIO = "", .before = 1)
        # }

        # Render the table and the raw GPT responses
        output$gpt_table <- renderTable(gpt_table)

        output$gpt_response <- renderUI({
            HTML(paste0(
                "<pre>", gpt_response_work, "</pre>", # Render the GPT responses in their original
                "<pre>", gpt_response_study, "</pre>"
            ))
        })
    })
}

# Run the application
shinyApp(ui = ui, server = server)
