#' Biography Extractor Shiny App
#'
#' This Shiny application extracts and processes biographies from given URLs or text inputs.
#' It uses various functions to parse HTML content, extract career information, and interact
#' with the OpenAI API to format the extracted data.
#'
#' @file app.R
#' @library shiny
#' @library rvest
#' @library tidyverse
#' @library curl
#' @library chromote
#'
#' @section Functions:
#' \describe{
#'   \item{\code{CareerTextExtractor(input)}}{
#'     Extracts career-related text from the given HTML input.
#'     @param input HTML content as a character string.
#'     @return Extracted career text or "No biography found".
#'   }
#'   \item{\code{AkimNameFinder(input)}}{
#'     Extracts the Akim name from the given HTML input.
#'     @param input HTML content as a character string.
#'     @return Extracted Akim name or "No name found".
#'   }
#'   \item{\code{LinkToCareerText(link)}}{
#'     Fetches and processes the HTML content from the given URL.
#'     @param link URL as a character string.
#'     @return HTML content as a character string.
#'   }
#'   \item{\code{GPTBiographyPrompter(prompt, model)}}{
#'     Sends a prompt to the OpenAI API and retrieves the response.
#'     @param prompt The prompt to send to the API.
#'     @param model The model type to use (e.g., gpt-3.5-turbo, gpt-4).
#'     @return The content of the API response.
#'   }
#' }
#'
#' @section UI:
#' The user interface consists of:
#' \describe{
#'   \item{titlePanel}{Displays the title "Biography Extractor".}
#'   \item{sidebarPanel}{
#'     Contains input fields for URL, Akim name, and biography text, along with submit buttons.
#'   }
#'   \item{mainPanel}{
#'     Displays the GPT response and a table of the extracted data.
#'   }
#' }
#'
#' @section Server:
#' The server logic includes:
#' \describe{
#'   \item{Reactive Values}{
#'     \code{akim_name_reactive} stores the Akim name.
#'   }
#'   \item{Event Observers}{
#'     \code{observeEvent(input$url_submit)} processes the URL input and updates the text areas.
#'     \code{observeEvent(input$bio_submit)} processes the biography input and updates the output.
#'   }
#'   \item{Outputs}{
#'     \code{output$gpt_response} displays the GPT response.
#'     \code{output$gpt_table} displays the extracted data in a table.
#'   }
#' }
#'
#' @section Running the Application:
#' The application is run using \code{shinyApp(ui = ui, server = server)}.
library(shiny)
library(rvest)
library(tidyverse)
library(chromote)
library(curl)

# These rows are required for running on a deployed shiny server :)
# Sys.setenv(CHROMOTE_CHROME = "/srv/shiny-server/bio-extractor/chrome/linux-129.0.6668.70/chrome-linux64/chrome")

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

# Define the LinkToCareerText function
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
    paste0(collapse = "") # Ensures that element*s* are not returned as a list
    #' This code is horrible-ish, but rvest does not (consistently) preserve the
    #' structure of the nodeset, and this is one way to keep it.
    #' Read back in with rvest::read_html().

# Return the content
return(html_content)
}

# Define the CheckForYandexHeaderTell function
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
CareerTextExtractor <- function(input) {
    if (length(input) != 0 && input != "No biography found") {
        
        text_output <- MainTextExtractor(input)

        if (stringr::str_detect(text_output, "Карьера")) {
            text_output <- stringr::str_extract(text_output,
                pattern = "Карьера\\s(?s).*"
            )
        }
        return(text_output)
    } else{
    return(NA_character_)
    }
}

# Define the BasicInformationTextExtractor function
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

system_prompt_work <- '
You receive biographies and split them into individual positions. Return the split biographies in the following csv format:
start_date, end_date, place_name, organisation, position, full_original_text
"dd.mm.yyyy", "dd.mm.yyyy", "string", "string", "string", "string"
"dd.mm.yyyy", "dd.mm.yyyy", "string", "string", "string", "string"

For missing data, return "NA". If the month is not clear, dates should be "yyyy". For place_name, organisation, and position, return the original text. full_original_text is the full entry for that position.
Do not geuss place if it is not explicitly stated.
'

system_prompt_study <- 'You receive biographies and split them into individual educational experiences. Only consider study at higher education institutions, not any work experience. Return the split biographies in the following csv format:
start_date, end_date, place_name, organisation, position, full_original_text
"dd.mm.yyyy", "dd.mm.yyyy", "string", "string", "string", "string"
"dd.mm.yyyy", "dd.mm.yyyy", "string", "string", "string", "string"

For missing data, return "NA". For place_name and organisation return the original text. For position, return "Студент", with the speciality in brackets, if given. full_original_text is the full entry for that position.
Do not geuss place if it is not explicitly stated.'

# Define the GPTBiographyPrompter function
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

instructions <-
    "<small>
<p>
    This app is built to make the task of scraping, formatting, and checking akims' biographies easier. To use it:
</p>

<ol>
    <li>
        <strong>[Optional]</strong> Put the web address of a biography on <strong>gov.kz</strong> and click 'submit url'. The app scrapes the page
        and attempts to separate the name and career sections for submission to the correct formatting model. It tries to autofill the relevant
        section with the text of these sections. This usually takes about 10 seconds. If the biography does not match the typical patterns of a biography page,
        it inserts all the text into the top input box and you'll have to seperate it manually.
    </li>

    <li>
        Insert/check the biography and <strong>[optional]</strong> akim name into the input boxes, and click submit. Wait a little bit while our
        fine-tuned GPT models try to format the biography. The 'career' box is sent to the GPT trained on work entries. The 'basic info/education' box sends
        it to the GPT trained on study entries. If all goes well, the app reads in the GPT's response as a CSV and presents
        it as a table, which can just be copy and pasted into Google Sheets. Please check the results!
    </li>
</ol>
<p>
    <strong>Future tasks:</strong> Isolate akimat name. Fine-tune/include models for birth date, military service.
</p>
</small>
"

# Define UI for the Shiny app
ui <- fluidPage(
    titlePanel("Biography scraping and formatting interface"),
    sidebarLayout(
        sidebarPanel(
            htmlOutput("instructions"),
            textInput("url", "Enter URL:", value = ""),
            actionButton("url_submit", "Submit URL"),
            textInput("akim_name_input", "Akim name", value = ""),
            textAreaInput("biography_input", "Enter career (Карьера) section of biography:", value = "", rows = 20),
            textAreaInput("basicinfo_input", "Enter basic information/education (Общая информация, образование) section of biography:", value = "", rows = 15),
            actionButton("bio_submit", "Submit biography")
        ),
        mainPanel(
            tableOutput("gpt_table"),
            htmlOutput("gpt_response")
        )
    )
)
# Define server logic for the Shiny app
server <- function(input, output, session) {
    akim_name_reactive <- reactiveVal(NULL)

    output$instructions <- renderUI({
        HTML(instructions)
    })

    observeEvent(input$url_submit, {
        req(input$url)
        html_content <- tryCatch(
            {
                LinkToCareerText(input$url)
            },
            error = function(e) {
                return("No biography found")
            }
        )

        biography <- CareerTextExtractor(html_content)
        if (is.na(biography)) {
            biography <- "No biography found"
        }

        basic_info <- ""

        if (!is.na(biography) && str_detect(biography, "Карьера")) {
            basic_info <- BasicInformationTextExtractor(html_content)

            if (is.na(basic_info)) {
                basic_info <- ""
            }
        }
        akim_name <- AkimNameFinder(html_content)

        akim_name_reactive(akim_name)

        updateTextAreaInput(session, "biography_input", value = biography)
        updateTextAreaInput(session, "basicinfo_input", value = basic_info)
        updateTextAreaInput(session, "akim_name_input", value = akim_name)
    })

    observeEvent(input$bio_submit, {
        req(input$biography_input)

        gpt_response_work <- GPTBiographyPrompter(input$biography_input,
            system = system_prompt_work,
            model = "ft:gpt-4o-2024-08-06:personal:corrected-pilot-bio-work2:ABKpSsvl"
        )

        if (nzchar(input$basicinfo_input)) {
            gpt_response_study <- GPTBiographyPrompter(input$basicinfo_input,
                system = system_prompt_study,
                model = "ft:gpt-4o-2024-08-06:personal:pilot-bio-study:ADC2E7TU"
            )
        } else {
            gpt_response_study <- NA_character_
        }

        gpt_table <- tryCatch(
            {
                read_csv(gpt_response_work, show_col_types = FALSE)
            },
            error = function(e) {
                return(data.frame(Error = "Invalid CSV format returned for work section."))
            }
        )

        gpt_table <- gpt_table |>
            mutate(event = "work", .before = 1)

        if (!is.na(gpt_response_study)) {
            gpt_table_study <- tryCatch(
                {
                    read_csv(gpt_response_study, show_col_types = FALSE)
                },
                error = function(e) {
                    return(data.frame(Error = "Invalid CSV format returned for study section."))
                }
            )

            gpt_table_study <- gpt_table_study |>
                mutate(event = "study", .before = 1)

            gpt_table <- rbind(gpt_table_study, gpt_table)
        }

        if (is.null(akim_name_reactive())) {
            akim_name_reactive(input$akim_name_input)
        }

        akim_name <- akim_name_reactive()

        if (nrow(gpt_table) > 0 && !is.null(akim_name) && length(akim_name) > 0) {
            gpt_table <- gpt_table |>
                mutate(FIO = akim_name, .before = 1)
        } else if (nrow(gpt_table) > 0 && (is.null(akim_name) || length(akim_name) == 0)) {
            gpt_table <- gpt_table |>
                mutate(FIO = "", .before = 1)
        }

        output$gpt_table <- renderTable(gpt_table)

        output$gpt_response <- renderUI({
            HTML(paste0(
                "<pre>", gpt_response_work, "</pre>",
                "<pre>", gpt_response_study, "</pre>"
            ))
        })
    })
}

# Run the application
shinyApp(ui = ui, server = server)
