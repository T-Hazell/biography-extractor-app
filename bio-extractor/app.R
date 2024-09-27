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

# Define the CareerTextExtractor function
CareerTextExtractor <- function(input) {
    if (length(input) != 0) {
        text_output <- input |>
            rvest::read_html() |>
            rvest::html_elements(xpath = "//main") |>
            rvest::html_text() |>
            paste(collapse = "\n")

        if (stringr::str_detect(text_output, "Карьера")) {
            text_output <- stringr::str_extract(text_output,
                pattern = "Карьера\\s(?s).*"
            )
        }
        return(text_output)
    }
    return("No biography found")
}

AkimNameFinder <- function(input) {
    if (length(input) != 0) {
        text_output <- input |>
            rvest::read_html() |>
            rvest::html_element("h2") |>
            rvest::html_text()

        if (!is.null(text_output) && str_count(text_output, "\\S+") == 3) {
            return(text_output)
        }
    }
    return("No name found")
}

# Define the LinkToCareerText function
LinkToCareerText <- function(link) {

    page <- link |>
        read_html_live()

    # Wait, or rvest stops
    Sys.sleep(3)

    html_content <- page |>
        rvest::html_element("div") |>
        rvest::html_elements(xpath = "//main") |>
        as.character()

    # Return the
    return(html_content)
}

system_prompt <- '
You receive biographies and split them into individual positions. Return the split biographies in the following csv format:
start_date, end_date, place_name, organisation, position, full_original_text
"dd.mm.yyyy", "dd.mm.yyyy", "string", "string", "string", "string"
"dd.mm.yyyy", "dd.mm.yyyy", "string", "string", "string", "string"

For missing data, return "NA". If the month is not clear, dates should be "yyyy". For place_name, organisation, and position, return the original text. full_original_text is the full entry for that position. # nolint: line_length_linter.
Do not geuss place if it is not explicitly state.
'

GPTBiographyPrompter <- function(prompt, model) {
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
                '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'
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
                list(role = "system", content = system_prompt),
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

# Define UI for the Shiny app
ui <- fluidPage(
    titlePanel("Biography Extractor"),
    sidebarLayout(
        sidebarPanel(
            textInput("url", "Enter URL:", value = ""),
            actionButton("url_submit", "Submit URL"),
            textInput("akim_name_input", "Akim name", value = ""),
            textAreaInput("biography_input", "Enter biography:", value = "", rows = 20),
            actionButton("bio_submit", "Submit biography")
        ),
        mainPanel(
            htmlOutput("gpt_response"),
            tableOutput("gpt_table")
        )
    )
)
# Define server logic for the Shiny app
server <- function(input, output, session) {
    akim_name_reactive <- reactiveVal(NULL)

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
        akim_name <- AkimNameFinder(html_content)

        akim_name_reactive(akim_name)

        updateTextAreaInput(session, "biography_input", value = biography)
        updateTextAreaInput(session, "akim_name_input", value = akim_name)
    })

    observeEvent(input$bio_submit, {
        req(input$biography_input)
        gpt_response <- GPTBiographyPrompter(input$biography_input, model = "ft:gpt-4o-2024-08-06:personal:corrected-pilot-bio-work2:ABKpSsvl")

        gpt_table <- tryCatch(
            {
                read_csv(gpt_response, show_col_types = FALSE)
            },
            error = function(e) {
                return(data.frame(Error = "Invalid CSV format"))
            }
        )

        gpt_table <- gpt_table |>
            mutate(event = "work", .before = 1)

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

        output$gpt_response <- renderUI({
            HTML(paste0("<pre>", gpt_response, "</pre>"))
        })

        output$gpt_table <- renderTable(gpt_table)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
