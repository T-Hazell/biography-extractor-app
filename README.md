# Biography Extractor App

This Shiny application is designed to scrape, format, and check biographies of akims (regional governors in Kazakhstan). It extracts relevant sections from the provided URLs and formats them using fine-tuned OpenAI models.

## TODO
- Fine-tune model on birth place.
- Fine-tune model on military service.
- Isolate (fine-tune model on?) akimat name.
- Link cleaning before checks.
- Add a 'loading' spinner.
- Add a 'copy to clipboard' button.
- Server-side improvements.

## Installation

1. Clone the repository:
    ```sh
    git clone https://github.com/yourusername/biography-extractor-app.git
    cd biography-extractor-app
    ```

2. Install the required R packages:
    ```r
    install.packages(c("shiny", "rvest", "tidyverse", "chromote", "curl"))
    ```

3. Set up the environment for `chromote` (if running on a deployed Shiny server). A Chrome executable should be put somewhere accessible to the Shiny server, and the path to it should be set in the environment variable `CHROMOTE_CHROME`. For example:
    ```r
    Sys.setenv(CHROMOTE_CHROME = "/srv/shiny-server/bio-extractor/chrome/linux-129.0.6668.70/chrome-linux64/chrome")
    ```

4. Add your OpenAI API key to `bio-extractor/api_key.txt`

## Usage

1. Run the Shiny app:
    ```r
    shiny::runApp("path/to/your/app")
    ```

2. Follow the instructions in the app to scrape, format, and check biographies:

<ol>
    <li>
        <strong>[Optional]</strong> Paste in the web address of a biography on <strong>gov.kz</strong> or <strong>yandexwebcache.net</strong>
        (for other links, it may fail to extract text or split it into sections) and click 'submit url'. The app scrapes the page and
        attempts to separate the name, career, and education sections for submission to the correct formatting model. It tries to
        autofill the relevant section with the text of these sections. This usually takes about 10 seconds. If the biography does not
        match the typical patterns of a biography page, it inserts all the text into the top input box and you'll have to separate it manually.
    </li>

    <li>
        Insert/check the biography and <strong>[optional]</strong> akim name into the input boxes, and click submit. Wait a little bit while our
        fine-tuned OpenAI models try to format the biography. The 'career' box is sent to the model trained on work entries. The 'basic
        info/education' box sends it to the model trained on study entries. If all goes well, the app reads in the model's response as a CSV
        and presents it as a table, which can just be copy and pasted into Google Sheets. Please check the results!
    </li>
</ol>
