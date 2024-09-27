# FROM rocker/shiny-verse:latest
FROM rocker/shiny-verse:4.4.1

## Install any Linux system dependencies
RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev

# please review all the latest versions here:
# https://googlechromelabs.github.io/chrome-for-testing/
ENV CHROMEDRIVER_VERSION=129.0.6668.70

### install chrome
RUN apt-get update && apt-get install -y wget && apt-get install -y zip
RUN wget -q https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
RUN apt-get install -y ./google-chrome-stable_current_amd64.deb

### install chromedriver
RUN wget https://edgedl.me.gvt1.com/edgedl/chrome/chrome-for-testing/$CHROMEDRIVER_VERSION/linux64/chromedriver-linux64.zip \
    && unzip chromedriver-linux64.zip && rm -dfr chromedriver_linux64.zip \
    && mv /chromedriver-linux64/chromedriver /usr/bin/chromedriver \
    && chmod +x /usr/bin/chromedriver

## Install R libraries
RUN R -e "install.packages(c('shiny', 'tidyverse', 'rvest', 'httr'))"
RUN R -e "remotes::install_github('rstudio/chromote')"

## Copy Shiny application files
COPY /bio-extractor /srv/shiny-server/

## Grant access to server directory
RUN sudo chown -R shiny:shiny /srv/shiny-server

## Open the port for Shiny
EXPOSE 3838

## Start Shiny Server
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 3838)"]
