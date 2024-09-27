# FROM rocker/shiny-verse:latest
FROM rocker/shiny-verse:latest

## Install any Linux system dependencies
RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev

## Install R libraries
RUN R -e "install.packages(c('shiny', 'tidyverse', 'rvest', 'httr'))"

## Copy Shiny application files
COPY /app /srv/shiny-server/
COPY /data /srv/shiny-server/data

## Grant access to server directory
RUN sudo chown -R shiny:shiny /srv/shiny-server

## Open the port for Shiny
EXPOSE 3838

## Start Shiny Server
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 3838)"]
