# Gunakan base image rocker/shiny (sudah ada R + Shiny Server)
FROM rocker/shiny:4.4.1

# Install system dependencies (untuk package R spatial & lainnya)
RUN apt-get update && apt-get install -y \
    libssl-dev libcurl4-openssl-dev libxml2-dev \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory di dalam container
WORKDIR /srv/shiny-server/forestverse

# copy app
COPY . /srv/shiny-server/forestverse

# copy shiny config
COPY config/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Install package R (sesuaikan dengan library di global.R)
RUN R -e "install.packages(c( \
    'shiny','shinydashboard','bslib','leaflet','pool','glue','DBI','RPostgres', \
    'readxl','dplyr','sf','mapview','plotly','tidyr','jsonlite','colorspace', \
    'shinyjs','remotes','DT','reactable','htmltools','purrr','lubridate','stringr','stringi','leafgl','rsconnect' \
  ), repos='https://cloud.r-project.org/')"

# Expose port 3838 (default Shiny Server)
EXPOSE 3838

# Perintah untuk menjalankan Shiny Server
CMD ["/usr/bin/shiny-server"]
