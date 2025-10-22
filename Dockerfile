FROM rocker/shiny:latest

# Install system libraries required by leaflet and other packages
RUN apt-get update && apt-get install -y \
    cmake \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    make \
    g++ \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinythemes', 'DT', 'wordcloud2', 'ggplot2', 'stringr', 'dplyr', 'leaflet', 'bslib', 'htmlwidgets', 'httr', 'jsonlite', 'zip'), repos='http://cran.rstudio.com/')"

# This gives the shiny user the necessary write access.
RUN mkdir -p /srv/shiny-server/app_cache && \
    chown -R shiny:shiny /srv/shiny-server/app_cache

# Copy the Shiny app into the container
COPY app.R /srv/shiny-server/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Expose Shiny default port
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]