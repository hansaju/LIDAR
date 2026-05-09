FROM rocker/shiny:4.3.3

# Install system dependencies (NO liblas-dev)
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages (single line)
RUN R -e "install.packages(c('shiny','shinydashboard','httr2','data.table','dplyr','ggplot2','leaflet','sf','lidR','rlas','plotly'), repos='https://cloud.r-project.org/')"

# Copy your app files
COPY . /srv/shiny-server/

# Configure Shiny Server to use Render port
RUN echo "run_as shiny;" > /etc/shiny-server/shiny-server.conf && \
    echo "server { listen 10000; location / { site_dir /srv/shiny-server; log_dir /var/log/shiny-server; directory_index on; } }" >> /etc/shiny-server/shiny-server.conf

# Expose port
EXPOSE 10000

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]
