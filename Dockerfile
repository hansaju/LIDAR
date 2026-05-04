FROM rocker/shiny:4.3.3

RUN R -e "install.packages(c('shiny','shinydashboard','httr2'), repos='https://cloud.r-project.org/')"

# Create app folder
RUN mkdir -p /srv/shiny-server/app

# Copy app into folder
COPY . /srv/shiny-server/app/

# Configure port for Render
RUN echo "run_as shiny;" > /etc/shiny-server/shiny-server.conf && \
    echo "server { listen 10000; location / { app_dir /srv/shiny-server/app; log_dir /var/log/shiny-server; } }" >> /etc/shiny-server/shiny-server.conf

EXPOSE 10000

CMD ["/usr/bin/shiny-server"]
