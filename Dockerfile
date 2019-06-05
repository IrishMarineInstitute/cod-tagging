FROM rocker/shiny:3.5.1
MAINTAINER Marine Institute
# install ssl
# and gdal
RUN sudo apt-get update && apt-get install -y libssl-dev libudunits2-0 libudunits2-dev libproj-dev libgdal-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
# install additional packages
RUN Rscript -e "install.packages(c('htmlwidgets','dplyr','plotly','leaflet','mapview'), repos='https://cran.rstudio.com/')"
## fixing running as non root
RUN sudo chown -R shiny:shiny /var/lib/shiny-server/
# copy shiny-server config file
#COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY Data /srv/shiny-server/tagging/Data
COPY www /srv/shiny-server/tagging/www
COPY .Rhistory /srv/shiny-server/tagging/
COPY app.R /srv/shiny-server/tagging/
COPY google-analytics.js /srv/shiny-server/tagging/
COPY README.md /srv/shiny-server/tagging/
COPY styles2.css /srv/shiny-server/tagging/
EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]
