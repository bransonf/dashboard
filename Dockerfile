FROM rocker/r-ver:3.6.0

MAINTAINER Branson Fox <bransonf@wustl.edu>

# linux dependencies for R packages
RUN apt-get update && apt-get install -y \
	libudunits2-0 \
	libudunits2-dev \
	libgdal-dev \
	libssl-dev \
	libcurl4-openssl-dev \
	libcairo2-dev \
	pandoc \
	wget


# install R libraries
RUN R -e "install.packages(c('shiny','shinyWidgets','leaflet','leaflet.extras','sf','dygraphs','timevis','dplyr','leafsync','magrittr','tidyr','httr','jsonlite','mapview','RColorBrewer','pushbar','lubridate','webshot','remotes'))"

# install dev version packages
RUN R -e "remotes::install_github('JohnCoene/waiter')"

# install phantomJS for image capture
RUN R -e "webshot::install_phantomjs()"

# copy app to image
RUN mkdir /root/dashboard
COPY dashboard /root/dashboard

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/dashboard', port = 3838, host = '0.0.0.0')"] 
