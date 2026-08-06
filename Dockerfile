FROM ubuntu:20.04

MAINTAINER "Daniel Jacob" daniel.jacob@u-bordeaux.fr

# Configure timezone
ENV  DEBIAN_FRONTEND=noninteractive \
     TZ=Europe/Paris
RUN  ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Install essential libraries
RUN  apt-get update && apt-get install -y \
     sudo ca-certificates wget curl git vim ed unzip \
     libcurl4-gnutls-dev libcairo2-dev libv8-dev libssl-dev \
     openssl gdebi-core pkgconf libxml2-dev \
     build-essential  software-properties-common \
# Install Repositories
  && sh -c 'echo "deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/" >> /etc/apt/sources.list' \
  && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
  && add-apt-repository ppa:c2d4u.team/c2d4u4.0+ \
# locale setting
  && apt-get install -y locales \
  && echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
  && locale-gen en_US.utf8 \
  && /usr/sbin/update-locale LANG=en_US.UTF-8 \
  && apt-get -y clean && apt-get -y autoremove && rm -rf /var/lib/{cache,log}/ /var/cache/oracle-jdk8-installer /tmp/* /var/tmp/*

# Create / Configure a Timezone
ENV  LC_ALL=en_US.UTF-8 \
     LANG=en_US.UTF-8 \
     TZ=Europe/Paris

# Download and install shiny server
RUN  ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone \
  && wget --no-verbose "https://download3.rstudio.org/ubuntu-20.04/x86_64/shiny-server-1.5.23.1030-amd64.deb" -O ss-latest.deb \
  && gdebi -n ss-latest.deb \
  && rm -f ss-latest.deb

# Install R and some packages
RUN  apt-get update && apt-get install -y \
       r-recommended r-cran-rcurl r-cran-base64enc r-cran-mime r-cran-inline r-cran-docopt \
       r-cran-foreach r-cran-multicore r-cran-doparallel r-cran-dosnow r-cran-xml \
       r-cran-signal r-cran-ptw r-cran-minqa r-cran-optimx r-cran-stringr r-cran-plyr \
       r-cran-htmltools r-cran-htmlwidgets r-cran-markdown r-cran-devtools \
       r-cran-dt r-cran-plotly r-cran-magrittr r-cran-ggplot2 r-cran-igraph \
       r-cran-biocmanager r-bioc-impute r-bioc-massspecwavelet \ 
       r-cran-shiny r-cran-shinybs  r-cran-shinyjs r-cran-shinywidgets \
       r-cran-shinycssloaders r-cran-colourpicker r-cran-openxlsx \
  && R -e "remotes::install_version('shiny', version = '1.14.0', force=TRUE)" \
  && R -e "remotes::install_version('shinyWidgets', version = '0.9.0', force=TRUE, upgrade='never')" \
  && R -e "remotes::install_version('shinycssloaders', version = '1.1.0', force=TRUE, upgrade='never')" \
  && R -e "remotes::install_version('htmltools', version = '0.5.9', force=TRUE, upgrade='never')" \
  && R -e "install.packages(c('webshot2'), repos='http://cran.rstudio.com/')"

# Install RnmrQuant1D package
RUN  R -e "remotes::install_github('inra/Rnmr1D', upgrade='never')" \
  && R -e "remotes::install_github('djacob65/RnmrQuant1D', upgrade='never')" \
  && apt-get -y clean && apt-get -y autoremove && rm -rf /var/lib/{cache,log}/ /tmp/* /var/tmp/*

# Add RnmrQuant1D_UI application
ADD ./app /srv/shiny-server

# Copy the shiny-server configuration file and its startup script.
RUN  cp /srv/shiny-server/conf/shiny-server.conf /etc/shiny-server/shiny-server.conf \
  && cp /srv/shiny-server/conf/launch-server.sh /usr/local/bin \
  && chmod 755 /usr/local/bin/launch-server.sh \
  && rm -f /srv/shiny-server/index.html /srv/shiny-server/sample-apps

WORKDIR /home

EXPOSE 3838

CMD ["/usr/local/bin/launch-server.sh"]
