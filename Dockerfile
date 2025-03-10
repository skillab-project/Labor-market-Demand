# Official R base image
FROM rocker/rstudio:latest

RUN apt-get update && apt-get install -y \
    ca-certificates \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
	libglpk40 \
	libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libudunits2-dev \
	libglu1-mesa-dev \
    libx11-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages('remotes', repos='https://cran.rstudio.com')"
RUN R -e "remotes::install_github('ricardo-bion/ggradar')"

RUN R -e "install.packages('plumber')"
RUN R -e "install.packages('future')"
RUN R -e "install.packages('promises')"
RUN R -e "install.packages('httr')"
RUN R -e "install.packages('jsonlite')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('igraph')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('ggthemes')"
RUN R -e "install.packages('apcluster')"
RUN R -e "install.packages('uwot')"
RUN R -e "install.packages('text2vec')"
RUN R -e "install.packages('cluster')"
RUN R -e "install.packages('mclust')"
RUN R -e "install.packages('factoextra')"
RUN R -e "install.packages('ggrepel')"
RUN R -e "install.packages('ggforce')"
RUN R -e "install.packages('binda')"
RUN R -e "install.packages('FactoMineR')"

RUN R -e "if (!requireNamespace('plumber', quietly = TRUE)) { stop('plumber not installed') }"

# Copy your application code to the Docker container
WORKDIR /app
COPY . /app

# Expose the API ports
EXPOSE 8872

# Default command to run the API on port 8872
CMD ["Rscript", "-e", "plumber::plumb('/app/plumber_analytics_Skillab_KC.R')$run(host='0.0.0.0', port=8872)"]
