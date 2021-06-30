# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:4.1.0

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    sudo \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    curl \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## app folder
COPY /app-files ./dashboard
## renv.lock file
COPY /app-files/renv.lock ./renv.lock

# change the working directory to the current directory
RUN cd /dashboard

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/dashboard', host = '0.0.0.0', port = 3838)"]

# To build and run this file and create a docker image, use the following commands:
# docker build -t dashboard-image .
# docker run -d --rm -p 3838:3838 dashboard-image