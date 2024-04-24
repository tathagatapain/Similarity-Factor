FROM rocker/shiny:latest

RUN R -e 'install.packages(c(\
                "shiny", \
                "shinydashboard", \
                "tibble", \
                "reshape2", \
                "ggplot2", \
                "data.table", \
                "DT", \
                "dplyr", \
                "plotly", \
                "vctrs", \
                "scales", \
                "shinyWidgets", \
                "shinythemes", \
                "shinyjs", \
                "shinyalert", \
                "MASS", \
                "knitr", \
                "V8", \
                "xml2", \
                "httr", \
                "openssl", \
                "kableExtra"), \
                repo = "https://packagemanager.posit.co/cran/latest"\

)'

WORKDIR /Users/tathagatapain
COPY server.R server.R
COPY www www
CMD Rscript server.R