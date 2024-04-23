FROM rocker/shiny:4

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
                "kableExtra"), \
                repo = "https://packagemanager.posit.co/cran/latest"\

)'

WORKDIR /Users/tathagatapain/Desktop
COPY server.R server.R
COPY www www
CMD Rscript server.R