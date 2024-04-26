FROM alpine:edge

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
                repo = "https://packagemanager.rstudio.com/cran/__linux__/focal/latest"\
)'

WORKDIR /Users/tathagatapain
COPY app.R app.R
COPY www www
CMD Rscript app.R
