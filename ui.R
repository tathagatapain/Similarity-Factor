library(shiny)
library(shinydashboard)
library(tibble)
library(reshape2)
library(ggplot2)
library(data.table)
library(DT)
library(dplyr)
library(tibble)
library(plotly)
library(vctrs)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shinyalert)
library(MASS)
library(knitr)
library(shinythemes)
library(V8)
library(kableExtra)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui<-fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsResetCode, functions = c("winprint")),
  tags$head(
    tags$style(HTML("
      .nav-tabs > li > a {
        background-color: #FFFFFF; 
        color: #000000; 
      }
      .logo {
            background-color: #000033 !important;
            }
      .navbar {
              background-color: #000033 !important;
              }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: #000033;
        color: #FFFFFF;
      }
      .dashboard-title {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        max-width: 100%;
      }
      .btn{
          background-color: #000033;
          color: white;
        }
    "))
  ),
dashboardPage(
  dashboardHeader(title="Dissolution Profile Comparison",titleWidth = "100%"),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenuOutput("menu")),
  dashboardBody(
    fluidRow(column(4, align="center", offset = 4,uiOutput("uiLogin"), textOutput("pass"))),
    tabItems(
      tabItem(
        tabName = "similarity_analysis",
            fluidRow(column(3,fileInput(inputId="file",label="Import the Datafile"),
                            actionButton("similarity_analysis","Load Analysis"),
                            
                            actionButton("refresh","Refresh Data")
                            
                            ),
                     column(6,box(tableOutput("objtable"),title = "Objectives",solidHeader = F, collapsible = TRUE,width = 300)),
                             hr(),  
                               # ),
        # fluidRow(
        #   column(4,
        #               actionButton("similarity_analysis","Load Analysis"),
        # 
        # actionButton("refresh","Refresh Data")
        # ),
        column(2),
        column(1,actionButton('logout',label = tags$b('Log Out'),icon("sign-out")))
        ),
          hr(),
          tabsetPanel(id="tabs",type="tabs",
                      tabPanel(conditionalPanel(condition="input.similarity_analysis>0",
                                                "Data",value="raw_data"),
                               
                               fluidRow(
                                 conditionalPanel(condition="input.similarity_analysis>0",
                                                  column(4,
                                                        box(tableOutput("raw_data"),title="Data",solidHeader = F,collapsible = T,width = 500,style = "max-height: 400px; overflow-y: auto;"),
                                                  fluidRow(downloadButton("download","Download Data",class="butt"))),
                                                  column(8,box(tableOutput("desctable"),title = "Data Description",solidHeader = F, collapsible = TRUE,width = 300)),
                                                  hr(),
                                                  hr(),
                                                  textOutput("value_note"),
                                                  

                                 )
                               )),
                      tabPanel(conditionalPanel(condition="input.similarity_analysis>0",
                                                "Similarity Analysis",value="similarity_analysis"),
                               fluidRow(
                                 conditionalPanel(condition="input.similarity_analysis>0",
                                                  column(4, box(tableOutput("table"),title="Similarity Table",solidHeader = F,collapsible = T,width = 300), 
                                                         conditionalPanel(condition="input.similarity_analysis>0",
                                                                          actionButton("excludebutton","Exclude"),
                                                                          actionButton("calculated_value_f2","Calculate F2"),
                                                                          
                                                                                                   textOutput("value"),
                                                                          hr(),
                                                                          # column(8,
                                                                          
                                                                          numericInput("bootsample", "Number of bootstrapping samples:", 5000, min = 1, max = Inf),
                                                                          actionButton("btsbutton","Calculate CI"),
                                                                          fluidRow(
                                                                          box(tableOutput("bootstrapdt"),title="Bootstrapping: Percentile Method",solidHeader = F,collapsible = T,width = 300),     
                                                                          box(tableOutput("cibcadt"),title="Bootstrapping: BCa Method",solidHeader = F,collapsible = T,width = 300)),     
                                                                          
                                                                          
                                                                          
                                                                          tags$head(tags$style("#value{color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                                                                          )
                                                                          ),
                                                  )),
                                                  column(8, selectInput("plot_graph", "Select Graph",choices=c("Trendline", "Bar Chart", "Boxplot"), 
                                                                        selected = "Trendline"),
                                                         box(plotOutput("plot_graph"),solidHeader = F,collapsible = T,width = 300,style = "font-family: 'Arial', sans-serif; font-size: 14px;",
                                                             tags$head(
                                                           tags$style(
                                                             HTML(".box-title { font-weight: bold; }")
                                                           )
                                                         )),
                                                         downloadButton("downloader","Download Report",class="butt"),
                                                         box(id="title1",title = "Add Title 1",width = 4,solidHeader = T,style = "font-family: 'Arial', sans-serif; font-size: 14px;",textInput("title1","",value = " ")),
                                                         box(id="title2",title = "Add Title 2",width = 4,solidHeader = T,style = "font-family: 'Arial', sans-serif; font-size: 14px;",textInput("title2","",value = " "))
                                                         
                                                         )))
                               )
                      
                      
                      # tabPanel(conditionalPanel(condition="input.similarity_analysis>0",
                      #                           "",value="similarity_analysis"),
        
                               
                               
                               )
      
  )
))
)
)
