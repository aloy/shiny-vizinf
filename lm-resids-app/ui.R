#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(rhandsontable)
library(shinycssloaders)
# library(shinythemes)


navbarPage(
  "Vizinf: Regression diagnostics",
  fluid = TRUE,
  tabPanel(
    "Enter data",
    useShinyjs(),
    fluidRow(
      
      column(4,
             wellPanel(
               p("Select a preloaded data set from the below list, 
            upload a data file, or enter the values of a variable."),           
               selectInput(inputId = "inputData",
                           label = "Select data set",
                           choices = datasets,
                           selected = "HollywoodMovies"),
               conditionalPanel(
                 condition = "input.inputData=='Upload data'",
                 h5("Upload Options"),
                 fileInput('file1', 'Choose a file to upload.',
                           accept = c(
                             'text/csv',
                             'text/comma-separated-values',
                             'text/tab-separated-values',
                             'text/plain',
                             '.csv',
                             '.tsv'
                           )
                 ),
                 p("Note: The file size limit is 5MB. Larger files will take longer to upload.
                  Accepted formats include: .txt, .csv, and .tsv files."),
                 actionButton("hideDataOptions", "Toggle upload options"),
                 hidden(
                   tags$div(id = "dataOptions",
                            checkboxInput('header', 'Header', TRUE),
                            radioButtons('sep', 'Separator',
                                         c(Comma=',',
                                           Semicolon=';',
                                           Tab='\t'),
                                         ','),
                            radioButtons('quote', 'Quote',
                                         c(None='',
                                           'Double Quote'='"',
                                           'Single Quote'="'"),
                                         '"')
                   )#divid
                 )
               ),
               
               # br(),
               fluidRow(
                 column(6, uiOutput("choose_x")),
                 column(6, uiOutput("choose_y"))
               )
               
               
             )
      ),
      
      column(8,
             # conditionalPanel("output.fileUploaded",
             withSpinner(plotOutput("fittedLine")),
             verbatimTextOutput("DF"),
             br(),
             tableOutput("fittedEqn")
             # )
      )
    )
    
  ),
  
  tabPanel(
    "Create a lineup",
    sidebarLayout(
      sidebarPanel(
        radioButtons("lineup", label = h4("Type of residual plot"),
                     c("Residuals vs. fitted values" = "resid.fitted",
                       "Residuals vs. x" = "resid.x", 
                       "Normal Q-Q plot" = "qq"), 
                     selected = "resid.fitted"),
        numericInput("num", 
                     label = h4("Number of plots"), 
                     value = 20, min = 1, max = 20),
        numericInput("ncols", 
                     label = h4("Number of columns"), 
                     value = 4, min = 1, max = 10),
        actionButton("goButton", "Create lineup!"),
        conditionalPanel(
          condition = "input.goButton > 0",
          br(),
          checkboxInput("reveal", "Reveal data panel"),
          conditionalPanel(
            condition = "input.reveal",
            uiOutput("dataPanel")
          )
        )
      ),
      mainPanel(
        plotOutput("lineup")
      )
    )
    
  ),
  
  
  tabPanel(
    "Observed plot",
    sidebarLayout(
      sidebarPanel(
        radioButtons("plot", label = h4("Type of residual plot"),
                     c("Residuals vs. fitted values" = "resid.fitted",
                       "Residuals vs. x" = "resid.x", 
                       "Normal Q-Q plot" = "qq"), 
                     selected = "resid.fitted")
      ),
      mainPanel(
        plotOutput("origPlot")
      )
    )
    
  ),
  

  tabPanel(
    "About",
    h3("Comparing groups vis the lineup protocol"),
    p("This Shiny app is intended..."),
    h3("Author"),
    p("Adam Loy"),
    p("Creative commons license...")
  )
  
  
)