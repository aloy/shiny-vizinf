library(shiny)
library(shinyjs)
library(shinycssloaders)

navbarPage(
  "Vizinf: Comparing groups",
  tabPanel(
    "Enter data",
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        p("Select a preloaded data set from the below list or 
            upload a data file."),           
        selectInput(inputId = "inputData",
                    label = "Select data set",
                    choices = datasets,
                    selected = "fly"),
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
 
        selectInput('group', label = h4('Grouping variable'), 'group'),
        selectInput('response', label = h4('Response variable'), 'response')
      ),
        
      # Show spreadsheet of the generated distribution
      mainPanel(
        dataTableOutput("theData")
      )
    )
  ),
  
  tabPanel(
    "Lineup",
    sidebarLayout(
      sidebarPanel(
        radioButtons("lineup_type", label = h4("Plot type"),
                     c("Mosaic plot" = "mosaic",
                       "Stacked barchart" = "bar")),
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
        # verbatimTextOutput("dataPanel"),
        withSpinner(plotOutput("lineup"))
      )
    )
  ),
  
  tabPanel(
    "Data plot",
    sidebarLayout(
      sidebarPanel(
        radioButtons("plot", label = h4("Plot type"),
                     c("Mosaic plot" = "mosaic",
                       "Stacked barchart" = "bar"), 
                     selected = "mosaic")
      ),
      mainPanel(
        withSpinner(plotOutput("origPlot")),
        h4("Summary Statistics"),
        verbatimTextOutput("basicSummary")
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