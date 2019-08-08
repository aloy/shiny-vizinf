library(shiny)
library(shinyjs)

navbarPage(
  "Vizinf: Comparing groups",
  tabPanel(
    "Enter data",
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        p("Select a preloaded data set from the below list, 
            upload a data file, or enter the values of a variable."),           
        selectInput(inputId = "inputData",
                    label = "Select data set",
                    choices = datasets,
                    selected = "ImmuneTea"),
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
 
        selectInput('group', label=h4('Grouping variable'),'group'),
        selectInput('response', label=h4('Response variable'), 'response')
      ),
        
      # Show a plot of the generated distribution
      mainPanel(
        dataTableOutput("theData")
      )
    )
  ),
  
  tabPanel(
    "Lineup",
    sidebarLayout(
      sidebarPanel(
        radioButtons("lineup", label = h4("Plot type"),
                     c("Boxplots" = "box",
                       "Density plots" = "den", 
                       "Violin plots" = "violin"), 
                     selected = "box"),
        numericInput("num", 
                     label = h4("Number of plots"), 
                     value = 20, min = 1, max = 20),
        numericInput("ncols", 
                     label = h4("Number of columns"), 
                     value = 4, min = 1, max = 10),
        actionButton("goButton", "Create lineup!")
      ),
      mainPanel(
        plotOutput("lineup")
      )
    )
  ),
  
  tabPanel(
    "Data plot",
    sidebarLayout(
      sidebarPanel(
        radioButtons("plot", label = h4("Plot type"),
                     c("Boxplots" = "box",
                       "Density plots" = "den", 
                       "Violin plots" = "violin"), 
                     selected = "box")
      ),
      mainPanel(
        plotOutput("origPlot"),
        h4("Summary Statistics"),
        tableOutput("basicSummary")
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