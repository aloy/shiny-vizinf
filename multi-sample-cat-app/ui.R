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
        DT::dataTableOutput("theData")
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
    h3("App overview"),
    p(
      "In the app you can render lineups to investigate associations across groups. To use the app:",
      tags$ul(
        tags$li("Choose a data set or upload your own."),
        tags$li("Specify the response and explanatory variables."),
        tags$li("Generate a lineup of stacked bar charts or mosaic plots."),
        tags$li("Inspect the lineups and reveal the data plot.")
      ),
      "After inspecting the lineup, you can focus on the observed data in the data plot tab."
    ),
    h3("Learning goals"),
    p(
      "This app is intended to introduce students to the logic behind hypothesis tests for
      an association. After exploring the app, students should understand that identifying 
      the data plot indicates that either the data are systematically different from what 
      would be expected if no association exists, or that they were 'lucky' in their guess. 
      Further, students should understand that variability exists when there is no association,
      which is why we need to rely on inferential procedures to help us understand whether we
      observed signal or noise."
    ),
    h3("Example class usage"),
    p(
      "I recommend building a guided activity for your students.",
      tags$ol(
        tags$li("Introduce the data set and problem."),
        tags$li("Ask students to state their hypotheses in words (or notation)."),
        tags$li("Ask students how they would display the sample data to review EDA."),
        tags$li("Have students generate a lineup. Quickly explain that one panel displays
                the observed data while the other panels are decoy plots generated
                under the assumption that no association exists between the variables.
                Have students choose the plot that is most different and justify their 
                answer before revealing the answer."),
       tags$li("Ask students to discuss their decision in light of the 'answer' and
               whether that supports one of the competing claims.") 
      )
      ),
    h3("Additional resources"),
    p("Below are additional resources to help you learn about visual inference and how it can be used in 
      the classroom:",
      tags$ul(
        tags$li(a("An overview of classroom usage", href="http://bit.ly/intro-viz-inference")),
        tags$li(a("Tutorial on creating lineups in R using the nullabor package", 
                  href="https://aloy.github.io/classroom-vizinf/")),
        tags$li(a("Introductory paper on visual inference (Buja et al., 2009)", href="https://royalsocietypublishing.org/doi/abs/10.1098/rsta.2009.0120"))
        )
    ),
    h3("Author"),
    p("Adam Loy - ", a("aloy.rbind.io", href = "https://aloy.rbind.io")),
    a(img(src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png"), rel="license", 
      href="http://creativecommons.org/licenses/by-nc-sa/4.0/"),
    p("This work is licensed under a", 
      a("Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.",  
        href="http://creativecommons.org/licenses/by-nc-sa/4.0/"))
  )
)