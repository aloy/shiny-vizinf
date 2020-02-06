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
# library(shinythemes)


navbarPage(
  "Vizinf: Normal Q-Q plots",
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
        conditionalPanel(
          condition = "input.inputData!='Enter data'",
          selectInput(inputId = "inputVar",
                      label = "Select a quantitative variable",
                      choices = NULL,
                      selected = NULL)
        ),
        
        conditionalPanel(
          condition = "input.inputData=='Enter data'",
          textAreaInput(inputId = "inputVector",
                    label = "Enter observations (separated by space) 
                    or copy and paste from spreadsheet",
                    width = "100%",
                    height = "100px",
                    resize = "vertical"
          )
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        dataTableOutput("theData")
      )
    )
  ),
  
  tabPanel(
    "Create a lineup",
    sidebarLayout(
      sidebarPanel(
        radioButtons("detrend", label = h4("Plot type"),
                     c("Ordinary Q-Q plot" = "qq",
                       "De-trended Q-Q plot" = "detrend"), 
                     selected = "qq"),
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
        # tableOutput("lineupdata"),
        plotOutput("lineup")
      )
    )
    
  ),
  
  
  tabPanel(
    "Data plot",
    sidebarLayout(
      sidebarPanel(
        h4("Q-Q plot options"),
        radioButtons("detrend_orig", label = h5("Q-Q plot types"),
                     c("Ordinary Q-Q plot" = "qq",
                       "De-trended Q-Q plot" = "detrend"),
                     selected = "qq"),
        h5("Standardization"),
        checkboxInput("standard", label = "Standardize the variable", value = TRUE)
      ),
      mainPanel(
        plotOutput("origPlot")
      )
    )
  ),

  tabPanel(
    "About",
    h3("App overview"),
    p(
      "In the app you can render lineups to introduce students to Q-Q plots",
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
      "I recommend building a guided activity for your students. Before this
      activity I recommend discussing how Q-Q plots are constructed and read. 
      You can also use the 'Rorschach protocol' (i.e. generating only null
      plots) to help students see a little variability before applying Q-Q plots
      to a data-driven problem. I recommend a guided activity with the following
      steps:",
      tags$ol(
        tags$li("Introduce the data set and problem."),
        tags$li("Generate a lineup of normal Q-Q plots. Quickly explain that one panel displays
                the observed Q-Q plot while the other panels are decoy plots generated
                under the assumption of normality. Have students choose the plot that 
                is most different and justify their answer before revealing the answer."),
        tags$li("Ask students to discuss their decision in light of the 'answer' and
               what that indicates about the appropriateness of the normal model.") 
      ),
      "You can repeat this type of activity for a few data situations, providing
      students with examples of common departures from normality, along with different
      samples sizes from the normal model."
    ),
    h3("Additional resources"),
    p("Below are additional resources to help you learn about visual inference and how it can be used in 
      the classroom:",
      tags$ul(
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