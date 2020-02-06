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
    h3("App overview"),
    p(
      "This app allows you to render lineups of residual plots 
      for simple linear regression models. This can help you learn 
      to interpret residual plots, as it hones your intuition of 
      what signal and noise mean in a residual plot. To use the app:",
      tags$ul(
        tags$li("Choose a data set or upload your own."),
        tags$li("Specify the response and explanatory variables."),
        tags$li("Generate a lineup of residual plots. You can render the 
        residuals vs. the fitted values or the explanatory variable, or 
                you can render a normal Q-Q plot of the standardized residuals."),
        tags$li("Inspect the lineups and reveal the data plot once you have made your selection.")
      ),
      "After inspecting the lineup, you can focus on the observed data in the data plot tab."
    ),
    h3("Learning goals"),
    p(
      "This app is intended to help students build their intuition about residual plots.
       Instead of showing students a single residual plot and talking about 'random scatter' or 
       'patterns,' having students identify the 'most different' plot and discuss why they
       made their choice will help students figure out what type of signal is problematic.
       This is facilitated by they fact that lineups force you to compare the observed plot
       to plots taken from the distribution of noise plots (i.e. plots generated from an
       appropriate model)."
    ),
    h3("Example class usage"),
    p(
      "I recommend building a guided activity for your students.",
      tags$ol(
        tags$li("Introduce the data set and problem."),
        tags$li("Ask students to plot the data and fit the regression model.
                Depending on your goals, you can have them interpret the coefficients, etc."),
        tags$li("Instead of having students construct a single residual plot, have students
        generate a lineup of residual plots. Quickly explain that one panel displays
                the observed residual plot while the other panels are decoy plots generated
                under the assumption that the model fits (i.e. not assumptions were violated).
                Have students choose the plot that is most different and justify their 
                answer before revealing the answer."),
        tags$li("Ask students to discuss their decision in light of the 'answer' and
               what that indicates about the appropriateness of the model.") 
      )
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