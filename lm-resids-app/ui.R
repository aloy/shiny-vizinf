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
    
    fluidRow(
      
      column(4,
             wellPanel(
               radioButtons("dataEntry", label = "Data entry method",
                            choices = c("Preloaded" = "preloaded", 
                                        "Upload file" = "upload", 
                                        "Enter manually" = "enter"),
                            selected = "preloaded",
                            inline = TRUE),
               
               conditionalPanel(
                 "input.dataEntry == 'preloaded'",
                 selectInput(inputId = "inputData",
                             label = "Choose data set",
                             choices = datasets,
                             selected = "HollywoodMovies"),
                 rHandsontableOutput("hot_preload")
                 
               ),
                 conditionalPanel(
                   "input.dataEntry == 'upload'",
                   fluidRow(
                     column(8,
                            fileInput('file1', 'Choose a CSV file',
                                      accept = c('text/csv',
                                                 'text/comma-separated-values', 
                                                 '.csv')
                            )
                     ),
                     column(4, 
                            checkboxInput("header", label = "Header row", value = TRUE)
                     ),
                     conditionalPanel("output.fileUploaded",
                                      rHandsontableOutput("hot_upload")
                   )
                 )       
               ),
               # br(),
               fluidRow(
                 column(6,
                        selectInput(inputId = "Xvar",
                                    label = "X-variable",
                                    choices = NULL,
                                    selected = NULL)
                 ),
                 column(6,
                        selectInput(inputId = "Yvar",
                                    label = "Y-variable",
                                    choices = NULL,
                                    selected = NULL)
                 )
                 
               ),
               
               conditionalPanel(
                 "input.dataEntry == 'enter'",
                 rHandsontableOutput("hot_enter")
               )
               
               
             )
      ),
      
      column(8,
             # conditionalPanel("output.fileUploaded",
             withSpinner(plotOutput("fittedLine")),
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
        h4("This is the sidebar")
        ),
      mainPanel(
        
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