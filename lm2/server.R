#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(Lock5Data)
library(dplyr)
library(broom)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  empty_df <- data.frame(x = rep(NA_integer_, 50), y = rep(NA_integer_, 50))
    
  output$fileUploaded <- reactive({
    if(input$dataEntry != "upload") {
      return(TRUE)
    } else {
      inFile1 <- input$file1
      return(!is.null(inFile1))
    }
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  theData <- reactive({
    if(input$dataEntry == "preloaded") {
      return(get(input$inputData))
    }

    if (input$dataEntry == "upload") {
      inFile1 <- input$file1
      if(!is.null(inFile1)) {
        return(read.csv(inFile1$datapath, header = input$header))
      } else {
        return(NULL)
      }
    }

    if (input$dataEntry == "enter") {
      if (is.null(input$hot_data)) {
        DF <- empty_df
      } else {
        DF <- hot_to_r(input$hot_data)
      }
      return(DF)
    }


  })
  
  # theData <- reactive({
  #   switch(
  #     input$dataEntry,
  #     preloaded = get(input$inputData),
  #     upload = {if (input$dataEntry == "upload") {
  #       inFile1 <- input$file1
  #       if(!is.null(inFile1)) {
  #         read.csv(inFile1$datapath, header = input$header)
  #       } else {
  #         NULL
  #       }
  #     }
  #     },
  #     enter =  { if (input$dataEntry == "enter") {
  #       if (is.null(input$hot_data)) {
  #         empty_df
  #       } else {
  #         hot_to_r(input$hot_data)
  #       }
  #     }
  #     }
  #   )
  # })
  
  output$hot_data <- renderRHandsontable({
    rhandsontable(theData(), useTypes = FALSE, selectCallback = TRUE, stretchH = "all", 
                  height = 210) 
  })
  
  observe({
    quant_vars <- colnames(theData())[sapply(theData(), is.numeric)]
    # upload_quant_vars <- colnames(upload_data())[sapply(upload_data(), is.numeric)]
    
    updateSelectInput(
      session = session,
      inputId = "Xvar",
      choices = quant_vars
    )
    
    updateSelectInput(
      session = session,
      inputId = "Yvar",
      choices = quant_vars
    )
    
  })
  
  output$data <- renderTable(theData())
  
})
