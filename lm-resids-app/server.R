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
  
  # shinyjs::onclick("hideDataOptions",
  #                  shinyjs::toggle(id = "dataOptions", anim = TRUE))
  # 
  empty_df <- data.frame(x = rep(NA_integer_, 50), y = rep(NA_integer_, 50))
  
  
  theData <- reactive({
    if(input$dataEntry == "preloaded") {
      get(input$inputData)
    } 
    
    if (input$dataEntry == "upload") {
      if(!is.null(inFile1)) {
        read.csv(inFile1$datapath, header = input$header)
      } else {
        NULL
      }
    }
    
    if (input$dataEntry == "enter") {
      if (is.null(input$hot_enter)) {
        DF <- empty_df
      } else {
        DF <- hot_to_r(input$hot_enter)
      }
      DF
    }
    
  })
  
  # preload_data <- reactive({
  #   get(input$inputData)
  # })
  # 
  # enter_data <- reactive({
  #   if (is.null(input$hot_enter)) {
  #     DF <- empty_df
  #   } else {
  #     DF <- hot_to_r(input$hot_enter)
  #   }
  #   DF
  # })
  
  output$fileUploaded <- reactive({
    inFile1 <- input$file1
    return(!is.null(inFile1))
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
    
  # upload_data <- reactive({
  #   inFile1 <- input$file1
  #   try(read.csv(inFile1$datapath, header = input$header), silent = TRUE)
  # })
  
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
    
    # updateSelectInput(
    #   session = session,
    #   inputId = "Xvar2",
    #   choices = upload_quant_vars
    # )
    # 
    # updateSelectInput(
    #   session = session,
    #   inputId = "Yvar2",
    #   choices = upload_quant_vars
    # )
    
  })

  
  
  output$hot_enter <- renderRHandsontable({
    rhandsontable(enter_data(), useTypes = FALSE, selectCallback = TRUE, stretchH = "all", 
                  height = 210) 
  })
  
  output$hot_preload <- renderRHandsontable({
    rhandsontable(preload_data(), useTypes = FALSE, selectCallback = TRUE, stretchH = "all", 
                  height = 210) 
  })
  
  output$hot_upload <- renderRHandsontable({
    rhandsontable(upload_data(), useTypes = FALSE, selectCallback = TRUE, stretchH = "all", 
                  height = 210) 
  })
  
  # theData <- reactive({
  #   switch(input$dataEntry,
  #          preloaded = preload_data(),
  #          upload = upload_data(),
  #          enter = enter_data()
  #   )
  # })
  
  regData <- reactive({
    data <- theData()
    if(is.null(data)){
      data <- data.frame(x = 0, y = 0)
    } else {
      if(input$dataEntry == "preloaded") {
        req(input$Xvar)
        req(input$Yvar)
        data <- data[, c(input$Xvar,input$Yvar)]
      }
      if(input$dataEntry == "upload") {
        req(input$Xvar)
        req(input$Yvar)
        data <- data[, c(input$Xvar2,input$Yvar2)]
      }   
      colnames(data) <- c("x","y")
    }
    data
  })

    
    output$fittedEqn <- renderTable({
      # input$goButton
      mod <- lm(y ~ x, data = regData())
      tidy(mod) %>%
        select(term, estimate)
    })
    
    # output$DF <- renderPrint(regData())
    
    output$fittedLine <- renderPlot({
      # input$goButton
      regData() %>%
        ggplot(aes(x, y)) +
        geom_point(shape = 1) +
        geom_smooth(method = "lm", se = FALSE)
    })
    
  
})
