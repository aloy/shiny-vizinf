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
library(qqplotr)
library(dplyr)
library(nullabor)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  rv <- reactiveValues(show = FALSE)
  
  observeEvent(input$goButton, {
    updateCheckboxInput(session, "reveal", value = FALSE)
  })
  
  observeEvent(input$goButton, {
    rv$show <- TRUE
  })
  
  observeEvent(input$inputData, {
    rv$show <- FALSE
  })
  
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  
  
  theData <- reactive({
    if(input$inputData == "Enter data") {
      txt <- input$inputVector
      return(data.frame(x = as.numeric(unlist(strsplit(txt, "\\s|\\n")))))
    } else {
      if(input$inputData == "Upload data") {
        req(input$file1)
        return(read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote))
        
      } else {
        return(get(input$inputData))
      }
    } 
  })
  
  quant_vars <- reactive({
    colnames(theData())[sapply(theData(), is.numeric)]
  })

  observe({
    updateSelectInput(
      session = session,
      inputId = "inputVar",
      choices = quant_vars()
    )
  })  

  
  output$theData <- DT::renderDataTable(theData(),
                                        options = list(pageLength = 10,
                                                       scrollX = TRUE))
    
  origData <- reactive({
    target_var <- ifelse(is.null(input$inputVar), "x", input$inputVar)
    data.frame(x = theData()[[target_var]])
  })
  
  
  
  pos <- eventReactive(input$goButton, {
    sample(input$num, size = 1)
  })
  
  lineupData <- eventReactive(input$goButton, {
    lineup_cols <- input$ncols
    npanels <- input$num

    lineup(
      method = null_dist("x", dist = "norm"),
      true = na.omit(origData()),
      n = npanels,
      pos = pos()
    )
  })
  
  output$lineup <- renderPlot({
    detrend <- FALSE
    if(input$detrend == "detrend") detrend <- TRUE
    
    qqplot_lineup <- lineupData() %>%
      ggplot(aes(sample = x)) +
      stat_qq_line(linetype = 2, detrend = detrend) +
      stat_qq_point(detrend = detrend) + 
      facet_wrap(~.sample, ncol = input$ncols) +
      theme_bw() + 
      labs(x = "N(0, 1) quantiles", y = "Sample quantiles")
    
    if(detrend) {
      qqplot_lineup +
        coord_fixed(ratio = 1, ylim = range(lineupData()$x))
    }
    
    qqplot_lineup
    # }
    
  }, 
  height = function() {
    0.8 * session$clientData$output_lineup_width
  }
  )
  
  output$origPlot <- renderPlot({
    df <- origData()

    if(input$standard) df <- as.data.frame(scale(df))

    detrend <- FALSE
    if(input$detrend_orig == "detrend") detrend <- TRUE

    qqplot_lineup <- df %>%
      ggplot(aes(sample = x)) +
      qqplotr::stat_qq_line(linetype = 2, detrend = detrend, dparams = list(mean = 0, sd = 1)) +
      qqplotr::stat_qq_point(detrend = detrend, dparams = list(mean = 0, sd = 1)) +
      theme_bw() +
      labs(x = "N(0, 1) quantiles", y = "Sample quantiles")

    if(detrend) {
      qqplot_lineup +
        coord_fixed(ratio = 1, ylim = range(df$x))
    }

    qqplot_lineup

  })
  
  output$dataPanel <- renderPrint({
    req(input$reveal)
    
    tagList(
      tags$h3(paste0("The data plot is #", pos()))
    )
  })

  
})
