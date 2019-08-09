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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
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

  
  output$theData <- renderDataTable(theData(), 
                                    options = list(pageLength = 10,
                                                   scrollX = TRUE))
  
  # lineupData <- reactive({
  #   if(input$goButton > 0) {
  #     target_var <- ifelse(is.null(input$inputVar), "x", input$inputVar)
  #     x <- theData()[[target_var]]
  #     N <- length(x)
  #     npanels <- input$num
  #     norm_sims <- lapply(1:(npanels - 1), function(x) rnorm(N))
  #     
  #     data.frame(
  #       x = c(unlist(norm_sims), scale(x)), 
  #       .sample = rep(1:npanels, each = nrow(theData()))
  #     ) %>%
  #       mutate(.id = sample(npanels, size = npanels, replace = FALSE) %>% rep(., each = N))
  #   }
  # })
  
  # output$lineupdata <- renderTable(lineupData())
  
  output$lineup <- renderPlot({
    if(input$goButton > 0) {
      input$goButton
      lineup_type <- isolate(input$lineup)
      lineup_cols <- isolate(input$ncols)
      
      target_var <- ifelse(is.null(input$inputVar), "x", input$inputVar)
      x <- theData()[[target_var]]
      N <- length(x)
      npanels <- input$num
      norm_sims <- lapply(1:(npanels - 1), function(x) rnorm(N))
      
      lineupData <- data.frame(
        x = c(unlist(norm_sims), scale(x)), 
        .sample = rep(1:npanels, each = nrow(theData()))
      ) %>%
        mutate(.id = sample(npanels, size = npanels, replace = FALSE) %>% rep(., each = N))
      
      detrend <- FALSE
      if(input$detrend == "detrend") detrend <- TRUE
      
      qqplot_lineup <- lineupData %>%
        ggplot(aes(sample = x)) +
        stat_qq_line(linetype = 2, detrend = detrend) +
        stat_qq_point(detrend = detrend) + 
        facet_wrap(~.id, ncol = input$ncols) +
        theme_bw() + 
        labs(x = "N(0, 1) quantiles", y = "Sample quantiles")
      
      if(detrend) {
        qqplot_lineup +
          coord_fixed(ratio = 1, ylim = range(lineupData$x))
      }
      
      qqplot_lineup
    }
  
  }, 
  height = function() {
    0.8 * session$clientData$output_lineup_width
  }
)
  
  output$origPlot <- renderPlot({
    target_var <- ifelse(is.null(input$inputVar), "x", input$inputVar)
    df <- theData()[target_var]
    colnames(df) <- "x"

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

  
})
