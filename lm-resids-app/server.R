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
  
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))

  # empty_df <- data.frame(x = rep(NA_integer_, 50), y = rep(NA_integer_, 50))
  
  
  theData <- reactive({
    if(input$inputData == "Upload data") {
      
      inFile1 <- input$file1
      if (is.null(inFile1)) return(NULL)
      
      return(read.csv(inFile1$datapath, header=input$header, sep=input$sep, quote=input$quote))
    } else {
      return(get(input$inputData))
    }
  })
  
  
  output$fileUploaded <- reactive({
    inFile1 <- input$file1
    return(!is.null(inFile1))
  })
  
  # outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
    
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
      choices = quant_vars,
      selected = quant_vars[2]
    )
    
  })
  
  
  
  regData <- reactive({
    data <- theData()
    if(is.null(data)){
      data <- data.frame(x = 0, y = 0)
    } else {
      req(input$Xvar)
      req(input$Yvar)
      data <- data[, c(input$Xvar, input$Yvar)]
    }
    colnames(data) <- c("x","y")
    data
  })
  
  mod <- reactive({
    mod <- lm(y ~ x, data = regData())
  })
    
  
  lineupData <- reactive({
    input$goButton
    obs <- augment(mod())
    N <- input$num
    df <- replicate(N-1, expr = simulate(mod()), simplify = FALSE)
    df[[N]] <- data.frame(sim_1 = obs$y)

    df <- lapply(df, FUN = function(y) {
      reg_df <- data.frame(y = y[[1]], x = obs[["x"]])
      broom::augment(lm(y ~ x, data = reg_df))
    })

    df <- df %>%
      bind_rows() %>%
      mutate(.sample = rep(1:N, each = nrow(obs)),
             .id = sample(N, size = N, replace = FALSE) %>% rep(., each = nrow(obs)))

    df
  })
    
    

    output$fittedEqn <- renderTable({
      # input$goButton
      tidy(mod()) %>%
        select(term, estimate)
    })

    # output$DF <- renderPrint(regData())

    output$fittedLine <- renderPlot({
      # input$goButton
      regData() %>%
        ggplot(aes(x, y)) +
        geom_point(shape = 1) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = input$Xvar, y = input$Yvar)
    })


    output$lineup <- renderPlot({
      if(input$goButton > 0) {
        input$goButton
        lineup_type <- isolate(input$lineup)
        lineup_cols <- isolate(input$ncols)
        lineup_plot <- switch(lineup_type,
                              resid.fitted = lineupData() %>%
                                ggplot() +
                                geom_hline(yintercept = 0, linetype = 2, color = "blue") +
                                geom_point(aes(x = .fitted, y = .resid), shape = 1) +
                                labs(x = "Fitted values", y = "Residuals"),
                              resid.x = lineupData() %>%
                                ggplot() +
                                geom_hline(yintercept = 0, linetype = 2, color = "blue") +
                                geom_point(aes(x = x, y = .resid), shape = 1) +
                                labs(x = input$Xvar, y = "Residuals"),
                              qq = lineupData() %>%
                                ggplot(aes(sample = .std.resid)) +
                                geom_qq_line() +
                                geom_qq() +
                                labs(x = "N(0, 1) quantiles", y = "Standardized residuals")
        )
        lineup_plot +
          facet_wrap(~.id, ncol = lineup_cols)
      }
    },
    height = function() {
      0.8 * session$clientData$output_lineup_width
    })
    
    
    output$origPlot <- renderPlot({
      obsData <- augment(mod())
      dataPlot <- switch(input$plot,
                         resid.fitted = obsData %>%
                           ggplot() +
                           geom_hline(yintercept = 0, linetype = 2, color = "blue") +
                           geom_point(aes(x = .fitted, y = .resid), shape = 1) +
                           labs(x = "Fitted values", y = "Residuals"),
                         resid.x = obsData %>%
                           ggplot() +
                           geom_hline(yintercept = 0, linetype = 2, color = "blue") +
                           geom_point(aes(x = x, y = .resid), shape = 1) +
                           labs(x = input$Xvar, y = "Residuals"),
                         qq = obsData %>%
                           ggplot(aes(sample = .std.resid)) +
                           geom_qq_line() +
                           geom_qq() +
                           labs(x = "N(0, 1) quantiles", y = "Standardized residuals")
      )
      dataPlot
    })
    
    

  })
