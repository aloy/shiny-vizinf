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
library(ggplot2)

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

  # empty_df <- data.frame(x = rep(NA_integer_, 50), y = rep(NA_integer_, 50))
  
  
  theData <- reactive({
    if(input$inputData == "Upload data") {
      req(input$file1)
      return(read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote))
    } else {
      return(get(input$inputData))
    }
  })
  
  quant_vars <- reactive({
    req(theData())
    colnames(theData())[sapply(theData(), is.numeric)]
  })
  
  output$choose_x <- renderUI({
    selectInput("Xvar", "X-variable", c("Choose one" = "", quant_vars()))
  })
  
  output$choose_y <- renderUI({
    selectInput("Yvar", "Y-variable", c("Choose one" = "", quant_vars()))
  })
  
  
  regData <- reactive({
    req(input$Xvar, input$Yvar, input$Xvar %in% colnames(theData()))
    theData()[, c(input$Xvar, input$Yvar)]
  })
  
  mod <- reactive({
    lm(paste(input$Yvar, "~", input$Xvar), regData())
  })
  
  
  output$fittedEqn <- renderTable({
    mod() %>%
      tidy() %>%
      select(term, estimate)
  })
  
  # output$DF <- renderPrint({
  #   head(regData())
  #   })
  # 
  output$fittedLine <- renderPlot({
    regData() %>%
      ggplot(aes_string(input$Xvar, input$Yvar)) +
      geom_point(shape = 1) +
      geom_smooth(method = "lm", se = FALSE)
  })
    
  
  lineupData <- reactive({
    input$goButton
    obs <- augment(mod())
    N <- input$num
    df <- replicate(N-1, expr = simulate(mod()), simplify = FALSE)
    df[[N]] <- data.frame(sim_1 = obs[, input$Yvar])

    df <- lapply(df, FUN = function(y) {
      reg_df <- data.frame(y = y[[1]], x = obs[[input$Xvar]])
      broom::augment(lm(y ~ x, data = reg_df))
    })

    df <- df %>%
      bind_rows() %>%
      mutate(#.sample = rep(1:N, each = nrow(obs)),
             replicate = rep(c(1:(N-1), NA), each = nrow(obs)),
             .id = sample(N, size = N, replace = FALSE) %>% rep(., each = nrow(obs)))

    df
  })


  # output$lineupData <- renderPrint({lineupData()})

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
          facet_wrap(~.id, ncol = lineup_cols) +
          theme_bw()
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
                           geom_point(aes_string(x = input$Xvar, y = ".resid"), shape = 1) +
                           labs(x = input$Xvar, y = "Residuals"),
                         qq = obsData %>%
                           ggplot(aes(sample = .std.resid)) +
                           geom_qq_line() +
                           geom_qq() +
                           labs(x = "N(0, 1) quantiles", y = "Standardized residuals")
      )
      dataPlot +
        theme_bw()
    })
    
    
    output$dataPanel <- renderPrint({
      req(input$reveal)
      
      data_panel <- isolate(lineupData()) %>% 
        select(replicate, .id) %>%
        distinct() %>%
        filter(is.na(replicate))%>%
        pull(.id)
      
      tagList(
        tags$h3(paste0("The data plot is #", data_panel))
      )
    })
    

  })
