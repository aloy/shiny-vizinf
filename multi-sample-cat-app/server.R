library(shiny)
library(shinyjs)
library(ggmosaic)
library(dplyr)
library(ggthemes)
library(Lock5Data)
library(summarytools)
library(magrittr)

shinyServer(function(input, output, session){
  
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
  
  pos <- eventReactive(input$goButton, {
    sample(input$num, size = 1)
  })
  

  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  
  
  theData <- reactive({
    if(input$inputData == "Upload data") {
      req(input$file1)
      return(read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote))
    } else {
      return(get(input$inputData))
    }
  })
  
  cvars <- reactive({
    colnames(theData())[sapply(theData(), is.factor)]
  })

  observe({
    updateSelectInput(session, 'group', choices = cvars())
    updateSelectInput(session, 'response', choices = cvars(), selected = cvars()[2])
  })  
  

output$theData <- renderDataTable(theData(), 
                                  options = list(pageLength = 10,
                                                 scrollX = TRUE))


filteredData <- reactive({
  req(theData())
  
  data <- theData()[, c(input$group, input$response)]
  names(data) <- c("group", "response")
  
  na.omit(data)
})


output$origPlot <- renderPlot({
  dataPlot <- switch(input$plot,
                     mosaic = ggplot(data = filteredData()) +
                       geom_mosaic(aes(x = product(response, group), fill = response), na.rm = TRUE),
                     bar = ggplot(data = filteredData()) +
                       geom_bar(aes(x = group, fill = response), position = "fill")
  )
  
  dataPlot +
    labs(x = input$group, y = input$response) + 
    theme_minimal() +
    scale_fill_colorblind("")
  
})


output$basicSummary <- renderPrint({
    tab <- with(filteredData(), table(group, response))
    addmargins(tab)
  })


lineupData <- reactive({
  req(input$goButton)
  N <- isolate(input$num)
  
  lineup(method = null_permute("group"), true = filteredData(), n = N, pos = pos())
})


output$lineup <- renderPlot({
  req(rv$show)
  # req(lineupData())
  # lineup_type <- isolate(input$lineup_type)
  lineup_cols <- isolate(input$ncols)
  
  lineup_plot <- switch(input$lineup_type,
                        mosaic = ggplot(data = lineupData()) +
                          geom_mosaic(aes(x = product(response, group), fill = response), na.rm = TRUE),
                        bar = ggplot(data = lineupData()) +
                          geom_bar(aes(x = group, fill = response), position = "fill")
  )

  
  lineup_plot + 
    facet_wrap(~.sample, ncol = lineup_cols) +
    labs(x = input$group, y = input$response) + 
    theme_minimal() +
    scale_fill_colorblind() +
    theme(
      legend.position = "none",
      axis.text = element_blank()
    )
},
height = function() {
  0.8 * session$clientData$output_lineup_width
})

output$dataPanel <- renderPrint({
  req(input$reveal)
  
  tagList(
    tags$h3(paste0("The data plot is #", pos()))
  )
})

})