library(shiny)
library(shinyjs)
library(mosaicCore)
library(ggmosaic)
library(dplyr)
library(resample)
library(ggthemes)
library(infer)    # tools for simulation-based inference
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
  # data <- isolate(theData())
  # #if there is no input, make a dummy dataframe
  # if(input$group == "group" && input$response == "response"){
  #   if(is.null(data)){
  #     data <- data.frame(group = 0, response = 0)
  #   }
  # }else{
  #   data <- data[, c(input$group, input$response)]
  #   names(data) <- c("group","response")
  # }
  
  req(theData())
  
  data <- theData()[, c(input$group, input$response)]
  names(data) <- c("group", "response")
  
  # data 
  
  na.omit(data)
})

# 
# success <- reactive({
#   levels(filteredData()$response)[1]
# })

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


output$basicSummary <- # renderPrint({
  renderPrint({
    tab <- with(filteredData(), table(group, response))
    addmargins(tab)
  })

nlevels <- reactive({
  length(levels(y()))
})

y <- reactive({
  filteredData()[["response"]]
})

success <- reactive({
  levels(y())[1]
})

lineupData <- reactive({
  req(input$goButton)
  N <- isolate(input$num)
  
  if(nlevels() == 2) {
    perms <- filteredData() %>%
      specify(response ~ group, success = success())
  } else {
    perms <- filteredData() %>%
      specify(response ~ group)
  }
  
  
  perms %>%
    hypothesize(null = "independence") %>%
    generate(reps = N - 1, type = "permute") %>%
    as_tibble() %>%
    bind_rows(filteredData()) %>%
    mutate(.id = sample(N, size = N, replace = FALSE) %>% rep(each = nrow(filteredData())))
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

  
  # lineup_plot <- ggplot(data = lineupData()) +
    # geom_mosaic(aes(x = product(response, group), fill = response), na.rm = TRUE)
  
  lineup_plot + 
    facet_wrap(~.id, ncol = lineup_cols) +
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