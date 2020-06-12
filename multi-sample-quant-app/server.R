library(shiny)
library(shinyjs)
library(mosaic)
library(dplyr)
library(ggthemes)
library(Lock5Data)
library(DT)

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

  
  variables <- reactive({
    req(theData())
    list(
      cat   = colnames(theData())[sapply(theData(), is.factor)],
      quant = colnames(theData())[sapply(theData(), is.numeric)]
    )
  })
  
observe({
  updateSelectInput(session, 'group', choices = variables()$cat)
  updateSelectInput(session, 'response', choices = variables()$quant)
})  


output$theData <- DT::renderDataTable(data.table(theData()),
                                  options = list(pageLength = 10,
                                                 scrollX = TRUE))

# output$theData <- renderPrint({
#   str(theData())
# })


filteredData <- reactive({
  req(theData())
  
  data <- theData()[, c(input$group, input$response)]
  names(data) <- c("group", "response")

  data  
})

output$origPlot <- renderPlot({
  dataPlot <- switch(input$plot,
                     box = ggplot(filteredData(), aes(x = group, y = response)) +
                       geom_boxplot(aes(fill = group, color = group), alpha = 0.4) +
                       geom_point(aes(x = group, y = mean, fill = group, color = group), 
                                  data = filteredData() %>% 
                                    group_by(group) %>% 
                                    summarize(mean = mean(response, na.rm = TRUE))
                                  ),
                     den = ggplot(filteredData(), aes(x = response)) +
                       geom_density(aes(fill = group, color = group), alpha = 0.4),
                     violin = ggplot(filteredData(), aes(x = group, y = response)) +
                       geom_violin(aes(fill = group, color = group), alpha = 0.4) +
                       geom_point(aes(x = group, y = mean, fill = group, color = group), 
                                  data = filteredData() %>% 
                                    group_by(group) %>% 
                                    summarize(mean = mean(response, na.rm = TRUE)))
                    
  )
  dataPlot +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw()
})


output$basicSummary <- renderTable({
  favstats(~response|group, data=filteredData())  
  })



lineupData <- reactive({
  req(input$goButton)
  N <- isolate(input$num)

    lineup(method = null_permute("group"), true = filteredData(), n = N, pos = pos())
})

groupMeans <- reactive({
  lineupData() %>%
    group_by(.sample, group) %>%
    summarize(mean = mean(response))
})


output$lineup <- renderPlot({
  req(rv$show)
  lineup_type <- input$lineup
  lineup_cols <- isolate(input$ncols)
  lineup_plot <- switch(lineup_type,
                        box = lineupData() %>%
                          ggplot() +
                          geom_boxplot(aes(x = group, y = response, fill = group, color = group), alpha = 0.4) +
                          geom_point(aes(x = group, y = mean, fill = group, color = group), 
                                     data = groupMeans()),
                        den = lineupData() %>%
                          ggplot() +
                          geom_density(aes(x = response, fill = group, color = group), alpha = 0.4),
                        violin = lineupData() %>%
                          ggplot() +
                          geom_violin(aes(x = group, y = response, fill = group, color = group), alpha = 0.4) +
                          geom_point(aes(x = group, y = mean, fill = group, color = group), 
                                     data = groupMeans())
  )
  lineup_plot +
    facet_wrap(~.sample, ncol = lineup_cols) +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_minimal() +
    theme(legend.position = "none") 
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