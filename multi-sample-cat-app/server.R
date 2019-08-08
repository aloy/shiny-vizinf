library(shiny)
library(shinyjs)
library(mosaicCore)
library(ggmosaic)
library(dplyr)
library(resample)
library(ggthemes)
library(infer)    # tools for simulation-based inference
library(Lock5Data)

shinyServer(function(input, output, session){

  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  
  
  theData <- reactive({
    if(input$inputData == "Upload data") {
      
      inFile1 <- input$file1
      if (is.null(inFile1)) return(NULL)
      
      return(read.csv(inFile1$datapath, header=input$header, sep=input$sep, quote=input$quote))
    } else {
      return(get(input$inputData))
    }
  })

observe({
  data <- theData()
  cvars <- colnames(data)[sapply(data,is.factor)]
  updateSelectInput(session, 'group', choices = cvars)
  updateSelectInput(session, 'response', choices = cvars, selected = cvars[2])
})  


output$theData <- renderDataTable(theData(), 
                                  options = list(pageLength = 10,
                                                 scrollX = TRUE))


filteredData <- reactive({
  data <- isolate(theData())
  #if there is no input, make a dummy dataframe
  if(input$group == "group" && input$response == "response"){
    if(is.null(data)){
      data <- data.frame(group = 0, response = 0)
    }
  }else{
    data <- data[,c(input$group,input$response)]
    names(data) <- c("group","response")
  }
  na.omit(data)
})

output$origPlot <- renderPlot({
  ggplot(data = filteredData()) +
    geom_mosaic(aes(x = product(response, group), fill = response), na.rm = TRUE) + 
    labs(x = input$group, y = input$response) + 
    theme_minimal() +
    scale_fill_colorblind()
})


output$basicSummary <- renderPrint({
  tab <- with(filteredData(), table(group, response))
  addmargins(tab)
  })



lineupData <- reactive({
  if(input$goButton > 0) {
      filteredData() %>%
      specify(response ~ group) %>% 
      hypothesize(null = "independence") %>%
      generate(reps = input$num - 1, type = "permute") %>%
      as_tibble() %>%
      bind_rows(filteredData()) %>%
      mutate(.id = sample(input$num, size = input$num, replace = FALSE) %>% rep(each = nrow(filteredData())))
  }
})


output$lineup <- renderPlot({
  if(input$goButton > 0) {
    input$goButton
    lineup_type <- isolate(input$lineup)
    lineup_cols <- isolate(input$ncols)
  
    ggplot(data = lineupData()) +
      geom_mosaic(aes(x = product(response, group), fill = response), na.rm = TRUE) + 
      facet_wrap(~.id, ncol = lineup_cols) +
      labs(x = input$group, y = input$response) + 
      theme_minimal() +
      scale_fill_colorblind() +
      theme(
        legend.position = "none",
        axis.text = element_blank()
      )
  }
},
height = function() {
  0.8 * session$clientData$output_lineup_width
})

})