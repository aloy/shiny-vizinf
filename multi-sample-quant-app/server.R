library(shiny)
library(shinyjs)
library(mosaic)
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
  qvars <- colnames(data)[sapply(data,is.numeric)]
  updateSelectInput(session, 'group', choices = cvars)
  updateSelectInput(session, 'response', choices = qvars)
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
  data
})

output$origPlot <- renderPlot({
  dataPlot <- switch(input$plot,
                     box = ggplot(filteredData(), aes(x = group, y = response)) +
                       geom_boxplot(aes(fill = group, color = group), alpha = 0.6) +
                       geom_point(aes(x = group, y = mean, fill = group, color = group), 
                                  data = filteredData() %>% group_by(group) %>% summarize(mean = mean(response, na.rm = TRUE))) +
                       scale_fill_colorblind() +
                       scale_color_colorblind(),
                     den = ggplot(filteredData(), aes(x = response)) +
                       geom_density(aes(fill = group, color = group), alpha = 0.6) +
                       scale_fill_colorblind() +
                       scale_color_colorblind(),
                     violin = ggplot(filteredData(), aes(x = group, y = response)) +
                       geom_violin(aes(fill = group, color = group), alpha = 0.6) +
                       geom_point(aes(x = group, y = mean, fill = group, color = group), 
                                  data = filteredData() %>% group_by(group) %>% summarize(mean = mean(response, na.rm = TRUE))) +
                       scale_fill_colorblind() +
                       scale_color_colorblind()
                    
  )
  dataPlot
})


output$basicSummary <- renderTable({
  favstats(~response|group, data=filteredData())  
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

groupMeans <- reactive({
  lineupData() %>%
    group_by(.id, group) %>%
    summarize(mean = mean(response))
})


output$lineup <- renderPlot({
  if(input$goButton > 0) {
    input$goButton
    lineup_type <- isolate(input$lineup)
    lineup_cols <- isolate(input$ncols)
    lineup_plot <- switch(lineup_type,
                          box = lineupData() %>%
                            # as.data.frame() %>%
                            ggplot() +
                            geom_boxplot(aes(x = group, y = response, fill = group, color = group), alpha = 0.6) +
                            geom_point(aes(x = group, y = mean, fill = group, color = group), 
                                       data = groupMeans()),
                          den = lineupData() %>%
                            ggplot() +
                            geom_density(aes(x = response, fill = group, color = group), alpha = 0.6),
                          violin = lineupData() %>%
                            ggplot() +
                            geom_violin(aes(x = group, y = response, fill = group, color = group), alpha = 0.6) +
                            geom_point(aes(x = group, y = mean, fill = group, color = group), 
                                       data = groupMeans())
    )
    lineup_plot +
      facet_wrap(~.id, ncol = lineup_cols) +
      scale_fill_colorblind() +
      scale_color_colorblind() +
      theme(legend.position = "none")
  }
},
height = function() {
  0.8 * session$clientData$output_lineup_width
})

})