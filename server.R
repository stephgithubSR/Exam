library(shiny)
shinyServer(function(input, output) {
  model <- reactive({
    brushed_data <- brushedPoints(ChickWeight, input$brush1,
                            xvar = "Time", yvar = "weight")
    if(nrow(brushed_data) < 2){
      return(NULL)
    }
    lm(weight ~ Time, data = brushed_data)
  })
  
  output$slopeOut <- renderText({
    if(is.null(model())){
      "No Model Found"
    } else {
      model()[[1]][2]
    }
  })
  
  output$intOut <- renderText({
    if(is.null(model())){
      "No Model Found"
    } else {
      model()[[1]][1]
    }
  })
  
  output$plot1 <- renderPlot({
    plot(ChickWeight$Time, ChickWeight$weight, xlab = "Time (days)",
         ylab = "weight (gm)", main = "Chicken weight since birth",
         cex = 1.5, pch = 16, bty = "n")
    if(!is.null(model())){
      abline(model(), col = "blue", lwd = 2)
    }
  })
  
  
})