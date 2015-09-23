library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define a server for the Shiny app

shinyServer(function(input, output) {
      
      output$text1<-renderText({ sum(sapply(c(input$oct,input$nov,input$dec,input$jan,input$feb,input$mar),consum<-function(temp){
            consum<-239.7-temp*22+input$Area*0.997}))})
      output$Plot<-renderPlot({par(mfrow=c(2,1),mar=c(2,4,2,4),oma=c(0,0,0,0))
                  plot(sapply(c(input$oct,input$nov,input$dec,input$jan,input$feb,input$mar),consum<-function(temp){
                   consum<-239.7-temp*22+input$Area*0.997}),type="l", main="Energy",ylab="Gas consumation mc", xlab="Month", col="red")
                  
                  plot(c(input$oct,input$nov,input$dec,input$jan,input$feb,input$mar),type="l", main="Temperature",ylab="Temperature")
             }
               )

})