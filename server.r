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
      output$downloadData <- downloadHandler(
               filename = function() {
                 paste('help-', Sys.Date(), '.csv', sep='')
               },
               content = function(con) {
                 help<-c("This app predicts gas consumption of a household during a winter
                         Input data in the application are
                         - The average temperature for winter months  in degrees Celsius, range of values,
                           the average temperature is set.  
                         - The built area of the house (in square meters).  
                         
                         The application will
                         - draw natural gas consumption graph for a house (cubic meters)  
                         - draw temperature variation chart  
                         - calculate the total natural gas consumption in winter ")
                     write.csv(help, con)
               }
             )

})