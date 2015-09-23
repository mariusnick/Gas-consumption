library(shiny)


library(ggplot2)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel(" Residential Natural Gas Consumption "),

    # Create a new Row in the UI for selectInputs
    fluidRow(
      h4 ('Average monthly temperature [`C]'), 
          column(2,
          numericInput("oct","oct",value=10,min=-1,max=15)
      ),
     
      column(2,
             numericInput("nov","nov",value=5,min=-5,max=12)
      ),
      column(2,
             numericInput("dec","dec",value=1,min=-15,max=10)
      ),
             column(2,
                    numericInput("jan","jan",value=0,min=-20,max=10)
             ),
       column(2,
                  numericInput("feb","feb",value=0,min=-20,max=10)
       ),
      column(2,
                  numericInput("mar","mar",value=5,min=-5,max=15)
      )
    ),
    # Create a new row for the table.
    fluidRow(
          column(3,
                 numericInput("Area","Bulit Area [ sq. meters] ",value=100,min=30,max=350,step=5)
            
                  )
    ),
  
  mainPanel (
        plotOutput("Plot"),
        h3("Total Consumation for winter [mc]:"),
        h3(textOutput(    "text1"   ))
        )
  )
)
