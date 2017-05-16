library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("105753501 Hw4"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a method:", 
                  choices = c("method1", "method2", "method3","method4","method5",
                              "method6","method7","method8","method9","method10")),
      
      numericInput("obs", "Number of observations to view:", 10)
    ),
    
    # Show a summary of the dataset and an HTML table with the 
    # requested number of observations
    mainPanel(
      h1(textOutput("method"),align="center"),
      plotOutput("plot"),
      tableOutput("view")
   
    
    )
  )
))