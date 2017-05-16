library(shiny)
library('ROCR')

method1<-read.csv("method1.csv",header=T,sep=",")
method2<-read.csv("method2.csv",header=T,sep=",")
method3<-read.csv("method3.csv",header=T,sep=",")
method4<-read.csv("method4.csv",header=T,sep=",")
method5<-read.csv("method5.csv",header=T,sep=",")
method6<-read.csv("method6.csv",header=T,sep=",")
method7<-read.csv("method7.csv",header=T,sep=",")
method8<-read.csv("method8.csv",header=T,sep=",")
method9<-read.csv("method9.csv",header=T,sep=",")
method10<-read.csv("method10.csv",header=T,sep=",")





shinyServer(function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "method1" = method1,
           "method2" = method2,
           "method3" = method3,
           "method4" = method4,
           "method5" = method5,
           "method6" = method6,
           "method7" = method7,
           "method8" = method8,
           "method9" = method9,
           "method10" = method10
           )
  })
  

  
  

  # Show the name of the mehtod
  output$method<-renderText(
    {
      input$dataset
    }
  )
  
  

  # Generate a summary of the dataset
  output$plot <- renderPlot({
    dataset <- datasetInput()
    pred<-prediction(dataset$pred.score,dataset$reference)
    perf<-performance(pred,"tpr","fpr")
    plot(perf)
  })
  
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
})