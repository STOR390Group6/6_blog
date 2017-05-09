library(shiny)



shinyUI(pageWithSidebar( 
  
  headerPanel("NBA Players' Sentiment"),  
  
  sidebarPanel(                             
    
    selectInput(inputId = "input_team",    
                
                label = "NBA Team",  
                
                choices = unique(twitter_sentiment$Tm)   
                
    ),
    
    
    uiOutput("ui")
    
  ),
  
  
  mainPanel(   
    plotOutput(outputId="plot_1"),
    plotOutput(outputId="plot_2")
    # h3("This is you saying it"),  
    # p("blahhh")  
  )
  
))
