
rm(list=ls())
library(shiny)
#library(shinydashboard)

#setwd("E:/R shiny")

#Define the UI for display page
UI<- fluidPage(
  #App title
  titlePanel("Sampling Distribution of Mean"),
  #Make Layout to include tabs and inputs
  tabsetPanel(
    #Page 1
    tabPanel("Mean Distribution of All possible value of Die ", fluid =T,
      sidebarLayout(
        sidebarPanel(
          numericInput("roll","Number of dice to roll", min=1, max=6,value=1),
          actionButton("Dice", "Select")
          
          
        ),
        mainPanel(plotOutput("histo",height = "400px", width="75%"),
                  h1(""),
                  h5("Click on the link for the documentation"),
                  h5("https://iamalabhya.wordpress.com/2020/06/11/mean-distribution-of-all-the-possible-value-of-a-rolling-a-dice/")
        )
        
      )
    ),
    #Page 2
    tabPanel("Mean distribution of the dice roll simulation", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 sliderInput("Die_size", "No. of Dice", min = 1, max=6, value =1),
                 numericInput("Die_times", "No. of times the average from the dice are recorded" , min=0, max= 500, step =50, value=1)
                 
               ),
               mainPanel(plotOutput("bar",height = "400px", width="75%"),
               h1(""),
               h5("Click on the link for the documentation"),
               h5("https://iamalabhya.wordpress.com/2020/06/11/mean-distribution-of-the-dice-roll-simulation/")
             )
            
            )
    ),
    #Page 3
    tabPanel("Mean Distribution of Population with Normal distribution", fluid=T, 
      sidebarLayout(
    #Side Panel
     sidebarPanel(
        sliderInput("Sample_size1", "Size of the sample", min = 1, max=499, value =1),
        sliderInput("Sample_times1", "No. of times the sample is drawn", value= 25, min = 25, max = 1000),
        
        actionButton("Input1", "Done" )
        ),
     mainPanel( plotOutput("Hist1",height = "350px", width="70%"), plotOutput("Hist01",height = "350px", width="70%"),
                h1(""),
                h5("Click on the link for the documentation"),
                h5("https://iamalabhya.wordpress.com/2020/06/11/mean-distribution-of-my-friends-weight/")
      )
    )
  ),
    #Page4
    tabPanel("Mean Distribution of Population without Normal distribution", fluid=T, 
           sidebarLayout(
           #Side Panel
           sidebarPanel(
             sliderInput("Sample_size2", "Size of the sample", min = 1, max=499, value =1),
             sliderInput("Sample_times2", "No. of times the sample is drawn", value= 25, min = 25, max = 1000),
                 
            actionButton("Input2", "Done" )
            ),
      mainPanel(plotOutput("Hist2", height = "350px", width="70%"), plotOutput("Hist02",height = "350px", width="70%"),
                 h1(""),
                 h5("Click on the link for the documentation"),
                h5("https://iamalabhya.wordpress.com/2020/06/11/mean-distribution-of-my-friends-weight/")
             )
    )
  )
))

#Make Server

Server <- function(input, output, session) {
  
  #Page 1
  observeEvent(input$Dice, {
    #One Dice
    if(input$roll==1){
      output$histo <- renderPlot({
        barplot(table(1:6), col = "light blue")
      })
    }
    #Two Dice
    else if (input$roll == 2){
      output$histo <- renderPlot({
        die <- c(1:6)
        die <- expand.grid(die,die)
        die$average <- rowMeans(die)
        barplot(table(die$average), col= "light blue")
      })
    }
    #Three Dice
    else if (input$roll == 3){
      output$histo <- renderPlot({
        die <- c(1:6)
        die <- expand.grid(die,die,die)
        die$average <- rowMeans(die)
        barplot(table(die$average), col= "light blue")
      })
    }
    #Four Dice
    else if (input$roll == 4){
      output$histo <- renderPlot({
        die <- c(1:6)
        die <- expand.grid(die,die,die,die)
        die$average <- rowMeans(die)
        barplot(table(die$average), col= "light blue")
      })
    }
    #Five Dice
    else if (input$roll == 5){
      output$histo <- renderPlot({
        die <- c(1:6)
        die <- expand.grid(die,die,die,die,die)
        die$average <- rowMeans(die)
        barplot(table(die$average), col= "light blue")
      })
    }
    #Six Dice
    else if (input$roll == 6){
      output$histo <- renderPlot({
        die <- c(1:6)
        die <- expand.grid(die,die,die,die,die,die)
        die$average <- rowMeans(die)
        barplot(table(die$average), col= "light blue")
        abline(v=63, col= "red", lwd=3, lty= 2)
      })
    }
    
   
    })
  
  #Page 2
  output$bar <- renderPlot({
    SampleAverage=1
    n <- (1:6)
    for(i in 1:input$Die_times){
      SampleAverage[i] <- mean(sample(n, input$Die_size, replace =T))
      barplot(table(SampleAverage), col ="light blue")
    }
  })
  
  #For 3 and 4
  
  set.seed(259)
  Weight_In_Kilos_Population <- rnorm(500, mean = 63, sd = 8 )
  
  output$Hist1 <- renderPlot({
      hist(Weight_In_Kilos_Population, col = "light blue" )
  })
  
  set.seed(216)
  Weight_In_Kilo_Population  <- sample(54:82, 500, replace = T)
  
  output$Hist2 <- renderPlot({
        hist(Weight_In_Kilo_Population , col = "light blue")
  })
  
  #Part3
  observeEvent(input$Input1, {
  output$Hist01 <- renderPlot({
    Sample_Average <- 1
    for (i in 1:input$Sample_times1) {
      Sample_Average[i] <- mean(sample(Weight_In_Kilos_Population , input$Sample_size1, replace = F))
    }
    hist(Sample_Average, breaks = 10, col = "light green", xlim = c(39.18,87.10))
    abline(v=62.4, col= "red", lwd=3, lty= 2)
  })
  })
  
  #Part4
  observeEvent(input$Input2, {
  output$Hist02 <- renderPlot({
    Sample_average <- 1
    for (i in 1:input$Sample_times2) {
      Sample_average[i] <- mean(sample(Weight_In_Kilo_Population , input$Sample_size2, replace = F))
        }
    hist(Sample_average, breaks =10, col="light green", xlim = c(54,82))
    abline(v=68.66, col= "red", lwd=3, lty= 2)
  })
  })
 
  

}




shinyApp(ui =UI, server= Server)



