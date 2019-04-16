

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Investment Estimators"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(4,sliderInput("initial",
                         "Initial Amount:",
                         min = 1,
                         max = 100000,
                         value = 1000)
           ),
    column(4,sliderInput("annual",
                         "Annual Contribution:",
                         min = 0,
                         max = 50000,
                         value = 2000)
           ),
    column(4,sliderInput("return",
                         "Return Rate (in %)",
                         min = 0,
                         max = 20,
                         value = 5)
           ),
    column(4,sliderInput("growth",
                         "Growth Rate (in %)",
                         min = 0,
                         max = 20,
                         value = 2)
           ),
    column(4,sliderInput("year",
                         "Years",
                         min = 0,
                         max = 50,
                         value = 10)
      
    ),
    column(4,selectInput("fac","Facet?",c("no","yes"))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      h4("Timelines"),
      plotOutput("distPlot"),
      h4("Balances"),
      verbatimTextOutput("table")

      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    future_value <- function(amount,rate,years){
      FV <- amount*(1+rate)^years
      return(FV)
    }
    annuity <- function(contrib,rate,years){
      FVA <- contrib*((1+rate)^years-1)/rate
      return(FVA)
    }
    growing_annuity <- function(contrib,rate,growth,years){
      FVGA <- contrib*(((1+rate)^years-(1+growth)^years))/(rate-growth)
      return(FVGA)
    }
    
    year <- c(0:input$year)
    no_contrib <- c()
    fixed_contrib <- c()
    growing_contrib <- c()
    
    for (i in 0:input$year){
      no_contrib[i+1] <- future_value(amount=input$initial,rate=input$return/100,years=i)
      fixed_contrib[i+1] <- annuity(contrib=input$annual,rate=input$return/100,years=i) + no_contrib[i+1]
      growing_contrib[i+1] <- growing_annuity(contrib=input$annual,rate=input$return/100,growth=input$growth/100,years=i) + no_contrib[i+1]
    }
    no_contrib <- round(no_contrib,2)
    fixed_contrib <- round(fixed_contrib,2)
    growing_contrib <- round(growing_contrib,2)
    
    modalities <- data.frame(year,no_contrib,fixed_contrib,growing_contrib)
    modalities
    
    a <- data.frame(year,values=no_contrib,type=rep("no contrib",input$year+1))
    b <- data.frame(year,values=fixed_contrib,type=rep("fixed contrib",input$year+1))
    c <- data.frame(year,values=growing_contrib,type=rep("growing contrib",input$year+1))
    new_mod <- data.frame(rbind(a,b,c))
    
    graph1 <- ggplot(data=modalities,aes(x=year))+
      geom_line(aes(y=no_contrib,col="line1")) + geom_point(aes(y=no_contrib,col="line1")) +
      geom_line(aes(y=fixed_contrib,col="line2")) + geom_point(aes(y=fixed_contrib,col="line2")) +
      geom_line(aes(y=growing_contrib,col="line3")) + geom_point(aes(y=growing_contrib,col="line3")) +
      labs(title="Three modes of investing",x="Year",y="Return(in dollars)") +
      scale_colour_manual(name="variable",labels=c("no_contribution","fixed_contribution","growing_contribution"),values=c("red","green","blue"))
    
    graph2 <- ggplot(data=new_mod,aes(x=year)) +
      geom_line(aes(y=values,col=type)) +
      geom_point(aes(y=values,col=type)) +
      geom_area(aes(y=values,fill=type),alpha=0.5) +
      facet_wrap(~type) +
      theme_bw() +
      labs(title="Three modes of investing",x="year",y="value")
      
    facet <- input$fac
    if (facet=="no"){
      graph1
    }
    else{
      graph2
    }
  })
  output$table <- renderPrint({
    future_value <- function(amount,rate,years){
      FV <- amount*(1+rate)^years
      return(FV)
    }
    annuity <- function(contrib,rate,years){
      FVA <- contrib*((1+rate)^years-1)/rate
      return(FVA)
    }
    growing_annuity <- function(contrib,rate,growth,years){
      FVGA <- contrib*(((1+rate)^years-(1+growth)^years))/(rate-growth)
      return(FVGA)
    }
    
    year <- c(0:input$year)
    no_contrib <- c()
    fixed_contrib <- c()
    growing_contrib <- c()
    
    for (i in 0:input$year){
      no_contrib[i+1] <- future_value(amount=input$initial,rate=input$return/100,years=i)
      fixed_contrib[i+1] <- annuity(contrib=input$annual,rate=input$return/100,years=i) + no_contrib[i+1]
      growing_contrib[i+1] <- growing_annuity(contrib=input$annual,rate=input$return/100,growth=input$growth/100,years=i) + no_contrib[i+1]
    }
    no_contrib <- round(no_contrib,2)
    fixed_contrib <- round(fixed_contrib,2)
    growing_contrib <- round(growing_contrib,2)
    
    modalities <- data.frame(year,no_contrib,fixed_contrib,growing_contrib)
    modalities
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)

