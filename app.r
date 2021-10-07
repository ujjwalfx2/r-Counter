library(shiny)
library(shinydashboard)
library(googlesheets4)
library(ggplot2)
library(graphics)

#reading CSV filea
real_data=read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2SVfqRbNwsIzSgd0FOwJ3Rc-Mvpbc7GWwuZI0_DKXIhd4E83vc1PetPZSnnlmPrgrHtAF3Y3hSTjr/pub?output=csv", sep = ",", header = TRUE)

ui <- fluidPage(
  #background color change
  tags$style('
    .container-fluid {
      background-color: #008BA6;
    }       
  '),
  
  
  # Application title
  titlePanel("Reading-Display Google Sheet data"),
  sidebarPanel( 
    sidebarMenu(
      #input district name
      selectizeInput(
      "newvar", "Select District from DropDown menu:", choices = sort(unique(real_data$District.Name)), multiple = FALSE,options=list(placeholder="Birju")      )
    ),
    br(),
    tags$a(href="https://www.youtube.com/c/ujjwalfx", img(src='UJ.png', align = "middle")),
    br(),
    h4(strong("Youtube Channel Name:"), style = "font-size:20px;"),
    tags$a(href="https://www.youtube.com/c/ujjwalfx", "https://www.youtube.com/c/ujjwalfx"),
    h5(strong(textOutput("counter")))
  ),
  
  
  # Show a plot of the generated distribution
  
  mainPanel(
    
    
    textOutput('footfalltotal'),
    br(),
    plotOutput("plot1"),
    br(),
    plotOutput("plot2")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    par(bg = "#008BA6")
    plotdata=subset(real_data,real_data$District.Name==input$newvar)
    barplot(sort(plotdata$Footfall.Total),main =paste("Total Footfall = ",sum(plotdata$Footfall.Total,na.rm=TRUE)),border="green", col="purple", names.arg = plotdata$Footfall.Total)
  })
  
  output$plot2 <- renderPlot({
    par(bg = "#008BA6")
    plotdata=subset(real_data,real_data$District.Name==input$newvar)
    barplot(sort(plotdata$Patients.received.medicines),main =paste("Patient received medicines = ",sum(plotdata$Patients.received.medicines,na.rm=TRUE)),border="green", col="purple", names.arg = plotdata$Patients.received.medicines)
  })
  
  output$footfalltotal <- renderText({
    plotdata=subset(real_data,real_data$District.Name==input$newvar)
    sumtotal=paste("Total Footfall = ",sum(plotdata$Footfall.Total,na.rm=TRUE))
  })
  
  output$counter <- 
    renderText({
      if (!file.exists("counter.Rdata")) 
        counter <- 0
      else
        load(file="counter.Rdata")
      counter  <- counter + 1
      save(counter, file="counter.Rdata")     
      paste("Hits: ", counter)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)

