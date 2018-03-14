#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Sample Distribution Example"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "u",
                     label = "Mean",
                     choices = c(-10, -5, 0, 5, 10),
                     selected = 0),

         selectInput(inputId = "std",
                    label = "Standard Deviation",
                    choices = c(1, 5, 10, 20),
                    selected = 1),
         
         radioButtons(inputId = "NumSamp",
                      label = "Number of Samples",
                      choices = c(20, 100, 500, 1000),
                      selected = 100),

         sliderInput(inputId = "NumBins",
                     label = "Number of Bins in Histogram",
                     min = 1,
                     max = 50,
                     value = 30),
         
         actionButton(inputId = "showStats",
                      label = "Print Stats")
         
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         verbatimTextOutput("stat_summary")
#         textOutput("MeanValue"),
#         textOutput("stdValue")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   sample_data <- reactive({
     Sample_size <- as.numeric(input$NumSamp)
     Avg <- as.numeric(input$u)
     Std_dev <- as.numeric(input$std)
     rnorm(n = Sample_size, mean = Avg, sd = Std_dev)
   })

   output$distPlot <- renderPlot({
     Num_bins <- input$NumBins
     samples_df <- as.data.frame(sample_data())
     ggplot(samples_df, aes(samples_df)) + geom_histogram(aes(y = ..density..), stat = "bin", bins = Num_bins) + geom_density()
     
   })

   output$stat_summary <- renderPrint({
     summary(sample_data())
   })   
#   observeEvent(input$showStats, {
#     output$meanValue <- renderText({toString(mean(samples))})
#     output$stdValue <- renderText({toString(sd(samples))})
#   })

}

# Run the application 
shinyApp(ui = ui, server = server)

