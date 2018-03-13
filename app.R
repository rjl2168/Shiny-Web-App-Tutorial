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
   titlePanel("Visualizing The Central Limit Theorem"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("SampleSize",
                     "Number of Random Variables:",
                     min = 20,
                     max = 50,
                     value = 1),

         sliderInput("NumSamples",
                     "Number of Random Samples Drawn from Distribution",
                     min = 1,
                     max = 50,
                     value = 30),
         
         selectInput(inputId = "distr",
                     label = "Distribution",
                     choices = c("Uniform", "Exponential", "Poisson", "Chi-Squared", "Binomial", "Student-t"),
                     selected = "Uniform")
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$distPlot <- renderPlot({
     Sample_size <- input$SampleSize
     Num_samples <- input$NumSamples
     for (i in 1:Num_samples) {
       samples <- switch(input$distr, 
                          "Uniform" = runif(Sample_size, min = 0, max = 10), 
                          "Exponential" = rexp(Sample_size, rate = 1),
                          "Poisson" = rpois(Sample_size, lambda = 1),
                          "Chi-Squared" = rchisq(Sample_size, df = 1),
                          "Binomial" = rbinom(Sample_size, size = 50, prob = 0.1),
                          "Student-t" = rt(Sample_size, df = 10))
       sample_mean[i] <- mean(samples)
     }

     sample_mean_df <- as.data.frame(sample_mean)
     ggplot(sample_mean_df, aes(V1)) + geom_histogram(aes(y = ..density..), stat = "bin", bins = 50) + geom_density()
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

