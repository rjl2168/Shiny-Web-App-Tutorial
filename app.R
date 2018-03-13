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
         sliderInput("N",
                     "Number of Random Variables:",
                     min = 1,
                     max = 1000,
                     value = 1),
         
         selectInput(inputId = "distr",
                     label = "Distribution",
                     choices = c("Uniform", "Exponential", "Poisson", "Chi-Squared", "Binomial", "Student-t"),
                     selected = "Uniform")
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         plotOutput("qqnormPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Define the number of samples in each distribution
  N_samp <- 1000
  
   output$distPlot <- renderPlot({
     #Initialize the data frame to all zeros
     total_dist <- as.data.frame(matrix(0,N_samp,1))
     for (i in 1:input$N) {
       new_dist <- switch(input$distr, 
                          "Uniform" = runif(N_samp, min = 0, max = 10), 
                          "Exponential" = rexp(N_samp, rate = 1),
                          "Poisson" = rpois(N_samp, lambda = 1),
                          "Chi-Squared" = rchisq(N_samp, df = 1),
                          "Binomial" = rbinom(N_samp, size = 50, prob = 0.1),
                          "Student-t" = rt(N_samp, df = 10))
       total_dist <- total_dist + as.data.frame(new_dist)
     }
     #Render the plot
     ggplot(total_dist, aes(total_dist)) + geom_histogram(aes(y = ..density..)) + stat_bin(bins = 50) + geom_density()
     
   })
   
   output$qqnormPlot <- renderPlot({
     #Initialize the data frame to all zeros
     total_dist <- as.data.frame(matrix(0,N_samp,1))
     for (i in 1:input$N) {
       new_dist <- switch(input$distr, 
                          "Uniform" = runif(N_samp, min = 0, max = 10), 
                          "Exponential" = rexp(N_samp, rate = 1),
                          "Poisson" = rpois(N_samp, lambda = 1),
                          "Chi-Squared" = rchisq(N_samp, df = 1),
                          "Binomial" = rbinom(N_samp, size = 50, prob = 0.1),
                          "Student-t" = rt(N_samp, df = 10))
       total_dist <- total_dist + as.data.frame(new_dist)
     }
     #Render the plot
     qqnorm(total_dist$total_dist[,N_samp])
     qqline()
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

