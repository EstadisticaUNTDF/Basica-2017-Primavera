#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(mosaic)

load(url("https://stat.duke.edu/~mc301/data/ames.RData"))

shinyApp(
  ui <- fluidPage(
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
        selectInput("selected_var",
                    "Variable:",
                    choices = list("area", "price"),
                    selected = "area"),         
        
        numericInput("n_samp",
                     "Tamaño de muestra:",
                     min = 1,
                     max = nrow(ames),
                     value = 30),
        
        numericInput("n_sim",
                     "Número de muestras:",
                     min = 1,
                     max = 15000,
                     value = 5000) 
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("sampling_plot"),
        verbatimTextOutput("sampling_mean"),
        verbatimTextOutput("sampling_se")
      )
    )
  ),
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    # create sampling distribution
    sampling_dist <- reactive({
      ames[[input$selected_var]] %>%
        sample(size = input$n_samp * input$n_sim, replace = TRUE) %>%
        matrix(ncol = input$n_samp) %>%
        rowMeans() %>%
        data.frame(x_bar = .)
    })
    
    # plot sampling distribution
    output$sampling_plot <- renderPlot({
      x_min <- quantile(ames[[input$selected_var]], 0.1, names = FALSE)
      x_max <- quantile(ames[[input$selected_var]], 0.9, names = FALSE)
      
      ggplot(sampling_dist(), aes(x = x_bar)) +
        geom_histogram(binwidth = 30) +
        xlim(x_min, x_max) +
        ylim(0, input$n_sim * 0.35) +
        ggtitle(paste0("Distribución muestral de la media de ", 
                       input$selected_var, " (n = ", input$n_samp, ")")) +
        ylab("Frecuencia") +
        xlab(paste("Media", input$selected_var)) +
        theme(plot.title = element_text(face = "bold", size = 16))
    })
    
    # mean of sampling distribution
    output$sampling_mean <- renderText({
      paste0("media de la distribución muestral = ", round(mean(sampling_dist()$x_bar), 2))
    })
    
    # mean of sampling distribution
    output$sampling_se <- renderText({
      paste0("Error Standard de la distribución muestral = ", round(sd(sampling_dist()$x_bar), 2))
    })
  }
)