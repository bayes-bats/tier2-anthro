library(shiny)
library(bayesrules)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Introduction to Bayesian Inference"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        # First Panel: Set Prior Probability
        tabPanel("Set Prior",
                 h3("Select Question and Prior Beliefs"),
                 selectInput("initial_question", "Choose a Question:",
                             choices = list(
                               "Is the mandibular torus present or absent?" = "mandibular_torus",
                               "Does the archaeological site have colonial artifacts?" = "colonial_artifacts",
                               "Is English the primary language spoken at your home?" = "english_language",
                               "Are you a member of a fraternity or sorority on campus?" = "fraternity_membership"
                             )),
                 actionButton("submit_question", "Submit Question"),
                 
                 # Confidence question shown based on initial question
                 uiOutput("confidence_ui")
        ),
        
        # Second Panel: Enter Sample Data to Update Posterior
        tabPanel("Update with Data",
                 h3("Enter Observed Data"),
                 numericInput("sample_size", "Total Sample Size:", min = 1, value = 10),
                 numericInput("number_present", "Number Present:", min = 0, value = 3),
                 actionButton("submit_data", "Submit Data")
        ),
        
        # Third Panel: Adjust Prior Parameters
        tabPanel("Custom Prior",
                 h3("Different Prior Beliefs"),
                 sliderInput("custom_alpha", "Alpha:", min = 1, max = 20, value = 5, step = 0.5),
                 sliderInput("custom_beta", "Beta:", min = 1, max = 20, value = 5, step = 0.5),
                 actionButton("apply_custom_prior", "Apply Different Prior")
        )
      )
    ),
    
    mainPanel(
      # Display prior plot in the first panel
      conditionalPanel(
        condition = "input.submit_question > 0 && input.submit_data == 0",
        plotOutput("prior_plot")
      ),
      
      # Display the posterior plot in the second panel
      conditionalPanel(
        condition = "input.submit_data > 0",
        plotOutput("posterior_plot")
      ),
      
      # Display custom prior and posterior plot in the third panel
      conditionalPanel(
        condition = "input.apply_custom_prior > 0",
        plotOutput("custom_prior_posterior_plot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive to store prior parameters based on initial question and confidence
  prior_params <- reactiveVal(NULL)
  
  # Show confidence question after the initial question is submitted
  observeEvent(input$submit_question, {
    output$confidence_ui <- renderUI({
      req(input$initial_question)  # Ensure a question is selected
      selectInput("confidence_level", 
                  label = paste("How confident are you in your answer to:", input$initial_question), 
                  choices = c("Unsure" = "unsure", "Very Confident" = "very_confident", "Not Very Confident" = "not_confident"))
    })
  })
  
  # Set prior parameters based on confidence level
  observeEvent(input$confidence_level, {
    req(input$confidence_level)
    
    params <- switch(input$confidence_level,
                     "very_confident" = list(alpha = 7, beta = 2),
                     "not_confident" = list(alpha = 2, beta = 7),
                     "unsure" = list(alpha = 1, beta = 1))
    
    prior_params(params)  # Save prior params in reactive value
  })
  
  # Render the prior distribution plot
  output$prior_plot <- renderPlot({
    req(prior_params())  # Ensure prior parameters are set
    
    plot_beta_binomial(alpha = prior_params()$alpha, beta = prior_params()$beta) +
      xlab("Probability of Success")
  })
  
  # Render the posterior distribution plot after observed data is entered
  output$posterior_plot <- renderPlot({
    req(prior_params())  # Ensure prior parameters are set
    req(input$sample_size, input$number_present)  # Ensure observed data is entered
    
    # Total sample size (n) and number present (y)
    n <- input$sample_size
    y <- input$number_present
    
    plot_beta_binomial(alpha = prior_params()$alpha, beta = prior_params()$beta, n = n, y = y) +
      xlab("Probability of Success") + ggtitle("Updated Beliefs")

  })
  
  # Render the plot with custom prior and posterior
  output$custom_prior_posterior_plot <- renderPlot({
    req(input$custom_alpha, input$custom_beta, input$sample_size, input$number_present)
    
    # Custom prior parameters from sliders
    alpha_prior <- input$custom_alpha
    beta_prior <- input$custom_beta
    
    # Observed data
    n <- input$sample_size
    y <- input$number_present
    
    # Calculate posterior parameters using custom prior and observed data
    
    # Display the custom prior and posterior distributions
    plot_beta_binomial(alpha = alpha_prior, beta = beta_prior, n = n, y = y) +
      xlab("Probability of Success") + ggtitle("Different Prior")
  })
}

shinyApp(ui, server)
