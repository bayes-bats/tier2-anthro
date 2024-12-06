library(shiny)
library(bayesrules)

ui <- fluidPage(
  titlePanel("Fun with Bayes"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tab_selected",
                  
                  # First Panel: Set Prior Probability
                  tabPanel("Set Prior",
                           h3("Select Question and Adjust Prior"),
                           selectInput("initial_question", "Choose a Question:",
                                       choices = list(
                                         "Is the mandibular torus present or absent?" = "mandibular_torus",
                                         "Does the archaeological site have colonial artifacts?" = "colonial_artifacts",
                                         "Is English the primary language spoken at your home?" = "english_language",
                                         "Are you a member of a fraternity or sorority on campus?" = "fraternity_membership"
                                       )),
                           actionButton("submit_question", "Submit Question"),
                           
                           # Sliders for custom prior parameters
                           h4("Adjust Prior Parameters"),
                           sliderInput("custom_alpha", "Alpha:", min = 1, max = 20, value = 5, step = 0.5),
                           sliderInput("custom_beta", "Beta:", min = 1, max = 20, value = 5, step = 0.5)
                  ),
                  
                  # Second Panel: Enter Sample Data to Update Posterior
                  tabPanel("Update with Data",
                           h3("Enter Observed Data"),
                           numericInput("sample_size", "Number of Observations (n):", min = 1, value = 10),
                           numericInput("number_present", "Number of Positive Outcomes (y):", min = 0, value = 3),
                           actionButton("submit_data", "Submit Data")
                  ),
                  
                  # Third Panel: Explore Large Sample Sizes
                  tabPanel("Explore Large Sample Sizes",
                           h3("Simulate Large Sample Sizes"),
                           sliderInput("large_sample_size", "Sample Size (n):", min = 10, max = 1000, value = 100, step = 10),
                           numericInput("proportion_positive", "Proportion of Positive Outcomes (y/n):", min = 0, max = 1, value = 0.5, step = 0.01),
                           actionButton("simulate_large_sample", "Simulate")
                  )
      )
    ),
    
    mainPanel(
      # Display prior plot in the first panel
      conditionalPanel(
        condition = "input.tab_selected == 'Set Prior' && input.submit_question > 0",
        plotOutput("prior_plot")
      ),
      
      # Display the posterior plot in the second panel
      conditionalPanel(
        condition = "input.tab_selected == 'Update with Data' && input.submit_data > 0",
        plotOutput("posterior_plot")
      ),
      
      # Display the large sample size plot in the third panel
      conditionalPanel(
        condition = "input.tab_selected == 'Explore Large Sample Sizes' && input.simulate_large_sample > 0",
        plotOutput("large_sample_plot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive to store prior parameters based on the sliders
  prior_params <- reactive({
    list(alpha = input$custom_alpha, beta = input$custom_beta)
  })
  
  # Reset inputs when switching between tabs
  observeEvent(input$tab_selected, {
    if (input$tab_selected == "Update with Data") {
      updateNumericInput(session, "sample_size", value = 10)
      updateNumericInput(session, "number_present", value = 3)
    }
    if (input$tab_selected == "Explore Large Sample Sizes") {
      updateSliderInput(session, "large_sample_size", value = 1000)
      updateNumericInput(session, "proportion_positive", value = 0.5)
    }
  })
  
  # Render the prior distribution plot
  output$prior_plot <- renderPlot({
    req(prior_params())  # Ensure prior parameters are set
    
    plot_beta_binomial(alpha = prior_params()$alpha, beta = prior_params()$beta) +
      xlab("Probability") + theme_minimal()
  })
  
  # Render the posterior distribution plot after observed data is entered
  output$posterior_plot <- renderPlot({
    req(prior_params())
    req(input$sample_size, input$number_present)
    
    n <- input$sample_size
    y <- input$number_present
    
    plot_beta_binomial(alpha = prior_params()$alpha, beta = prior_params()$beta, n = n, y = y) +
      xlab("Probability") + theme_minimal()
  })
  
  # Render the large sample size exploration plot
  output$large_sample_plot <- renderPlot({
    req(prior_params())
    req(input$large_sample_size, input$proportion_positive)
    
    n_large <- input$large_sample_size
    y_large <- round(input$proportion_positive * n_large)
    
    alpha_large <- prior_params()$alpha 
    beta_large <- prior_params()$beta 
    
    plot_beta_binomial(alpha = alpha_large, beta = beta_large, n = n_large, y = y_large) +
      ggtitle(paste("Posterior with Large Sample Size\nn =", n_large, "| y =", y_large)) +
      xlab("Probability") + theme_minimal()
  })
}

shinyApp(ui, server)
