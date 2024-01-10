library(shiny)
library(plotly)
library(ggplot2)
library(gridExtra)
library(shinythemes)

# Data
data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# Load the regression model
model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data) # Assuming 'data' is your dataset

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(tags$style(HTML('
    body {
      background-color: #ffc0cb; /* Warna pink */
    }
  '))),
  
  # Monthly Sales Volume Analysis Dashboard
  navbarPage(
    title = "Dashboard",
    
    # Variable Relationship
    tabPanel("Variable Relationship",
             fluidPage(
               titlePanel("Variable Relationship Analysis"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("variable", "Select Variable:", choices = colnames(data)),
                   actionButton("plot", "Generate Plot", class = "btn-primary")
                 ),
                 mainPanel(
                   plotOutput("scatter_boxplot", height = "800px")
                 )
               )
             )
    ),
    
    # Prediction Dashboard
    tabPanel("Sales Analysis",
             fluidPage(
               titlePanel("Monthly Sales Volume Analysis"),
               sidebarLayout(
                 sidebarPanel(
                   numericInput("x1_input", "Number of Website Visitors:", value = 270000),
                   numericInput("x2_input", "Number of Monthly Transactions:", value = 16000),
                   numericInput("x3_input", "Average Items per Transaction:", value = 6),
                   numericInput("x4_input", "Customer Satisfaction Rating (1-10):", value = 9.2),
                   numericInput("x5_input", "Number of Online Advertisements:", value = 62000),
                   actionButton("analyze_button", "Analyze Sales", class = "btn-primary")
                 ),
                 
                 mainPanel(
                   plotOutput("sales_plot"),
                   verbatimTextOutput("analysis_output")
                 )
               )
             )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Variable Relationship
  observeEvent(input$plot, {
    variable <- input$variable
    
    output$scatter_boxplot <- renderPlot({
      
      scatter_plot <- ggplot(data, aes_string(x = variable, y = "y")) +
        geom_point(color = "#3498DB", size = 3) +
        geom_smooth(method = "lm", se = FALSE, color = "#FF5722") +
        labs(x = variable, y = "y", title = paste("Scatter Plot of", variable, "vs. y")) +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#333333"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              legend.position = "none")
      
      box_plot <- ggplot(data, aes_string(x = "y", y = variable)) +
        geom_boxplot(fill = "#3498DB") +
        labs(x = "y", y = variable, title = paste("Boxplot of", variable)) +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#333333"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12))
      
      grid.arrange(scatter_plot, box_plot, nrow = 2)
      
    })
  })
  
  # Prediction Dashboard
  observeEvent(input$analyze_button, {
    new_data <- data.frame(
      x1 = input$x1_input,
      x2 = input$x2_input,
      x3 = input$x3_input,
      x4 = input$x4_input,
      x5 = input$x5_input
    )
    
    # Predicted monthly sales volume
    predicted_sales <- predict(model, newdata = new_data)
    predicted_sales <- abs(predicted_sales)
    predicted_sales <- round(predicted_sales)
    
    output$sales_plot <- renderPlot({
      # Scatter plot for monthly sales volume prediction results
      plot(data$x1, data$y, col = "#3498DB", xlab = "Number of Website Visitors", ylab = "Monthly Sales Volume (in thousands of USD)", main = "Scatterplot of Predicted Monthly Sales Volume", xlim = c(120000, 300000), ylim = c(0, 500))
      points(new_data$x1, predicted_sales, col = "red", pch = 16)
      legend("topright", legend = c("Actual Data", "Predicted Data"), col = c("#3498DB", "red"), pch = c(1, 16))
    })
    
    output$analysis_output <- renderPrint({
      cat("Predicted Monthly Sales Volume: ", predicted_sales, " (in thousands of USD)\n")
      
      # Displaying regression coefficients
      coefficients <- coef(model)
      
      cat("\nRegression Coefficients:\n")
      for (i in seq_along(coefficients)) {
        cat(names(coefficients)[i], ": ", coefficients[i], "\n")
      }
      
      # Identifying the variable with the highest positive coefficient
      most_influential_variable <- names(coefficients)[which.max(coefficients)]
      
      cat("\nThe variable to focus on improving: ", most_influential_variable, "\n")
      
      # Adding interaction between x1 (number of website visitors) and x5 (number of ads)
      model_with_interaction <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x1:x5, data = data)
      
      # Evaluating significance of interaction
      interaction_p_value <- summary(model_with_interaction)$coefficients["x1:x5", "Pr(>|t|)"]
      
      cat("\nInteraction p-value between Number of Website Visitors (x1) and Number of Ads (x5): ", interaction_p_value, "\n")
      
      if (interaction_p_value < 0.05) {
        cat("There is a significant interaction between x1 and x5 affecating sales.\n")
      } else {
        cat("There is no significant interaction between x1 and x5 affecting sales.\n")
      }
    })
  })
}

# Run the app
shinyApp(ui = ui, server=server)
