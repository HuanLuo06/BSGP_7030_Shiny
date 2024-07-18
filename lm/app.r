#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a scatter plot and linear model
ui <- fluidPage(
  
  # Application title
  titlePanel("Scatter Plot and Linear Model"),
  
  # Sidebar with file input and action button
  sidebarLayout(
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Display head or all ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),

      # Action button to model the data ----
      actionButton("model", "Generate Linear Model")
    ),
    
    # Show a plot of the scatter plot and linear model, and text output for coefficients
    mainPanel(
      plotOutput("scatterPlot"),
      plotOutput("lmPlot"),
      verbatimTextOutput("modelInfo"),
      tableOutput("contents")
    )
  )
)

# Define server logic required to draw scatter plot and linear model
server <- function(input, output) {
  
  dataInput <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    return(df)
  })
  
  output$scatterPlot <- renderPlot({
    df <- dataInput()
    plot(df$x, df$y, xlab = "X", ylab = "Y", main = "Scatter Plot")
  })
  
  observeEvent(input$model, {
    output$lmPlot <- renderPlot({
      df <- dataInput()
      plot(df$x, df$y, xlab = "X", ylab = "Y", main = "Scatter Plot with Linear Model")
      lm_model <- lm(y ~ x, data = df)
      abline(lm_model, col = "red")
    })
    
    output$modelInfo <- renderPrint({
      df <- dataInput()
      lm_model <- lm(y ~ x, data = df)
      model_summary <- summary(lm_model)
      slope <- model_summary$coefficients[2, 1]
      intercept <- model_summary$coefficients[1, 1]
      correlation <- cor(df$x, df$y)
      list(
        Slope = slope,
        Intercept = intercept,
        `Correlation Coefficient` = correlation
      )
    })
  })
  
  output$contents <- renderTable({
    if (input$disp == "head") {
      return(head(dataInput()))
    } else {
      return(dataInput())
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
