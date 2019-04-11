library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Microarray Analysis"), #1
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a CSV file ----
      fileInput(
        "file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Input: Numeric entry for number of obs to view ----
      numericInput(
        inputId = "genenum",
                   label = "Number of Genes to Display ",
                   value = 10),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Adjusted P value slider ----
      sliderInput("adj.P.Val", "Adjusted P-value",
                  value = 0.05,
                  min = 0.0001,
                  max = 0.5),

      sliderInput("logFC", "log2FCvalue",
                  value = 1,
                  min = 0,
                  max = 10)),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption","Gene List",container = span)),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("table1"),
      
      # Output: Plot output for data summary ----
      plotOutput("volcano"))))


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) { 
      df <- reactive({
      
      req(input$file1)
      read.csv(input$file1$datapath,sep="\t")
      })
      
      output$caption <- renderText({
        input$caption
      })
        
      output$table1 <- renderTable({
      head(df(), n = input$genenum)
      })
      
      output$volcano <- renderPlot({
        
      sig <- df()[df()$adj.P.Val <= input$adj.P.Val & abs(df()$logFC) >=input$logFC,]
      print(sig)
      plot(df()$logFC, -log10(df()$adj.P.Val), pch="*", xlab="Log2 Fold Change", ylab="-10log (adjusted p-value)")
      abline(h= -log10(input$adj.P.Val), v=c(-input$logFC, input$logFC), col="red", lty=2)
      points(sig$logFC, -log10(sig$adj.P.Val), col="red", pch="*")
      })
}

# Create Shiny app ----
shinyApp(ui, server)