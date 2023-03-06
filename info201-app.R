library(shinyjs)
library(shiny)
library(data.table)
library(ggplot2)

library(readr)
Customers <- read_csv("Customers.csv")

ui <- fluidPage(
  useShinyjs(), 
  
  sidebarLayout(
    sidebarPanel(
      id="sidebar",
      selectInput("var_1", "Variable 1", choices = c("Age", "AnnualIncome", "SpendingScore", "WorkExperience", "FamilySize")),
      selectInput("var_2", "Variable 2", choices = c("AnnualIncome", "SpendingScore", "WorkExperience", "FamilySize")),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About",
          p('Shop Customer Data is a detailed analysis of a imaginative shop ideal customers. It helps a business to better understand its customers. The owner of a shop gets information about Customers through membership cards. '),
          p('Dataset consists of 2000 records and 8 columns:'),
          tags$ul(
            tags$li('Customer ID'),
            tags$li('Gender'),
            tags$li('Age'),
            tags$li('Annual Income'),
            tags$li('Spending Score'),
            tags$li('Profession'),
            tags$li('Work Experience'),
            tags$li('Family Size')
          ),
          renderTable(Customers)
        ),
        tabPanel(
          "Plot",
          plotOutput("plot_result")
        ),
        tabPanel(
          "Table",
          tableOutput("table_result")
        ),
        id = "tabset"
      ), 
      id="main"
    )
  )
)

draw_plot <- function(data_input, num_var_1, num_var_2, fact_var){
  data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
  ggplot(data = data_input, aes_string(x = num_var_1, y = num_var_2, color = fact_var)) + geom_point()
}

server <- function(input, output){
  
  observeEvent(input[["tabset"]], {
    if(input[["tabset"]] == "About"){
      hideElement(selector = "#sidebar")
      removeCssClass("main", "col-sm-8")
      addCssClass("main", "col-sm-12")
    }
    if(input[["tabset"]] == "Plot"){
      showElement(selector = "#sidebar")
      removeCssClass("main", "col-sm-12")
      addCssClass("main", "col-sm-8")
      

      compute_plot <- reactive({
          var_1 <- input$var_1
          var_2 <- input$var_2
          ggplot(Customers, aes_string(x = var_1, y = var_2))+
            labs(title = "From diamonds dataset")+
            geom_point()
      })
      
      output$plot_result <- renderPlot({
        compute_plot();
      })
      
    }
    
    if(input[["tabset"]] == "Table"){
      showElement(selector = "#sidebar")
      removeCssClass("main", "col-sm-12")
      addCssClass("main", "col-sm-8")
      
      compute_table <- reactive({
        var_1 <- input$var_1
        var_2 <- input$var_2
        df <- Customers[, c(var_1, var_2)]
      })
      
      output$table_result <- renderTable({
        compute_table();
      })
    }
  })
  
}

shinyApp(ui = ui, server = server)