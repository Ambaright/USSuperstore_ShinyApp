library(shiny)
library(shinyalert)
library(tidyverse)
library(readxl)
library(ggplot2)
library(geofacet)

# Data to work with
store_data <- read_excel("US Superstore data.xls")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("US Superstore Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Investigate"),
      # Categorical variables
      checkboxGroupInput("categorical_vars",
                         "Categorical Variables",
                         choices = c("Ship Mode", "Segment", "State", 
                                     "Region", "Category", "Sub-Category"),
                         selected = c("Segment", "Region")
      ),
      
      # Conditional UI for displaying the options to filter by the selected
      # Selecting Numeric variable 1
      selectInput("numeric_one", 
                  "First Numeric Variable",
                  choices = c("Sales", "Quantity", "Discount", "Profit"),
                  selected = "Sales"
      ),
      
      # Dynamic slider for Numeric variable 1
      uiOutput("sliderOne"),
      
      # Selecting Numeric variable 2
      selectInput("numeric_two", 
                  "First Numeric Variable",
                  choices = c("Sales", "Quantity", "Discount", "Profit"),
                  selected = "Profit"
      ),
      
      # Dynamic slider for Numeric variable 2
      uiOutput("sliderTwo"),
      
      # Action button that when pressed, subsets the data according to the selections made on the sidebar
      actionButton("subset",
                   "Subset the data")
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      DT::DTOutput("data_table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Reactive expression to get selected ranges for sliders
  observe({
    numeric_var1 <- input$numeric_one
    output$sliderOne <- renderUI({
      req(numeric_var1)
      sliderInput("sliderOne_values",
                  paste("Select range for", numeric_var1),
                  min = min(store_data[[numeric_var1]]),
                  max = max(store_data[[numeric_var1]]),
                  value = c(min(store_data[[numeric_var1]]),
                            max(store_data[[numeric_var1]])))
    })
  })
  
  observe({
    numeric_var2 <- input$numeric_two
    output$sliderTwo <- renderUI({
      req(numeric_var2)
      sliderInput("sliderTwo_values",
                  paste("Select range for", numeric_var2),
                  min = min(store_data[[numeric_var2]]),
                  max = max(store_data[[numeric_var2]]),
                  value = c(min(store_data[[numeric_var2]]),
                            max(store_data[[numeric_var2]])))
    })
  })
  
  # update input boxes so they can't choose the same variable
  observeEvent(c(input$numeric_one, input$numeric_two), {
    numeric_one <- input$numeric_one
    numeric_two <- input$numeric_two
    choices <- c("Sales", "Quantity", "Discount", "Profit")
    if (numeric_one == numeric_two){
      choices <- choices[-which(choices == numeric_one)]
      updateSelectizeInput(session,
                           "numeric_two",
                           choices = choices)#we'll cover this kind of thing shortly!
    }
  })
  
  # Create a reactive object here to subset data appropriately
  filtered_data <- eventReactive(input$subset, {
    data <- store_data
    
    # Filter by selected numeric variables with their slider ranges
    
    numeric_var1 <- input$numeric_one
    range_var1 <- input$sliderOne_values
    data <- data |> filter(get(numeric_var1) >= range_var1[1] & get(numeric_var1) <= range_var1[2])
    
    numeric_var2 <- input$numeric_two
    range_var2 <- input$sliderTwo_values
    data <- data |> filter(get(numeric_var2) >= range_var2[1] & get(numeric_var2) <= range_var2[2])
    
    # Filter by selected categorical variables
    selected_cat_vars <- input$categorical_vars
    always_keep_vars <- c("Row ID", "Order ID", "Order Date", "Ship Date", "Customer ID", "Customer Name", 
                          "Product ID", "Product Name")
    data <- data |> select(all_of(c(always_keep_vars, selected_cat_vars, numeric_var1, numeric_var2))) |>
                    mutate(
                      !!numeric_var1 := round(get(numeric_var1), 3),
                      !!numeric_var2 := round(get(numeric_var2), 3)
                    )
                    
    
    # Return filtered data
    data
  })
  
  
  # Display filtered data
  observeEvent(input$subset, {
    output$data_table <- DT::renderDataTable({
      # Only update if the "Subset the data" button is clicked
      req(input$subset)
      
      print(filtered_data())
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
