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
      checkboxGroupInput("categorical",
                         "Categorical Variables",
                         choices = c("Ship Mode", "Customer Name", "Segment", "City", "State", 
                                     "Region", "Category", "Sub-Category"),
                         selected = c("Segment", "Region")
      ),
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
      actionButton("subset_true",
                   "Subset the data"),
      
      #Delete later
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
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
  data <- reactive({
    store_data
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
