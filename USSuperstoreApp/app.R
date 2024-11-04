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
            # Numeric variable 1
            selectInput("numeric_one", 
                        "First Numeric Variable",
                        choices = c("Sales", "Quantity", "Discount", "Profit"),
                        selected = "Sales"
            ),
            
            # Conditional Panel for Numeric 1: Sales
            conditionalPanel(condition = "input.numeric_one == 'Sales'",
                             sliderInput("salesSlider1", "Sales Subset", 
                                         min = min(store_data$Sales), max = max(store_data$Sales), 
                                         value = c(min(store_data$Sales),max(store_data$Sales)))
            ),
            
            # Conditional Panel for Numeric 1: Quantity
            conditionalPanel(condition = "input.numeric_one == 'Quantity'",
                             sliderInput("quantitySlider1", "Quantity Subset", 
                                         min = min(store_data$Quantity), max = max(store_data$Quantity), 
                                         value = c(min(store_data$Quantity),max(store_data$Quantity)))
            ),
            
            # Conditional Panel for Numeric 1: Discount
            conditionalPanel(condition = "input.numeric_one == 'Discount'",
                             sliderInput("discountSlider1", "Discount Subset", 
                                         min = min(store_data$Discount), max = max(store_data$Discount), 
                                         value = c(min(store_data$Discount),max(store_data$Discount)))
            ),
            
            # Conditional Panel for Numeric 1: Profit
            conditionalPanel(condition = "input.numeric_one == 'Profit'",
                             sliderInput("profitSlider1", "Profit Subset", 
                                         min = min(store_data$Profit), max = max(store_data$Profit), 
                                         value = c(min(store_data$Profit),max(store_data$Profit)))
            ),
            
            # Numeric variable 2
            selectInput("numeric_two", 
                        "First Numeric Variable",
                        choices = c("Sales", "Quantity", "Discount", "Profit"),
                        selected = "Profit"
            ),
            
            # Conditional Panel for Numeric 2: Sales
            conditionalPanel(condition = "input.numeric_two == 'Sales'",
                             sliderInput("salesSlider2", "Sales Subset", 
                                         min = min(store_data$Sales), max = max(store_data$Sales), 
                                         value = c(min(store_data$Sales),max(store_data$Sales)))
            ),
            
            # Conditional Panel for Numeric 2: Quantity
            conditionalPanel(condition = "input.numeric_two == 'Quantity'",
                             sliderInput("quantitySlider2", "Quantity Subset", 
                                         min = min(store_data$Quantity), max = max(store_data$Quantity), 
                                         value = c(min(store_data$Quantity),max(store_data$Quantity)))
            ),
            
            # Conditional Panel for Numeric 2: Discount
            conditionalPanel(condition = "input.numeric_two == 'Discount'",
                             sliderInput("discountSlider2", "Discount Subset", 
                                         min = min(store_data$Discount), max = max(store_data$Discount), 
                                         value = c(min(store_data$Discount),max(store_data$Discount)))
            ),
            
            # Conditional Panel for Numeric 2: Profit
            conditionalPanel(condition = "input.numeric_two == 'Profit'",
                             sliderInput("profitSlider2", "Profit Subset", 
                                         min = min(store_data$Profit), max = max(store_data$Profit), 
                                         value = c(min(store_data$Profit),max(store_data$Profit)))
            ),
            
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
