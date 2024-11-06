library(shiny)
library(shinyalert)
library(tidyverse)
library(readxl)
library(ggplot2)
library(geofacet)
library(DT)
library(shinyjs)

# Data to work with
store_data <- read_excel("US Superstore data.xls")

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  
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
      tabsetPanel(
        
          # About Tab
          tabPanel("About",
                 h3("About this App"),
                 h4("App Purpose:"),
                 p("This Shiny application allows the users to filter the US Superstore dataset based on various
                   categorical and numeric variables. The app allows the users to subset the data interactively 
                   and view the filtered results through a downloadable data table format or through contingency
                   tables and graphs."),
                 h4("Data Description:"),
                 p("The data used in this app comes from the US Superstore dataset, which includes information on
                   product purchases for e-commerce platforms, including selling price and profit of products and 
                   information on the customer and type of product purchased."),
                 a("See more information about the US Superstore data at: 
                   https://www.kaggle.com/datasets/juhi1994/superstore?resource=download",
                   href = "https://www.kaggle.com/datasets/juhi1994/superstore?resource=download"),
                 h4("Sidebar Purpose:"),
                 p("The purpose of the sidebar is to allow users to select the categories and numeric variables 
                   that they want to filter the data by. At least two categorical variables must be selected to
                   be able to filter the data. The 'Subset the data' button allows the user to apply these
                   selections to subset the data."),
                 p("In the 'Data Download' tab you can view the subsetted data in a table format and you have 
                 the option to download the data as a CSV file at the end of the tab. In the 'Data Exploration'
                   tab, you can obtain numeric and graphical summaries for selected variables."),
                 img(src = "https://www.google.com/url?sa=i&url=https%3A%2F%2Fseeklogo.com%2Fvector-logo%2F331692%2Fsuperstore&psig=AOvVaw1x6Q2GqSs3ic_fLK9HNy3g&ust=1731002472859000&source=images&cd=vfe&opi=89978449&ved=0CBQQjRxqFwoTCODMnu-kyIkDFQAAAAAdAAAAABAE", height = 100, width = 300)
        
          ),
        
          # Data Download Tab
          tabPanel("Data Download",
                   # Display categorical error message if fewer than 2 categorical variables were selected
                   textOutput("cat_error_message"),
                   
                   # Display table output
                   DT::DTOutput("data_table"),
                   
                   # Download Button
                   downloadButton("download_data", "Download Data")
          ),
          
          # Data Exploration Tab
          tabPanel("Data Exploration",
              tabsetPanel(
                  # Numeric Summary Tab
                  tabPanel("Numeric Summaries",
                           # Display categorical error message if fewer than 2 categorical variables were selected
                           textOutput("cat_error_message"),
                           selectInput("num_summary_var",
                                       "Select Numeric Variable for Summary:",
                                       choices = NULL),
                           uiOutput("group_by_var"),
                           shinycssloaders::withSpinner(DTOutput("numeric_summary_table"))
                  ),
                  # Categorical Summary Tab
                  tabPanel("Categorical Summaries",
                           # Display categorical error message if fewer than 2 categorical variables were selected
                           textOutput("cat_error_message"),
                           selectInput("cat_summary_var",
                                       "Select Categorical Variable(s) for Summary:",
                                       choices = NULL, multiple = TRUE),
                           shinycssloaders::withSpinner(DTOutput("categorical_summary_table")))
              )
          )
      )
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
    
    # Check that at least two categorical variables are selected
    if(length(input$categorical_vars) < 2) {
      return(NULL)
    }
    
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
    always_keep_vars <- c("Order ID", "Order Date", "Ship Date", "Customer ID", "Customer Name", 
                          "Product ID", "Product Name")
    data <- data |> select(all_of(c(always_keep_vars, selected_cat_vars, numeric_var1, numeric_var2))) |>
                    mutate(
                      !!numeric_var1 := round(get(numeric_var1), 3),
                      !!numeric_var2 := round(get(numeric_var2), 3)
                    )
                    
    
    # Return filtered data
    data
  })
  
  # Dynamically update choices for categorical summary based on selected categorical variables
  observe({
    updateSelectInput(session, "cat_summary_var", choices = input$categorical_vars, selected = input$categorical_vars[1])
  })
  
  # Render contingency table
  output$categorical_summary_table <- renderDT({
    req(input$cat_summary_var)
    
    # Get selected categorical variables
    selected_vars <- input$cat_summary_var
    
    # Generate contingency table based on the number of selected variables
    if(length(selected_vars) == 1) {
      table_data <- table(filtered_data()[[selected_vars]])
      table_df <- as.data.frame(table_data)
      colnames(table_df) <- c(selected_vars, "Count")
      
    } else if(length(selected_vars) == 2) {
      table_data <- table(filtered_data()[[selected_vars[1]]], filtered_data()[[selected_vars[2]]])
      table_df <- as.data.frame(table_data)
      colnames(table_df) <- c(selected_vars[1], selected_vars[2], "Count")
      
    } else {
      return(NULL)
    }
    
    table_df
  })
  
  # Dynamically update choices for group_by input based on selected categorical variables
  observeEvent(input$subset, {
    filtered_data_cols <- colnames(filtered_data())
    
    categorical_in_data <- intersect(filtered_data_cols, c("Ship Mode", "Segment", "State", 
                                                           "Region", "Category", "Sub-Category"))
    
    updateSelectInput(session, "group_by", choices = categorical_in_data)
    
  })

  # Dynamic UI for selecting a categorical variable to group by
  output$group_by_var <- renderUI({
    req(input$num_summary_var)
    
    # Only show categorical variables that are selected in the sidebar
    selectInput("group_by", "Group by Categorical Variable:",
                choices = input$categorical_vars,
                selected = input$categorical_vars[1])
  })
  
  # Dynamically update choices for numeric variable summary
  observe({
    numeric_choices <- c(input$numeric_one, input$numeric_two)
    updateSelectInput(session,  "num_summary_var", choices = numeric_choices)
  })
  
  # Data Summaries: Numeric Variable Summaries (Mean, Median, SD, Max, Min)
  output$numeric_summary_table <- renderDT({
    req(input$num_summary_var, input$group_by)
    
    num_var <- input$num_summary_var
    cat_var <- input$group_by
    
    summary_data <- filtered_data()
    
    if(is.null(summary_data)) {
      return(NULL)
    }
    
    summary_stats <- summary_data |>
      group_by_at(cat_var) |>
      summarize(
        Mean = round(mean(get(num_var)),2),
        Median = round(median(get(num_var)),2),
        SD = round(sd(get(num_var)),2),
        IQR = round(IQR(get(num_var)),2),
        Min = round(min(get(num_var)),2),
        Max = round(max(get(num_var)),2)
      )
    
    summary_stats
        
  })
  
  # Disable Subset Action Button if fewer than 2 categorical variables
  observe({
    if(length(input$categorical_vars) < 2) {
      shinyjs::disable("subset")
    } else {
      shinyjs::enable("subset")
    }
  })
  
  # Display error message if fewer than 2 categorical variables were selected
  output$cat_error_message <- renderText({
    if(length(input$categorical_vars) < 2) {
      return("You must select at least two categorical variables!")
    }
    return(NULL)
  })
  
  
  # Display filtered data
  observeEvent(input$subset, {
    output$data_table <- DT::renderDataTable({
      # Only update if the "Subset the data" button is clicked
      req(input$subset)
      
      data <- filtered_data()
      
      # If the data is NULL bc there are less than 2 categorical variables, show no table
      if(is.null(data)) {
        return(NULL)
      }
      
      data
    })
  })
  
  # Data Download functionality
  output$download_data <- downloadHandler(
    filename <- function() {
      paste("superstore_data_subset.csv")
    },
    content = function(file) {
      data <- filtered_data()
      if(is.null(data)) return(NULL)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
