# Function to install and load a package if not already installed
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("shiny","shinydashboard","ggplot2","dplyr","plotly","tidyr","this.path", "readr","sf")

# Install and load packages
lapply(packages, install_and_load)

# Dynamic File Path
cur_dir = dirname(this.path())
setwd(cur_dir)
getwd()

# Load the cleaned dataset
data_science_path <- file.path(cur_dir, "AustraliaDataScienceJobs.csv")
data <- read.csv(data_science_path, sep=";")

# Define UI for the Shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Data Science Jobs Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demand and Salary Analysis", tabName = "demand_salary", icon = icon("bar-chart")),
      menuItem("Skills Analysis", tabName = "skills", icon = icon("cogs")),
      menuItem("Company Comparisons", tabName = "company_comparisons", icon = icon("balance-scale"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab: Demand and Salary Analysis
      tabItem(tabName = "demand_salary",
              fluidRow(
                box(
                  title = "Filter by Sector",
                  selectInput("sectorInput", "Select Sector:", choices = unique(data$Company.Sector), selected = unique(data$Company.Sector)[1]),
                  width = 3
                )
              ),
              fluidRow(
                box(plotlyOutput("demandPlot"), width = 12),
                box(plotlyOutput("salaryPlot"), width = 12)
              )
      ),
      # Second tab: Skills Analysis
      tabItem(tabName = "skills",
              fluidRow(
                box(
                  title = "Filter by Sector",
                  selectInput("sectorSkillInput", "Select Sector:", choices = unique(data$Company.Sector), selected = unique(data$Company.Sector)[1]),
                  width = 3
                )
              ),
              fluidRow(
                box(plotlyOutput("skillsPlot"), width = 12)
              )
      ),
      # Third tab: Company Comparisons
      tabItem(tabName = "company_comparisons",
              fluidRow(
                box(
                  title = "Choose Filter Type",
                  selectInput("filterType", "Choose Filter:", 
                              choices = c("Filter by Sector", "Filter by Industry", "Filter by Company Size"), 
                              selected = "Filter by Sector"),
                  width = 3
                ),
                box(
                  title = "Choose Sort Type",
                  selectInput("sortType", "Choose Sort:", 
                              choices = c("Sort by Career Opportunities", "Sort by Culture and Values", "Sort by Senior Management", "Sort by Work Life Balance", "Sort by Compensation and Benefits"), 
                              selected = "Sort by Career Opportunities"),
                  width = 3
                )
              ),
              fluidRow(
                box(plotlyOutput("companyFactorsPlot"), width = 12)
              )
      )
    )
  )
)

# Define server logic for the Shiny dashboard
server <- function(input, output, session) {
  # Reactive data based on filters for demand and salary analysis
  filteredData <- reactive({
    data %>%
      filter(Company.Sector == input$sectorInput)
  })
  
  # Define the desired order
  size_levels <- c("1 to 50 Employees", "51 to 200 Employees", "201 to 500 Employees", "501 to 1000 Employees", "1001 to 5000 Employees", "5001 to 10000 Employees", "10000+ Employees")
  
  # Plot for job demand with grouped bar chart
  output$demandPlot <- renderPlotly({
    filtered_data <- filteredData()
    
    # Convert 'Company.Size' to a factor with the desired levels
    filtered_data$Company.Size <- factor(filtered_data$Company.Size, levels = size_levels)
    
    p <- ggplot(filtered_data, aes(x = Company.Size, fill = Company.Industry)) +
      geom_bar(position = "dodge") +
      labs(title = "Job Demand by Industry and Company Size", x = "Company Size", y = "Number of Jobs") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Plot for salary distribution with violin plots
  output$salaryPlot <- renderPlotly({
    filtered_data <- filteredData()
    p <- ggplot(filtered_data, aes(x = Company.Industry, y = Estimate.Base.Salary, fill = Company.Industry)) +
      geom_violin() +
      labs(title = "Salary Distribution by Industry", x = "Industry", y = "Estimated Base Salary") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma) + # Use scales::comma to format y-axis labels
      scale_fill_brewer(palette = "Set3")
    ggplotly(p)
  })
  
  # Reactive data based on filters for skills analysis
  filteredSkillData <- reactive({
    data %>%
      filter(Company.Sector == input$sectorSkillInput)
  })
  
  # Plot for top 3 skills in each industry
  output$skillsPlot <- renderPlotly({
    skills <- filteredSkillData() %>%
      select(Company.Industry, contains("_yn")) %>%
      pivot_longer(cols = -Company.Industry, names_to = "skill", values_to = "count") %>%
      group_by(Company.Industry, skill) %>%
      summarise(total_count = sum(count, na.rm = TRUE)) %>%
      group_by(Company.Industry) %>%
      top_n(3, total_count) %>%
      ungroup()
    
    p <- ggplot(skills, aes(x = Company.Industry, y = total_count, fill = skill)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Top 3 Skills by Industry", x = "Industry", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Reactive data based on the selected filter type for company comparisons
  filteredCompData <- reactive({
    if (input$filterType == "Filter by Sector") {
      data %>%
        group_by(Company.Sector) %>%
        summarise(across(c(Company.Culture.and.Values, Company.Work.Life.Balance, Compensation.and.Benefits, Company.Career.Opportunities, Company.Senior.Management), median, na.rm = TRUE))
    } else if (input$filterType == "Filter by Industry") {
      data %>%
        group_by(Company.Industry) %>%
        summarise(across(c(Company.Culture.and.Values, Company.Work.Life.Balance, Compensation.and.Benefits, Company.Career.Opportunities, Company.Senior.Management), median, na.rm = TRUE))
    } else {
      data %>%
        group_by(Company.Size) %>%
        summarise(across(c(Company.Culture.and.Values, Company.Work.Life.Balance, Compensation.and.Benefits, Company.Career.Opportunities, Company.Senior.Management), median, na.rm = TRUE))
    }
  })
  
  output$companyFactorsPlot <- renderPlotly({
    # Filter and pivot the data
    filtered_data <- filteredCompData() %>%
      pivot_longer(cols = -1, names_to = "factor", values_to = "value")
    
    # Check column names to debug
    print(colnames(filtered_data))
    
    # Determine the sorting factor based on user input
    sort_factor <- switch(input$sortType,
                          "Sort by Career Opportunities" = "Company.Career.Opportunities",
                          "Sort by Culture and Values" = "Company.Culture.and.Values",
                          "Sort by Senior Management" = "Company.Senior.Management",
                          "Sort by Work Life Balance" = "Company.Work.Life.Balance",
                          "Sort by Compensation and Benefits" = "Compensation.and.Benefits")
    
    # Determine grouping variable based on input filter type
    group_var <- switch(input$filterType,
                        "Filter by Sector" = "Company.Sector",
                        "Filter by Industry" = "Company.Industry",
                        "Filter by Company Size" = "Company.Size")
    
    # Calculate average for the selected factor for each grouping variable
    avg_factor <- filtered_data %>%
      filter(factor == sort_factor) %>%
      group_by_at(group_var) %>%
      summarize(avg_value = mean(value, na.rm = TRUE)) %>%
      arrange(avg_value)  # Ensure sorting is in descending order. Somehow adding desc here will ascend the value
    
    # Reorder grouping variable by the selected factor from highest to lowest
    filtered_data[[group_var]] <- factor(filtered_data[[group_var]], levels = avg_factor[[group_var]])
    
    # Create the plot
    p <- ggplot(filtered_data, aes_string(x = "value", y = group_var, color = "factor")) +
      geom_point(size = 3) +
      labs(title = "Company Industry Factors", x = "Value", y = group_var) +
      scale_color_discrete(name = "Factors") +
      theme(axis.text.y = element_text(size = 10))
    
    # Render the plot with Plotly
    ggplotly(p, height = 1200)
  })
}

# Run the Shiny app
shinyApp(ui, server)