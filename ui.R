# UI ----------------------------------------------
ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Personal Finance Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.main_tabs == 'Transactions'",
        h4("Import Bank Statement"),
        fileInput("bank_statement", "Upload Statement (PDF)", accept = ".pdf"),
        hr(),
        h4("Manual Entry"),
        dateInput("manual_date", "Transaction Date", value = Sys.Date(),
                  min = as.Date("2000-01-01"), max = Sys.Date()),
        numericInput("manual_amount", "Amount", value = 0, step = 0.01),
        textInput("manual_desc", "Description"),
        selectInput("manual_category", "Category",
                    choices = c("Uncategorized","Housing", "Groceries","Transport","Shopping",
                                "Dining","Entertainment","Utilities","Health",
                                "Education","Travel","Income","Transfer")),
        actionButton("add_manual", "Add Transaction", class="btn-primary"),
        actionButton("delete_tx",  "Delete Selected",  class="btn-danger"),
        hr(),
        downloadButton("download_data","Export Data")
      ),
      # budget panel
      conditionalPanel(
        condition = "input.main_tabs == 'Budget Calculator'",
        h4("Find out how to approach your budget"),
        numericInput("net_income", "Enter your net income:",
                     value=0, min=0, step=1),
        actionButton("calculate","Calculate Budget"),
        actionButton("reset_budget", "Clear Results")
      )
    ),
    
    mainPanel(
        tabsetPanel(
          id = "main_tabs",
          tabPanel("Transactions",
                   h4(textOutput("import_status")),
                   DTOutput("transaction_table"),
                   uiOutput("category_ui")),
                   
          tabPanel("Spending Analysis",
                   fluidRow(
                     column(12,
                            dateRangeInput("date_range", "Select Date Range:",
                                           start = Sys.Date() - 30,
                                           end = Sys.Date(),
                                           min = "2024-01-01",
                                           max = Sys.Date(),
                                           format = "yyyy-mm-dd",
                                           separator = " to ")
                     )
                   ),
                   plotOutput("category_trend"),
                   plotOutput("spending_by_category", height = "500px")
          ),
          tabPanel("Summary",
                   fluidRow(
                     column(4, shinydashboard::valueBoxOutput("total_income")),
                     column(4, shinydashboard::valueBoxOutput("total_expenses")),
                     column(4, shinydashboard::valueBoxOutput("net_balance"))
                   ),
                   plotOutput("category_summary")),
          tabPanel("Budget Calculator",
                   uiOutput("budget_results")
          ),
          about_tab
        )
      )
    )
  )
