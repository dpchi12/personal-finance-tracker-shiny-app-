# Server  -----------------------------------------------
server <- function(input, output, session) {
  # reactive to store values
  iv <- InputValidator$new()
  iv$add_rule("manual_amount", sv_required("Please enter an amount"))
  iv$add_rule("manual_amount", sv_numeric(message = "Amount must be ≥ 0"))
  iv$enable()
  
  transactions <- reactiveVal(
    tibble(
      id = character(),
      date = as.Date(character()),
      description = character(),
      amount = numeric(),
      source = character(),
      category = character(),
      type= character()
    )
  )
  categories <- reactiveVal(category_choices)
  
  bank_type <- reactiveVal("")
  
  generate_id <- function(n) {
    replicate(n, paste0("tx-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                        sprintf("%04d", sample.int(9999,1)))
    )
  }
  
  budget_vals <- reactiveVal(NULL)

  filtered_transactions <- reactive({
    req(nrow(transactions()) > 0) 
    req(input$date_range)
    
    transactions() %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
  })
  
  #### IMPORT/MANUAL ENTRY/DELETE ####
  
  ## statement import---------------------------------
  observeEvent(input$bank_statement, {
    tryCatch({
      detected_type <- detect_bank_type(input$bank_statement$datapath)
      bank_type(detected_type)

      if (detected_type == "millennium") {
        bank_data <- process_millennium_statement(input$bank_statement$datapath)
      } else if (detected_type == "citi") {
        bank_data <- process_citi_statement(input$bank_statement$datapath)
      } else if (detected_type == "santander") {
        bank_data <- process_santander_statement(input$bank_statement$datapath)
      } else if (detected_type == "revolut") {
        bank_data <- process_revolut_statement(input$bank_statement$datapath)
      } else if (detected_type == "pko") {
        bank_data <- process_pko_bp_statement(input$bank_statement$datapath)  
      } else {
        showNotification("Unsupported bank statement format", type = "error")
        return()
      }
      
      if (nrow(bank_data) > 0) {
        bank_data <- categorize_transactions(bank_data)
        bank_data <- bank_data %>%
          mutate(
            id = paste0("imp-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", row_number())
          )
        transactions(bank_data)
        showNotification(paste("Successfully imported", nrow(bank_data), "transactions"), type = "message")
      } else {
        showNotification("No transactions found in the statement", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Error processing statement:", e$message), type = "error")
    })
  })
  
  ## manual entry----------------------------------
  observeEvent(input$add_manual, {
    req(iv$is_valid())
    
    tryCatch({
      tx <- ManualTransaction$new(
        date        = as.Date(input$manual_date),
        description = input$manual_desc,
        amount      = input$manual_amount,
        category    = input$manual_category
      )

      transactions(bind_rows(transactions(), tx$to_tibble()))
      
      updateDateInput(session,"manual_date", value = Sys.Date())
      updateNumericInput(session,"manual_amount", value = 0)
      updateTextInput(session,"manual_desc", value = "")
      showNotification("✅ Manual transaction added.", type = "message")
    }, error = function(err) {
      showNotification(paste("Error adding transaction:", err$message), type = "error")
    })
  })
  
  ## delete selected row------------------------------
  observeEvent(input$delete_tx, {
    sel <- input$transaction_table_rows_selected
    if (length(sel) == 0) {
      showNotification("Select a row in the table first.", type = "warning")
      return()
    }
    current_tbl <- isolate(transactions())
    sel_ids     <- current_tbl$id[sel]
    transactions(
      current_tbl %>% 
        filter(!id %in% sel_ids)
    )
    showNotification("Selected transaction deleted️", type = "message")
  })
  
  #### EDIT CATEGORY #### 
  observeEvent(input$category_update, {
    new_cat <- input$category_update$value
    sel_id <- input$category_update$id
    new_type <- ifelse(new_cat == "Income", "Income", "Expense")
    
    transactions(
      transactions() %>%
        mutate(
          category = ifelse(id == sel_id, new_cat, category),
          type     = ifelse(id == sel_id, new_type, type)
        )
    )
  })
  
  #### CALCULATE BUDGET #### 
  observeEvent(input$calculate, {
    req(input$net_income)
    val <- tryCatch(
      calc_budget(input$net_income),
      error = function(e) { 
        showNotification(e$message, type = "error"); 
        NULL 
      })
    budget_vals(val)                
  })

  observeEvent(input$reset_budget, {
    budget_vals(NULL)              
  })
  
  ######### OUTPUT ########
  
  ## import label
  output$import_status <- renderText({
    if (bank_type() == "") {
      "No statement imported yet"
    } else {
      paste("Imported", bank_type(), "statement with", nrow(transactions()), "transactions")
    }
  })
  
  ## transactions table
  output$transaction_table <- renderDT({
    req(nrow(transactions()) > 0)
    make_category_select <- function(current_cat, tx_id) {
      options <- map_chr(categories(), ~ sprintf(
        '<option value="%s"%s>%s</option>',
        .x, 
        ifelse(.x == current_cat, ' selected', ''),
        .x
      )) %>% paste(collapse = "")
      
      sprintf('<select class="cat-select" data-id="%s">%s</select>', tx_id, options)
    }
    
    df <- transactions() %>%
      mutate(
        category = map2_chr(category, id, make_category_select)
      ) %>% 
      select(-id)
    
    cat_idx <- which(names(df) == "category") - 1
    
    datatable(
      df,
      escape = FALSE,
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(
            targets = cat_idx,
            orderable = FALSE,
            searchable = FALSE
          )
        ),
        initComplete = JS("
        function(settings) {
          $('.cat-select').on('change', function() {
            let id = $(this).data('id');
            Shiny.setInputValue('category_update', {
              id: id,
              value: $(this).val()
            }, {priority: 'event'});
          });
        }
      ")
      )
    )
  })
  
  ## plot: category trend over time
  output$category_trend <- renderPlot({
    req(nrow(filtered_transactions()) > 0)
    
    filtered_transactions() %>%
      filter(type == "Expense") %>%
      mutate(total = abs(amount))%>%

      ggplot(aes(x = date, y = total, color = category)) +
      geom_line() +
      geom_point() +
      labs(title = "Spending Trends by Category",
           x = "Date",
           y = "Amount (PLN)") +
      theme_minimal()
  })
  
  ## plot: bar chart of categories
  output$spending_by_category <- renderPlot({
    req(nrow(filtered_transactions()) > 0)
    filtered_transactions() %>%
      filter(type == "Expense") %>%
      group_by(category) %>%
      summarise(total = sum(abs(amount)), .groups = "drop") %>%
      ggplot(aes(x = category, y = total, fill = category)) +
      geom_col(show.legend = FALSE) +
      labs(
        title = "Total Expenses by Category",
        x = NULL,
        y = "Amount (PLN)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()
      )
  })
  
  ## summary spending
  output$total_income <- renderValueBox({
    total <- filtered_transactions() %>% filter(type == "Income") %>%
      summarise(sum = sum(amount)) %>% pull(sum)
    
    valueBox(sprintf("%.2f PLN", total), "Total Income",
      icon = icon("money-bill-wave"), color = "green")
  })
  
  output$total_expenses <- renderValueBox({
    total <- filtered_transactions() %>% filter(type == "Expense") %>%
      summarise(sum = sum(abs(amount))) %>% pull(sum)
    
    valueBox(sprintf("%.2f PLN", total), "Total Expenses",
      icon = icon("credit-card"), color = "red")
  })
  
  output$net_balance <- renderValueBox({
    income <- filtered_transactions() %>% filter(type == "Income") %>%
      summarise(sum = sum(amount)) %>% pull(sum)
    
    expenses <- filtered_transactions() %>%
      filter(type == "Expense") %>% 
      summarise(sum = sum(abs(amount))) %>% pull(sum)
    
    net <- income - expenses
    
    valueBox(
      sprintf("%.2f PLN", net),
      "Net Balance",
      icon = icon("balance-scale"),
      color = ifelse(net >= 0, "green", "red"))
  })
  
  ## pie chart
  output$category_summary <- renderPlot({
    req(nrow(filtered_transactions()) > 0)
    filtered_transactions() %>%
      filter(type == "Expense") %>%
      group_by(category) %>%
      summarise(total = sum(abs(amount))) %>%
      mutate(percentage = total/sum(total)) %>%
      ggplot(aes(x = "", y = total, fill = category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(percentage*100), "%")),
                position = position_stack(vjust = 0.5)) +
      labs(title = "Expense Distribution by Category",
           fill = "Category") +
      theme_void()
  })
  
  ## budget UI
  output$budget_results <- renderUI({
    vals <- budget_vals(); if (is.null(vals)) return()
    tagList(
      valueBox(sprintf("%.0f PLN", vals$necessities), 
               "50% for necessities", icon("utensils")),
      valueBox(sprintf("%.0f PLN", vals$wants), 
               "30% for what you wants", icon("shopping-cart")),
      valueBox(sprintf("%.0f PLN", vals$savings), 
               "20% for savings", icon("piggy-bank"))
    )
  })
  
  ## download data
  output$download_data <- downloadHandler(
    filename = function() {paste("transactions-", Sys.Date(), ".csv", sep = "")},
    content = function(file) {write.csv(transactions(), file, row.names = FALSE)}
  )
}