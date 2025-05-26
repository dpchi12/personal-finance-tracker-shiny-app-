# revolut
process_revolut_statement <- function(pdf_path) {
  text_lines <- pdf_text(pdf_path) %>%
    str_split("\n") %>%
    unlist() %>%
    str_trim()
  
  # transaction block
  start_idx <- which(str_detect(text_lines, "Account transactions from")) + 3
  end_idx   <- which(str_detect(text_lines, "Report lost or stolen card")) - 1
  trans_lines <- text_lines[start_idx:end_idx]
  
  # pre-compile
  date_pattern   <- "^[A-Za-z]{3} \\d{1,2}, \\d{4}"
  amount_pattern <- "\\d+\\.\\d{2} PLN"
  income_keywords<- c(
    "Top[-]*Up", "From:", "Salary", "Wypłata",
    "Refund", "Zwrot", "Deposit", "Wpłata",
    "Transfer In", "Transfer from",
    "Payroll", "Wages", "Income", "Payment Received",
    "Credit", "Incoming Transfer", "Rebate", "Reimbursement",
    "Dividend", "Interest", "Bonus", "Cashback",
    "Commission", "Payout", "Grant",
    "Pensja", "Przychód", "Przelew Otrzymany", "Przelew Przychodzący",
    "Premia", "Nagroda", "Refundacja", "Dotacja",
    "Dywidenda", "Odsetki", "Doładowanie", "Zwrot Podatku",
    "Zwrot Środków", "Należność", "Wpłata Gotówkowa"
  )
  income_regex <- regex(paste(income_keywords, collapse="|"), ignore_case=TRUE)
  
  idxs <- which(str_detect(trans_lines, date_pattern))
  
  map_dfr(idxs, function(i) {
    line <- trans_lines[i]
    
    # date
    date_str <- str_extract(line, date_pattern)
    date_val <- mdy(date_str)
    
    # amounts 
    amts <- str_extract_all(line, amount_pattern)[[1]]
    if(length(amts) < 2) {
      return(tibble(
        date        = as.Date(NA),
        description = NA_character_,
        amount      = NA_real_,
        source      = NA_character_,
        category    = NA_character_,
        type        = NA_character_
      ))
    }
    
    txn_amt_txt <- amts[1]
    txn_val     <- as.numeric(str_remove(txn_amt_txt, " PLN"))
    
    # description
    desc <- line %>%
      str_remove(date_pattern) %>%
      str_remove(paste0(txn_amt_txt, ".*")) %>%
      str_trim()
    
    if (i < length(trans_lines)) {
      nxt <- trans_lines[i + 1]
      if (!str_detect(nxt, date_pattern)) {
        details <- str_trim(nxt)
      }
    }
    full_desc <- str_squish(paste(desc, details))
    
    # classify income vs expense
    is_inc <- str_detect(full_desc, income_regex)
    amt    <- if (is_inc) abs(txn_val) else -abs(txn_val)
    tx_type<- if (amt > 0) "Income" else "Expense"
    
    tibble(
      date        = date_val,
      description = full_desc,
      amount      = amt,
      source      = "Revolut",
      category    = "Uncategorized",
      type        = tx_type
    )
  })
}