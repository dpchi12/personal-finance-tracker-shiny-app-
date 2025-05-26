#citibank processor
process_citi_statement <- function(pdf_path) {
  text <- pdf_text(pdf_path) %>%
    str_split("\n") %>%
    unlist() %>%
    str_trim()
  
  # valid date in format DD.MM.YYYY
  transaction_lines <- text[str_detect(text, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}")]
  
  if (length(transaction_lines) == 0) {
    message("No transaction lines found.")
    return(tibble(
      date = as.Date(character()),
      description = character(),
      amount = numeric(),
      source = character(),
      category = character(),
      type = character()
    ))
  }
  
  transactions <- map_df(transaction_lines, function(line) {
    # the first date from anywhere in the line
    date_match <- str_extract(line, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}")
    if (is.na(date_match)) return(NULL)
    
    # amount at the end of line (end with zl)
    amount_raw <- str_extract(line, "[\\-−–—]?[\\d\\.\\s]+,\\d{2}\\s*zł$")
    if (is.na(amount_raw)) return(NULL)
    
    # normalize amount string
    amount_raw <- str_extract(line, "[\\-−–—\\u00AD]?[\\d\\.\\s]+,\\d{2}\\s*zł$")
    
    if (is.na(amount_raw)) return(NULL)

    # clean amount
    amount_raw <- str_extract(line, "[\\-−–—\\u00AD]?[\\d\\s\\.]+,\\d{2}\\s*zł\\b")
    if (is.na(amount_raw)) return(NULL)
    
    amount_str_clean <- amount_raw %>%
      str_remove("\\s*zł\\b") %>%         
      str_remove_all("[\\s\\.]") %>%       
      str_replace_all("[\u2212\u2013\u2014\u00AD−–—]", "-") %>%  
      str_replace(",", ".")               
    
    is_income <- str_detect(amount_str_clean, "^-")
    amount <- if (is_income) {
      abs(as.numeric(amount_str_clean))
    } else {
      -abs(as.numeric(amount_str_clean))
    }
    
    # remove date and amount
    description <- line %>%
      str_remove("\\d{1,2}\\.\\d{1,2}\\.\\d{4}") %>%
      str_remove(amount_raw) %>%
      str_squish()
    
    tibble(
      date = dmy(date_match),
      description = description,
      amount = amount,
      source = "Citi Credit Card",
      category = "Uncategorized"
    )
  })
  
  return(transactions)
}