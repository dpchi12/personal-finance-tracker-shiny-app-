# Millennium bank processor
process_millennium_statement <- function(pdf_path) {
  text <- pdf_text(pdf_path) %>%
    str_split("\n") %>%
    unlist() %>%
    str_trim()
  
  # find transaction blocks
  start_idx <- which(str_detect(text, "CURRENT ACCOUNTS - DETAILS"))[1] + 2
  end_idx <- which(str_detect(text, "TOTAL CREDIT:"))[1] - 1
  
  if (length(start_idx) == 0 || length(end_idx) == 0 || start_idx >= end_idx) {
    return(tibble(date = as.Date(character()),
                  description = character(),
                  amount = numeric(),
                  source = character(),
                  category = character(),
                  type = character()))
  }
  
  transactions <- tibble()
  current_date <- as.Date(NA)
  
  for(line in text[start_idx:end_idx]) {
    # first date pattern only
    date_match <- str_extract(line, "\\d{4}-\\d{2}-\\d{2}")
    
    if (!is.na(date_match)) {
      current_date <- ymd(date_match)
      
      # remove all dates from description
      clean_line <- str_remove_all(line, "\\d{4}-\\d{2}-\\d{2}\\s*")
      
      # split into components
      parts <- str_split(clean_line, "\\s{2,}")[[1]]
      
      if (length(parts) >= 2) {
        # handle negative amounts with trailing minus
        amount_str <- parts[length(parts)-1]  
        
        # check for trailing minus sign
        is_negative <- str_detect(amount_str, "-$")
        amount_str_clean <- str_remove(amount_str, "-$") %>%
          str_replace_all("\\.", "") %>%  
          str_replace(",", ".")  
        
        amount <- as.numeric(amount_str_clean)
        if (is_negative) amount <- -amount
        
        # description is everything before amount
        description <- if (length(parts) > 2) {
          paste(parts[1:(length(parts)-2)], collapse = " ")
        } else {
          ""
        }
        
        transactions <- bind_rows(transactions, tibble(
          date = current_date,
          description = str_trim(description),
          amount = amount,
          source = "Millennium Bank",
          category = "Uncategorized"
        ))
      }
    } else if (!is.na(current_date) && nrow(transactions) > 0) {
      # append continuation line
      clean_line <- str_remove_all(line, "\\d{4}-\\d{2}-\\d{2}\\s*")
      transactions$description[nrow(transactions)] <- 
        paste(transactions$description[nrow(transactions)], str_trim(clean_line))
    }
  }
  
  return(transactions)
}
