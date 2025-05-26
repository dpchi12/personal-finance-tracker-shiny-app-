#santander
process_santander_statement <- function(pdf_path) {
  text_lines <- pdftools::pdf_text(pdf_path) %>%
    paste(collapse = "\n") %>%
    str_split("\n") %>%
    unlist() %>%
    str_trim()
  
  transactions <- list()
  current_date <- NULL
  
  for (i in seq_along(text_lines)) {
    line <- text_lines[i]
    
    # date
    if (str_detect(line, "^\\d{1,2} [a-z]{3} \\d{4}")) {
      # extract date from start of line
      date_part <- str_extract(line, "^\\d{1,2} [a-z]{3} \\d{4}")
      current_date <- dmy(date_part)
      
      # amount
      amount_match <- str_match(line, "(-?\\d{1,3}(?:\\s\\d{3})*,\\d{2}) PLN")[,2]
      
      if (!is.na(amount_match)) {
        # convert to numeric
        amount <- amount_match %>%
          str_remove_all("\\s") %>%  
          str_replace(",", ".") %>%
          as.numeric()
        
        # description - everything between date and amount
        description <- str_remove(line, paste0(date_part, "|", amount_match, ".*")) %>%
          str_trim() %>%
          str_replace_all("\\s+", " ")
        
        # multiple lines description
        # check next lines for continuation
        j <- i + 1
        while(j <= length(text_lines) && 
              !str_detect(text_lines[j], "^\\d{1,2} [a-z]{3} \\d{4}") &&
              !str_detect(text_lines[j], "Booking date")) {
          description <- paste(description, str_trim(text_lines[j]))
          j <- j + 1
        }
        
        transactions[[length(transactions) + 1]] <- tibble(
          date = current_date,
          description = description,
          amount = amount,
          source = "Santander Bank",
          category = "Uncategorized"
        )
      }
    }
  }
  
  return(bind_rows(transactions))
}