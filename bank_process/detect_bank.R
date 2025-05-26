detect_bank_type <- function(pdf_path) {
  text <- tryCatch({
    pdf_text(pdf_path) %>% 
      paste(collapse = " ") %>% 
      str_to_upper()
  }, error = function(e) return(""))
  
  case_when(
    str_detect(text, "SANTANDER") ~ "santander",
    str_detect(text, "MILLENNIUM|MILLENECIE") ~ "millennium",
    str_detect(text, "CITI|CITI HANDLOWY|CITIBANK") ~ "citi",
    str_detect(text, "REVOLUT") ~ "revolut",
    str_detect(text, "BANK POLSKI|PKO") ~ "pko",
    TRUE ~ "unknown"
  )
}