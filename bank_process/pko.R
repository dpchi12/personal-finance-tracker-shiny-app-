# bank polski
process_pko_bp_statement <- function(pdf_path) {
  # read & split into lines
  all_text <- pdf_text(pdf_path) %>% paste(collapse="\n")
  lines    <- str_split(all_text, "\n")[[1]] %>% str_trim()
  
  # find the header‐lines: start with "YYYY-MM-DD  YYYY-MM-DD"
  header_idx <- which(str_detect(lines,
                                 "^\\d{4}-\\d{2}-\\d{2}\\s+\\d{4}-\\d{2}-\\d{2}"))
  if (length(header_idx)==0) return(tibble())  
  
  out <- list()
  for (j in seq_along(header_idx)) {
    i <- header_idx[j]
    
    # parse the transaction date
    date_str <- str_extract(lines[i], "^\\d{4}-\\d{2}-\\d{2}")
    tx_date  <- ymd(date_str)
    
    # pull the first comma-decimal number as the amount
    nums     <- str_extract_all(lines[i],
                                "-?\\d{1,3}(?:[ \\u00A0]\\d{3})*,\\d{2}")[[1]]
    amount   <- if (length(nums)>=1) {
      parse_number(nums[1],
                   locale = locale(decimal_mark=",", grouping_mark=" "))
    } else NA_real_
    
    # find the block of lines until the *next* header
    next_i <- if (j<length(header_idx)) header_idx[j+1] else length(lines)+1
    block  <- lines[(i+1):(next_i-1)]
    
    # locate the "Tytuł :" line 
    tpos <- which(str_detect(block, regex("^\\s*Tytuł\\s*:", ignore_case=TRUE)))[1]
    if (!is.na(tpos)) {
      desc_lines <- block[(tpos+1):length(block)]
    } else {
      desc_lines <- block
    }
    
    # drop blank lines and collapse
    desc <- desc_lines[desc_lines!=""] %>%
      paste(collapse=" ") %>%
      str_squish()
    
    out[[j]] <- tibble(
      date        = tx_date,
      description = desc,
      amount      = amount,
      source      = "PKO BP",
      category    = "Uncategorized"
    )
  }
  
  bind_rows(out)
}
