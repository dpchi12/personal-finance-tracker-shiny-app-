categorize_transactions <- function(df) {
  
  category_rules <- list(
    "Housing" = c("RENT", "MORTGAGE", "HOUSE", "DOM", "WYNAJEM", "WYNAJECIA"),
    "Groceries" = c("BIEDRONKA", "LIDL", "JMP", "SPOLEM", "CARREFOUR", "ELECLERC", "DOMASIA", "ZABKA", "DEALZ"),
    "Transport" = c("UBER", "TAXI", "BOLT", "MPK", "ZTM", "PKP", "JAKDOJADE"),
    "Shopping" = c("DECATHLON", "ZARA", "ADIDAS", "NIKE", "EMPIK", "MEDIA MARKT", 
                   "ALLEGRO", "VINTED", "LACOSTE", "LEGO", "ROSSMANN", "AMAZON", 
                   "ZALANDO", "ACTION", "TEMU", "SEPHORA"),
    "Dining" = c("STARBUCKS", "KFC", "BAKERY", "RESTAURACJA", "PIZZA", 
                 "MC DONALD'S", "COSTA COFFEE", "VITA", "PASTELOWA"),
    "Entertainment" = c("CINEMA", "NETFLIX", "SPOTIFY", "CONCERT", "MUSEUM", 
                        "YOUTUBE", "DISNEY", "KLUB"),
    "Utilities" = c("OPLATA", "PGE", "TAURON", "WODOCIAGI", "INTERNET"),
    "Health" = c("APTEKA", "LEKARZ", "SZPITAL", "MEDICOVER", "LUXMED", "ZDROFIT"),
    "Education" = c("KSIAZKI", "UNIWERSYTET", "KURS", "SZKOLA"),
    "Travel" = c("HOTEL", "LOTNISKO", "RYANAIR", "AIRBNB", "FLIXBUS"),
    "Income" = c("SPLATA", "WPLATA", "WPLYW", "WYNAGRODZENIE", "PRZELEW PRZYCHODZACY", "ZWROT"),
    "Transfer" = c("PRZELEW WYCHODZACY", "PRZELEW NA NUMER TELEFONU", "TRANSFER")
  )
  
  income_keywords <- category_rules$Income
  
  df %>%
    mutate(
      # Normalize to ASCII and uppercase for matching
      description_ascii = ifelse(
        is.na(description),
        NA,
        stri_trans_general(toupper(description), "Latin-ASCII")
      ),
      category = case_when(
        str_detect(description_ascii, paste(category_rules$Housing, collapse = "|")) ~ "Housing",
        str_detect(description_ascii, paste(category_rules$Groceries, collapse = "|")) ~ "Groceries",
        str_detect(description_ascii, paste(category_rules$Transport, collapse = "|")) ~ "Transport",
        str_detect(description_ascii, paste(category_rules$Shopping, collapse = "|")) ~ "Shopping",
        str_detect(description_ascii, paste(category_rules$Dining, collapse = "|")) ~ "Dining",
        str_detect(description_ascii, paste(category_rules$Entertainment, collapse = "|")) ~ "Entertainment",
        str_detect(description_ascii, paste(category_rules$Utilities, collapse = "|")) ~ "Utilities",
        str_detect(description_ascii, paste(category_rules$Health, collapse = "|")) ~ "Health",
        str_detect(description_ascii, paste(category_rules$Education, collapse = "|")) ~ "Education",
        str_detect(description_ascii, paste(category_rules$Travel, collapse = "|")) ~ "Travel",
        str_detect(description_ascii, paste(category_rules$Income, collapse = "|")) ~ "Income",
        str_detect(description_ascii, paste(category_rules$Transfer, collapse = "|")) ~ "Transfer",
        TRUE ~ "Uncategorized"
      ),
      type = case_when(
        str_detect(description_ascii, paste(income_keywords, collapse = "|")) ~ "Income",
        amount > 0 ~ "Income",
        TRUE ~ "Expense"
      )
    ) %>%
    select(-description_ascii)
}