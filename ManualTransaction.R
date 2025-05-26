#R6 class for manual input
ManualTransaction <- R6Class("ManualTransaction",
                             public = list(
                               id = NULL,
                               date = NULL,
                               description = NULL,
                               amount = NULL,
                               category = NULL,
                               type = NULL,
                               source = "Manual Entry",
                               
                               initialize = function(date, description, amount, category) {
                                 self$id <- paste0("mt-", as.integer(Sys.time()), "-", sample(1e4:9e4,1))
                                 self$date <- date
                                 
                                 # description non-empty
                                 description <- trimws(as.character(description))
                                 if (nchar(description) == 0) {
                                   stop("Description cannot be empty.")
                                 }
                                 self$description <- description
                                 
                                 # amount numeric, >= 0, max two decimals
                                 amt <- suppressWarnings(as.numeric(amount))
                                 if (is.na(amt)) {
                                   stop("Amount must be numeric.")
                                 }
                                 if (amt < 0) {
                                   stop("Amount must be positive.")
                                 }
                                 # decimal point
                                 if (!grepl("^\\d+(\\.\\d{1,2})?$", format(amt, scientific = FALSE))) {
                                   stop("Amount may have at most two decimals.")
                                 }
                                 self$amount <- round(amt, 2)
                                 
                                 # category and type
                                 self$category <- as.character(category)
                                 self$type <- if (self$category == "Income") "Income" else "Expense"
                               },
                               
                               to_tibble = function() {
                                 tibble::tibble(
                                   id = self$id,
                                   date        = self$date,
                                   description = self$description,
                                   amount      = self$amount,
                                   source      = self$source,
                                   category    = self$category,
                                   type        = self$type
                                 )
                               }
                             ),
                             lock_objects = FALSE
)