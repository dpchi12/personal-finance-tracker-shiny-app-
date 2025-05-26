# Budget Calculator
calc_budget <- function(net_income) {
  if (!is.numeric(net_income) || any(is.na(net_income)))
    stop("`net_income` must be a numeric vector with no NAs.")
  if (any(net_income < 0))
    stop("All `net_income` values must be positive.")
  if (any(net_income %% 1 != 0))
    stop("`net_income` must be whole numbers.")
  
  tibble::tibble(
    net_income = net_income,
    necessities = net_income * 0.50,
    wants = net_income * 0.30,
    savings = net_income * 0.20
  )
}
