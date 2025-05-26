# Global -----------------------------------------------
# required packages 
install.packages(c("shiny", "shinythemes", 
                  "pdftools", "tidyverse", 
                  "lubridate", "DT", 
                  "shinydashboard", "shinyvalidate"))

# Load libraries
library(shiny)
library(shinythemes)
library(pdftools)
library(tidyverse)
library(lubridate)
library(DT)
library(shinydashboard)
library(R6)
library(readr)
library(DBI)
library(RSQLite)
library(shinyvalidate)
library(stringi)



category_choices <- c(
  "Uncategorized", "Groceries", "Transport", "Shopping", "Dining",
  "Entertainment", "Utilities", "Health", "Education", "Travel",
  "Income", "Transfer"
)

source("ManualTransaction.R")
source("BudgetCalculator.R")
source("categorize.R")
source("about.R")
source("ui.R")
source("server.R")
source("bank_process/detect_bank.R")
source("bank_process/millenium.R")
source("bank_process/citi.R") 
source("bank_process/santander.R") 
source("bank_process/revolut.R") 
source("bank_process/pko.R") 

