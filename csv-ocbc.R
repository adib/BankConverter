#!/usr/bin/env Rscript
# 
# Reformat OCBC 360 Saving Account pseudo-CSV export into a proper CSV file that is ready for import into a personal finance software.
#
# OCBC's CSV file format starts with five lines of header information containing:
#   - the account number
#   - account balances
#   - header information
#
# Furthermore a record may span more than one line in the `Description` column. Apparently the first row of the
# `Description` column is the transaction type whereas the second row contains free-text comments about the transaction.
# However not all records are expressed in two lines; those without a free-text field has only one line in the CSV dump.
#
library(dplyr)
library(lubridate)

# Load data
inputFileName <- "~/Downloads/TransactionHistory_20150307080629.csv"

# The first five rows are header information, we skip it
inputTable <- read.csv(inputFileName,skip=5)
inputTable$row.number = 1:nrow(inputTable)

mainRecords <- filter(inputTable,Transaction.date != "")
mainRecords <- filter(inputTable,Transaction.date != "") %>% 
    rename (
        Withdrawals = Withdrawals..SGD.,
        Deposits = Deposits..SGD.
    ) %>% 
    mutate(
        Transaction.date = dmy(Transaction.date),
        Value.date = dmy(Value.date),
        Withdrawals = as.numeric(gsub(",","", Withdrawals)),
        Deposits = as.numeric(gsub(",","", Deposits))        
)
suppRecords <- filter(inputTable,Transaction.date == "")
resultTable <- left_join(mainRecords,
                           mutate(suppRecords,mainRow=row.number-1) %>% 
                           rename(Description.Extended=Description) %>% select(mainRow,Description.Extended),
                         by=c("row.number" = "mainRow")
                )

