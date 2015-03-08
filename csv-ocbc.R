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

# Define a few configurable parameters
dateFormat <- "%Y-%m-%d"

# Load data
i <- "~/Downloads/TransactionHistory_20150307080629.csv"

#
# Reads the input file and outputs a cleaned data frame
readTable <- function(inputFileName) {
    # The first five rows are header information, we skip it
    inputTable <- read.csv(inputFileName,skip=5)
    inputTable$row.number = 1:nrow(inputTable)
    
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
                                rename(Description.Extended=Description) %>% 
                                select(mainRow,Description.Extended),
                             by=c("row.number" = "mainRow")
                             )
    resultTable
}

writeTable <- function(resultTable,outputFileName) {
    formatDate <- function(dv) {
        format(dv,fmt=dateFormat)
    }
    formatNumber <- function(nv) {
        sub("NA","",format(nv,trim=TRUE))   
    }
    formattedTable <- mutate(resultTable,
        Transaction.date = formatDate(Transaction.date),
        Value.date = formatDate(Value.date),
        Withdrawals = formatNumber(Withdrawals),
        Deposits = formatNumber(Deposits)
        ) %>% select(Transaction.date,Value.date,Description,Description.Extended,Withdrawals,Deposits)
    write.csv(formattedTable,outputFileName,row.names=FALSE)
}

