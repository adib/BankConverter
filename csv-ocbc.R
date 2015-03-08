#!/usr/bin/env Rscript --vanilla 
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

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))

# The date format to export.
dateFormat <- "%Y-%m-%d"

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
    formattedTable <- arrange(resultTable,desc(Transaction.date)) %>% 
        mutate(
            Transaction.date = formatDate(Transaction.date),
            Value.date = formatDate(Value.date),
            Withdrawals = formatNumber(Withdrawals),
            Deposits = formatNumber(Deposits),
            Memo = paste(Description,Description.Extended,sep="\t")
        ) %>% select(Transaction.date,Value.date,Memo,Withdrawals,Deposits)
    write.csv(formattedTable,outputFileName,row.names=FALSE)
}

#
# Main
#
args <- commandArgs(trailingOnly = TRUE)
if(length(args) != 2) {
    initial.options <- commandArgs(trailingOnly = FALSE)
    file.arg.name <- "--file="
    script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
    cat("Reformat OCBC 360 Saving Account pseudo-CSV export into a proper CSV file.\nUsage:\n")
    cat(paste("  ",script.name,"{input-file} {output-file}\nWhere:\n",
              "{input-file}\tThe file obtained from OCBC 360's `Export to CSV` function.\n",
              "{output-file}\tWhere to write the properly-formatted CSV output file.\n"),sep="")
    quit(save="no",status=3)
}
inputFileName <- args[1]
outputFileName <- args[2]
if(!file.exists(inputFileName)) {
    cat(paste("Input file '",inputFileName,"' not found.\n",sep=""))
    quit(save="no",status=3)    
}
if(file.exists(outputFileName)) {
    cat(paste("Input file '",outputFileName,"' already exists.\n",sep=""))
    quit(save="no",status=3)    
}

# Process
writeTable(readTable(inputFileName),outputFileName)
quit(save="no",status=0)
