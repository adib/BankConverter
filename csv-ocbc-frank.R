#!/usr/bin/env Rscript --vanilla 
# 
# Reformat OCBC Frank Credit Card account pseudo-CSV export into a proper CSV file that is ready for import into a personal finance software.
#
# OCBC's CSV file format starts with six lines of header information containing:
#   - the account number (repeated twice)
#   - account balances
#   - header information
#

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))

# The date format to export.
dateFormat <- "%Y-%m-%d"

#
# Reads the input file and outputs a cleaned data frame
readTable <- function(inputFileName) {
    # The first six rows are header information, we skip it
    inputTable <- read.csv(inputFileName,skip=6)
    inputTable$row.number = 1:nrow(inputTable)
    
    mainRecords <- filter(inputTable,is.na(dmy(Transaction.date,quiet=TRUE)) == FALSE) %>% 
        rename (
            Withdrawals = Withdrawals..SGD.,
            Deposits = Deposits..SGD.
        ) %>% 
        mutate(
            Transaction.date = dmy(Transaction.date),
            Withdrawals = as.numeric(gsub(",","", Withdrawals)),
            Deposits = as.numeric(gsub(",","", Deposits))        
        )
    suppRecords <- filter(inputTable,is.na(dmy(Transaction.date,quiet=TRUE)) == TRUE) %>%
                    mutate(Description.2 = as.character(Transaction.date),
                           Withdrawals.2 = as.numeric(as.character(Description)),
                           Deposits.2 = as.numeric(as.character(Withdrawals..SGD.)))
    resultTable <- left_join(mainRecords,
                                mutate(suppRecords,mainRow=row.number-1) %>% 
                                select(mainRow,Description.2,Withdrawals.2,Deposits.2),
                             by=c("row.number" = "mainRow")
                             ) %>% mutate(Deposits=ifelse(is.na(Deposits),Deposits.2,Deposits),
                                          Withdrawals=ifelse(is.na(Withdrawals),Withdrawals.2,Withdrawals))
    resultTable
}

writeTable <- function(resultTable,outputFileName) {
    formatDate <- function(dv) {
        format(dv,fmt=dateFormat)
    }
    formatNumber <- function(nv) {
        sub("NA","",format(nv,trim=TRUE))   
    }
    formattedTable <- arrange(resultTable,Transaction.date) %>% 
        mutate(
            Transaction.date = formatDate(Transaction.date),
            Withdrawals = formatNumber(Withdrawals),
            Deposits = formatNumber(Deposits),
            Memo = paste(Description,Description.2,sep="\t")
        ) %>% select(Transaction.date,Memo,Withdrawals,Deposits)
    write.csv(formattedTable,outputFileName,row.names=FALSE)
}

#
# Main
#
main <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    if(length(args) != 2) {
        initial.options <- commandArgs(trailingOnly = FALSE)
        file.arg.name <- "--file="
        script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
        cat("Reformat OCBC Frank Credit Card Account pseudo-CSV export into a proper CSV file.\nUsage:\n")
        cat(paste("  ",script.name,"{input-file} {output-file}\nWhere:\n",
                  "{input-file}\tThe file obtained from OCBC Frank Credit Card's `Export to CSV` function.\n",
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
}

main()

