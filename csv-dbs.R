#!/usr/bin/env Rscript --vanilla 
# 
# Reformat DBS' Saving Account pseudo-CSV export into a proper CSV file that is ready for import into a personal finance software.
#
# DBS's CSV file format starts with 16 lines of blanks and header information containing the following:
#   - the account number
#   - account balances
#   - header information
#   - statement date
#
# The file then contains a CSV header that is then followed by two blank lines.
# Therefore we skip the first 19 lines of the file and then add in our own headers.
#
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(lubridate))

# The date format to export.
dateFormat <- "%Y-%m-%d"

inputFileName <- "~/Downloads/DBS Account Dump Jan-Feb 2015.csv"

readTable <- function(inputFileName) {
    inputTable <- read.csv(inputFileName,header=FALSE,skip=19)
    names(inputTable) <- c("Transaction.date","Reference","Withdrawals","Deposits","Description.1","Description.2","Description.3","Empty")
    mutate(inputTable,Transaction.date=dmy(Transaction.date))
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
            Withdrawals = formatNumber(Withdrawals),
            Deposits = formatNumber(Deposits),
            Memo = paste(Description.1,Description.2,Description.3,sep="\t")
        ) %>% select(Transaction.date,Reference,Memo,Withdrawals,Deposits)
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
    cat("Reformat DBS Saving Account pseudo-CSV export into a proper CSV file.\nUsage:\n")
    cat(paste("  ",script.name,"{input-file} {output-file}\nWhere:\n",
              "{input-file}\tThe file obtained from DBS' `Export to CSV` function (the `download` button on the balance history screen).\n",
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
