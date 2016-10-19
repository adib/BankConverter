#!/usr/bin/env Rscript --vanilla 
# 
# Reformat BCA's Saving Account pseudo-CSV export into a proper CSV file that is ready for import into a personal finance software.
#
# BCA's CSV file format starts with 16 lines of blanks and header information containing the following:

suppressPackageStartupMessages(library(stringi))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
# The date format to export.
dateFormat <- "%Y-%m-%d"

readTable <- function(inputFileName) {
    
    parseDate <- function(dateValue,year) {
        imputeYear <- stri_join(year,"/")
        imputedDateString <- gsub("'",imputeYear,dateValue)
        ydm(imputedDateString)
    }
    
    guessDate <- function(dateValue) {
        now <- Sys.Date()
        guessYear <- year(now)
        parsedDate <- parseDate(dateValue,guessYear)
        ifelse(parsedDate > now,parseDate(dateValue,guessYear - 1),parsedDate)
    }
    
    inputTable <- read.csv(inputFileName,header=FALSE,skip=5)
    names(inputTable) <- c("Transaction.date","Description.1","Description.2","Amount","Type","Balance")
    resultTable <- head(inputTable,-4) %>%  data.frame(stringsAsFactors=FALSE) 
    resultTable["date.parsed"] = lapply(resultTable["Transaction.date"],guessDate)
    
    resultTable <- resultTable %>% mutate(Transaction.date = as.Date(date.parsed,origin="1970-01-01"))
    
    withdrawalRecords <- resultTable %>% filter(Type=="DB") %>% mutate(Withdrawals=Amount)
    depositRecords <- resultTable %>% filter(Type=="CR") %>% mutate(Deposits=Amount)
    
    union_all(withdrawalRecords,depositRecords) %>% 
        mutate(Reference="")
}

writeTable <- function(resultTable,outputFileName) {
    formatDate <- function(dv) {
        format(dv,format=dateFormat)
    }
    formatNumber <- function(nv) {
        sub("NA","",format(nv,trim=TRUE))   
    }
    formattedTable <- arrange(resultTable,Transaction.date) %>% 
        mutate(
            Transaction.date = formatDate(Transaction.date),
            Withdrawals = formatNumber(Withdrawals),
            Deposits = formatNumber(Deposits),
            Memo = paste(Description.1,Description.2,sep="\t")
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
    cat("Reformat BCA Saving Account pseudo-CSV export into a proper CSV file.\nUsage:\n")
    cat(paste("  ",script.name,"{input-file} {output-file}\nWhere:\n",
              "{input-file}\tThe file obtained from BCA's `Export to CSV` function (the `download` button on the balance history screen).\n",
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
