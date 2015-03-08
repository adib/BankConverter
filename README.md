# Bank Converter

These scripts takes the so-called "CSV export" files generated by some internet banking websites into a proper [CSV file](http://en.wikipedia.org/wiki/Comma-separated_values) format ready to import into popular personal finance software applications. The problem is that the files that these banks generates seems to be intended for use in spreadsheet applications and not to import to other databases. However many personal finance software expects CSV formats in which every line contains a transaction, except for the first line which may be a header. 

In consequence you will need to convert your bank's dump into the file format expected by your personal finance application whenever you are reconciling your accounts.

## Supported Formats

Currently these banks and account types are supported:

 - [DBS](http://en.wikipedia.org/wiki/DBS_Bank) Savings.
 - [OCBC](http://en.wikipedia.org/wiki/Oversea-Chinese_Banking_Corporation) 360.

### DBS Savings

DBS' CSV export file has a header with plenty of blank lines, not sure how they got there. Furthermore the CSV record header is followed by a blank line before the data comes in. I'm not sure what were they thinking when they designed this format.

### OCBC 360

OCBC's export file also contains non-columnar header lines but without much superfluous blank lines. However in a transaction _may_ consists of two lines – depending whether it contains an extra free-form text. However not all transactions comes in two lines, hence we can't assume in that every even line contains the free-form text.

## Prerequisites
You will need to have [R](http://cran.r-project.org) version 3.1.2 (or newer) installed in your system as well as these R packages:

 - `dplyr`
 - `lubridate`
Please consult R's documentation how to install these.

## Installation
These installation steps should work on Unix or Unix-like systems (e.g. Mac OS X, Linux, or BSD).

 1. Download the repository
 2. Mark the scripts as executable:   
	$ chmod a+x csv-dbs.R csv-ocbc.R
 3. Create a symbolic link from `/usr/local/bin` to the script's installation directory:  
	$ cd /usr/local/bin  
	$ ln -s {path-to-download}/csv-dbs.R csv-dbs  
	$ ln -s {path-to-download}/csv-ocbc.R csv-ocbc

If you are a Windows user, please adapt those steps to your system as necessary.

## Usage
  1. Open your bank's Internet Banking website and download your transaction history into a CSV file.
  2. Run the conversion script against it, either `csv-dbs` or `csv-ocbc`
      - The first argument is your bank's CSV file as input.
      - The second argument is where to write the processed CSV output.
  3. Import the resulting CSV output into your favorite personal finance software.

## License
These scripts are provided under the BSD 2-clause open source license. Please refer to file `LICENSE.md` for details.

## Endnote
I have been having this CSV conversion problem for a long time and have been solving it manually so far – that is, clean up the data manually using a text editor and a spreadsheet. I even [wrote a blog post](http://basilsalad.com/labs/reconciling-bank-accounts-painful/) on the topic and see whether it would be worth to write an app around the issue. Apparently there was not much interest, hence I wrote these scripts just to solve my own problem. Please [let us know](http://basilsalad.com/about/contact/) if I was wrong and it's worth your time and money to have an easy-to-use app or service to do this.

Sasmito Adibowo  
http://cutecoder.org


