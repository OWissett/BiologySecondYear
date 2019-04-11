
##Nice little function that I found that will install packages if they are not 
##installed.

ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
  
}

libs <- c("ggplot2", 
          "gridExtra",
          "itsmr",
          "pracma",
          "git2r",
          "car",
          "minpack.lm")



ipak(libs)

########################
##      Load Data     ##
########################



##List of CSV data frames
#folder <- readline("Where is the data from the spectrophotometer?  ")

dat <- list()

##List of files in current directory (ensure directory is correct)
#fils <- list.files(folder, full.names = TRUE)

files <- list.files(pattern = "\\.csv$")



##Loads CSV files

dat <- lapply(files, read.csv)

files <- NULL
libs <- NULL


my.ConcentrationGraph <- function(){
   
  conc.Dat <- data.frame("Fraction" )
  
}


