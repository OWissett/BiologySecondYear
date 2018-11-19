###################################
## BL2300: Data Handling Project ##
###################################

###################################
##     INITIALISE LIBRARIES      ##
###################################

##ggplot requires dplyr and pillar, some university workstations do not 
##automatically load dependencies for a packages. This code has been 
##add so that the deployability of the code is increased.
library("dplyr")
library("pillar")
library("ggplot2")
library("gridExtra")

###################################
##     AUTOCALL FUNCTIONS        ##
###################################

my.Load() <- function(){
  dat <- read.csv("dataset.csv", header=T, sep=",")
  return(dat)
}

###################################
##    USER CALL FUNCTIONS        ##
###################################
my.Models <- function(){
  models <- list()
  models[[1]] <- ##Create a list of different plausible GLMs
  
  
}
