###################################
## BL2300: Data Handling Project ##
###################################
## Dependent Variable: Systolic Blood Pressure
## Independent Variables: Smoke, Age
## Control Variables: Sex
## 
## HYPOTHESIS: There will be an increase in Systolic BP in smokers
##
##
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

##Purpose: Defines plausible biological GLMs
##Output: List of Models
my.Models <- function(){
  
  dat <- read.csv("dataset.csv", header=T, sep=",")
  model <- list()
  
  ## Sysbp Depends on Smoking and Age, with no iteraction
  model[[1]] <- glm(sysbp~age+smoke ,data=dat)
  
  ## Sysbp depends on smoking and age, with interaction between age and smoking
  model[[2]] <- glm(sysbp~age+smoke+age*smoke, data=dat)
  
  ## Sysbp depends only on age
  model[[3]] <- glm(sysbp~age, data=dat)
  
  ##Sysbp depends only on smoking
  model[[4]] <- glm(sysbp~smoke, data=dat)
  
  ##Returns List of models
  return(model)
}

###################################
##    USER CALL FUNCTIONS        ##
###################################

my.Summary <- function(){
  models <- my.Models()
  for(i in 1:length(models)){
    summary(models[[i]])
  }
}

