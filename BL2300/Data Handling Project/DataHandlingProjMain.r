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
library("ggfortify")

###################################
##     AUTOCALL FUNCTIONS        ##
###################################

##Purpose: Defines plausible biological GLMs
##Output: List of Models
my.Models <- function(){
  
  dat <- read.csv("dataset.csv", header=T, sep=",")
  model <- list()
  
  ## Sysbp Depends on Smoking and Age, with no iteraction
  ## AIC: 705 
  model[[1]] <- glm(sysbp~age+smoke ,data=dat)
  
  ## Sysbp depends on smoking and age, with interaction between age and smoking
  ## AIC: 707.8 
  model[[2]] <- glm(sysbp~age+smoke+age*smoke, data=dat)
  
  ## Sysbp depends only on age
  ##AIC: 709.7
  model[[3]] <- glm(sysbp~age, data=dat)
  
  ##Sysbp depends only on smoking
  ##AIC: 922.6
  model[[4]] <- glm(sysbp~smoke, data=dat)
  
  
  ##Returns List of models
  return(model)
}

###################################
##    USER CALL FUNCTIONS        ##
###################################

##Purpose: Summarises the GLMs
##OUTPUT: Text file called GLM_sum.txt containing summary data 
my.Summary <- function(){
  model <- my.Models()
  sink("GLM_sum.txt", append = F)
  for(i in 1:length(model)){
    print(summary(model[[1]]))
    print("######################## NEXT MODEL #########################")
  }
  sink()
}

##Purpose: Creates summary plots for the data
my.ModelPlot <- function(){
  model <- my.Models()
  
  for(i in 1:length(model)){
    autoplot(model[[i]])
  }
}



