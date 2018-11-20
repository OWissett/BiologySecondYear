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
##      GLOBAL VARIABLES         ##
###################################
dat <- read.csv("dataset.csv", header=T, sep=",")
mdat <- subset(dat, dat$sex == "M")
fdat <- subset(dat, dat$sex == "F")


###################################
##     AUTOCALL FUNCTIONS        ##
###################################

##Purpose: Defines plausible biological GLMs
##Output: List of Models
my.Models <- function(){
  
  model <- list()
  
  #######################################################
  #                  MIXED SEXES
  #######################################################
  
  
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
  
  #######################################################
  #                     Males
  #######################################################
  
  ## Sysbp Depends on Smoking and Age, with no iteraction
  ## AIC: 344.1
  model[[5]] <- glm(sysbp~age+smoke ,data=mdat)
  
  ## Sysbp depends on smoking and age, with interaction between age and smoking
  ## AIC: 345
  model[[6]] <- glm(sysbp~age+smoke+age*smoke, data=mdat)
  
  ## Sysbp depends only on age
  ##AIC: 346.7
  model[[7]] <- glm(sysbp~age, data=mdat)
  
  ##Sysbp depends only on smoking
  ##AIC: 461.2
  model[[8]] <- glm(sysbp~smoke, data=mdat)
  
  #######################################################
  #                    Females
  #######################################################
  
  ## Sysbp Depends on Smoking and Age, with no iteraction
  ## AIC: 330.8
  model[[9]] <- glm(sysbp~age+smoke ,data=fdat)
  
  ## Sysbp depends on smoking and age, with interaction between age and smoking
  ## AIC: 332.8
  model[[10]] <- glm(sysbp~age+smoke+age*smoke, data=fdat)
  
  ## Sysbp depends only on age
  ##AIC: 336.1
  model[[11]] <- glm(sysbp~age, data=fdat)
  
  ##Sysbp depends only on smoking
  ##AIC: 464.1
  model[[12]] <- glm(sysbp~smoke, data=fdat)
  
  ##Returns List of models
  return(model)
  
  
  ##Best Models: 1, 5, 9. (Mixed. Males, Females)
}

###################################
##    USER CALL FUNCTIONS        ##
###################################

##Purpose: Summarises the GLMs
##INPUT:   m, the lower index number for the model to be summarised; n, the upper
##         index number for the model to be summarised. Values must be integers.
##OUTPUT:  Text file called GLM_sum.txt containing summary data 

my.Summary <- function(m, n){
  model <- my.Models()
  sink("GLM_sum.txt", append = F)
  for(i in m:n){
    print(paste("############### Model: ", toString(i), "###############"))
    print(summary(model[[i]]))
  }
  sink()
}

##Purpose: Creates summary plots for the models
##INPUT:   m, the lower index number for the model to be summarised; n, the upper
##         index number for the model to be summarised. Values must be integers.
##OUTPUT:  Plots of models outputted to graphics

my.ModelPlot <- function(m, n){
  model <- my.Models()
  for(i in m:n){
    png(filename = paste("AnalysisPlots/Graph", toString(i), ".png"))
    par(mfrow=c(2,2))
    plot(model[[i]])
    dev.off()
  }
}

##Purpose: Create plots the different data with Regression lines plotted
##         using the best fitting model
##OUTPUT:
##INPUT:

my.Graph <- function(){
  
  plots <- list()
  pred <- list()
  
  mods <- list(my.Models()[[1]], my.Models()[[5]], my.Models()[[9]])
  
  
  ##Mixed Predictor
  pred[[1]] <- data.frame(sysbp_pred = predict(mods[[1]]), age=dat$age, smoke=dat$smoke)
  
  ##Male Predictor
  pred[[2]] <- data.frame(sysbp_pred = predict(mods[[2]]), age=mdat$age, smoke=mdat$smoke)
  
  ##Female Predictor
  pred[[3]] <- data.frame(sysbp_pred = predict(mods[[3]]), age=fdat$age, smoke=fdat$smoke)

  
  ##Mixed
  plots[[1]] <- ggplot(dat, aes(age, sysbp, colour=smoke)) + 
    geom_point() +
    geom_line(data=pred[[1]], aes(age, sysbp_pred))
  
  ##Males
  plots[[2]] <- ggplot(mdat, aes(age, sysbp, colour=smoke)) + 
    geom_point() +
    geom_line(data=pred[[2]], aes(age, sysbp_pred))
  
  ##Females
  plots[[3]] <- ggplot(fdat, aes(age, sysbp, colour=smoke)) + 
    geom_point() +
    geom_line(data=pred[[3]], aes(age, sysbp_pred))
  
  grid.arrange(plots[[1]])
  grid.arrange(plots[[2]], plots[[3]], ncol=2)
}




