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
##added so that the deployability of the code is increased.
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

dat$class[dat$smoke=="N" & dat$sex=="M"] <- "1" ##Male non smokers
dat$class[dat$smoke=="Y" & dat$sex=="M"] <- "2" ##Male smokers
dat$class[dat$smoke=="N" & dat$sex=="F"] <- "3" ##Female non smokers
dat$class[dat$smoke=="Y" & dat$sex=="F"] <- "4" ##Female smokers

###################################
##     AUTOCALL FUNCTIONS        ##
###################################

##Purpose: Defines plausible biological GLMs
##Output: List of 12 Models
my.Models <- function(){
  
  model <- list()
  
  #######################################################
  #                  MIXED SEXES
  #######################################################
  
  
  ## Sysbp Depends on Smoking and Age, with no iteraction
  ## AIC: 705 
  model[[1]] <- glm(sysbp~age+smoke, data=dat)
  
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
  model[[5]] <- glm(sysbp~age+smoke, data=mdat)
  
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
  model[[9]] <- glm(sysbp~age+smoke, data=fdat)
  
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

my.PrintTextToPlot <- function(my_text){
  par(mar=c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, my_text, 
       cex = 2, col = "black")
  par(mar = c(5, 4, 4, 2) + 0.1)
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
##         index number for the model to be summarised. Values must be integers
##         MODE: 1 - Single Test, and will out test to Plot. 0 - List Test, and will ouput PNG file
##OUTPUT:  Plots of models outputted to graphics

my.ModelPlot <- function(m, n, MODE, model){
  if(MODE==1){
    AIC_text <- toString(AIC(model))
    Form_text <- toString(model[["call"]])
    par(mfrow=c(3,2))
    plot(model)
    my.PrintTextToPlot(paste("AIC: ", AIC_text, "\n Formula:\n ", Form_text))
    hist(resid(model), main = "Histogram of Residues for Model", xlab="Residue Value")
  }else if(MODE==0 & isNumeric(m) & isNumeric(n) & length(model)>1){
    for(i in m:n){
      AIC_text <- toString(AIC(model[[i]]))
      Form_text <- toString(model[[i]][["call"]])
      png(filename = paste("Analysis Plots/Graph", toString(i), ".png"),
          width=1920, 
          height=1080)
      par(mfrow=c(3,2))
      plot(model[[i]])
      my.PrintTextToPlot(paste("AIC: ", AIC_text, "\n Formula: ", Form_text))
      hist(resid(model[[i]]), main = paste("Histogram of Residues for Model: ", toString(i)), xlab="Residue Value")
      dev.off()
    }
  }else{return("Enter Valid Parameters: 0 - List Test Mode, 1 - Single Test Mode")}
}

##Purpose: Create plots the different data with Regression lines plotted
##         using the best fitting model
##OUTPUT:
##INPUT:

my.Graph <- function(){
  
  plots <- list()
  pred <- list()
  mods <- list(my.Models()[[1]], my.Models()[[5]], my.Models()[[9]])

  ##Mixed Population Predictor
  pred[[1]] <- data.frame(sysbp_pred = predict(mods[[1]]), age=dat$age, smoke=dat$smoke, sex=dat$sex)
  
  ##Male Population Predictor
  pred[[2]] <- data.frame(sysbp_pred = predict(mods[[2]]), age=mdat$age, smoke=mdat$smoke)
  
  ##Female Population Predictor
  pred[[3]] <- data.frame(sysbp_pred = predict(mods[[3]]), age=fdat$age, smoke=fdat$smoke)
  
  ##Mixed Population graph showing Systolic BP against 
  plots[[1]] <- ggplot(dat, aes(age, sysbp, shape=dat$class, colour=dat$sex)) + 
    geom_point() +
    geom_line(data=pred[[1]], aes(age, sysbp_pred, colour=sex)) +
    ggtitle("Effect of Age, Cholesterol and Smoking on Systolic Blood Pressure", "Mixed Sex")
  
  ##Males
  plots[[2]] <- ggplot(mdat, aes(age, sysbp, shape=smoke, colour=mdat$smoke)) + 
    geom_point() +
    geom_line(data=pred[[2]], aes(age, sysbp_pred)) +
    ggtitle("Effect of Age, Cholesterol and Smoking on Systolic Blood Pressure", "Males")
  
  ##Females
  plots[[3]] <- ggplot(fdat, aes(age, sysbp, shape=smoke, colour=fdat$smoke)) + 
    geom_point() +
    geom_line(data=pred[[3]], aes(age, sysbp_pred)) +
    ggtitle("Effect of Age, Cholesterol and Smoking on Systolic Blood Pressure", "Females")
  
  
  grid.arrange(plots[[1]])
  grid.arrange(plots[[2]], plots[[3]], ncol=2)
  
}

####################################
##     NOTES  / DIARY             ##
####################################
##
## 20/11/18 - Day 1
##
## The best models were found to be 1, 5, 9. These models do not include
## an interaction between age and smoking. I found that I have been able to
## show 4 independent variable using, x axis = age, shape = smoking + gender
## and colour gradient = cholesterol. I am unsure whether this is stistically 
## correct. NOTE TO SELF: Ask Prof. Creswell during lab whether I can add 
## cholesterol to the graph using the colour function, even if it isnt included 
## as a covariate within the model or whether it must be added to the model and
## interactions must be tested or whether I should leave it out as it is not 
## necessary for the assignment...
##
## To Do: * Finish the labelling of my.Graph's graphs and the legends
##        * Solve the issue with my.ModelPlot, I want to be able to print the call
##          formula from the summary data of the model[[i]], this is not super 
##          important as it could be done manually, but this is a pain in the bum.
##        * Imrpove visual style of the document and clean up code and files.
## 
##  21/11/18
##
## Fixed my.ModelPlot issue with Formula output. Modified the my.ModelPlot function 
## allowing it to act on lists and an singlular models.
## 
## Currently investigating the affect of including cholesterol in the model. 
## 



