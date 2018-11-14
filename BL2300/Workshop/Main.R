#########################################
## Main File for Data Hanlding Project ##
#########################################

#########################################
##        Initialisation Code          ##
#########################################

## Iniate Libraries
library(dplyr)
library(pillar)
library(tidyverse)
library(ggplot2)
library(gridExtra)

## Load Data Set into R
cat <- read.table("H:/temp/AssessedPractical/catfleas4.csv", header=T, sep="," )

##Check File has been mounted correctly
head(cat) 

##Define Factors with data frame
cat$trtmt <- factor(cat$trtmt)
cat$carpet <- factor(cat$carpet)
cat$class[cat$carpet == 0 & cat$trtmt == 0] <- 0
cat$class[cat$carpet == 0 & cat$trtmt == 1] <- 1
cat$class[cat$carpet == 1 & cat$trtmt == 0] <- 2
cat$class[cat$carpet == 1 & cat$trtmt == 1] <- 3
cat$class1 <- factor(cat$class)

#########################################
##          FUNCTION CALL CODE         ##
#########################################

## Create General Linear Model
my.Model <- function() {
  
  #Deinfe Models - Models have been choosen as they are biologically sound.
  model1 <- glm(fleas ~ ncats + trtmt + carpet, data = cat) 
  model2 <- glm(fleas ~ ncats + trtmt + carpet + trtmt * ncats, data = cat) ##Includes interaction between treatment and number of cats.
  
  ## Plots Model Analysis Graphs
  par(mfrow = c(2, 2))
  
  plot(model1)
  plot(model2)
  
  ## Plot Model residue distribution, a normal distrobution is expected.
  hist(resid(model1))
  hist(resid(model2))
  
  ##Summarises GLM data
  summary(model1)
  summary(model2)
  
  ##Generate AIC for two models
  AIC(model1, model2)
  
  ################################### RESULTS ########################################
  ##                                                                                ##
  ## Model1 was found to have a lower AIK value than model2, however, it was not    ##
  ## a significant diferences between them. Model1 was chosen, this is because      ##
  ## model2 did not show a signifcant relation between treatment and number of cats ##
  ##                                                                                ##   
  ####################################################################################
  
}

##Role: Creates a graph using ggplot2.0 extended graphics library
##
##Output: the log of fleas found against number of cats within the house, showing all 4  
##        different combinations of carpet and treatment.

my.GraphCombined <- function() {
  ### Make a variable that allows you to plot different symbols for treatment and carpet types
  
  gp <- ggplot(cat, aes(ncats, fleas, shape = class1, colour = class1)) + 
    labs(x = "Number of Cats", y = "Log Fleas Found")
    
  
  gp + geom_point() +
    geom_jitter(width = 0.15) +
    geom_abline(intercept = 1.54118, slope = 0.25) +
    geom_abline(intercept = 1.54118 + (-0.53748), slope = 0.25) +
    geom_abline(intercept = 1.54118 + (-0.23643 - 0.53748), slope = 0.25) +
    geom_abline(intercept = 1.54118 + (-0.23643), slope = 0.25)
}

my.GraphIndiv <- function() {
  
  xLim <- xlim(0,4)
  yLim <- ylim(0,3) 
  
  plots=list()
  
  for(i in 1:4){
    plots[[i]]<-ggplot(subset(cat,class1==(i-1)), aes(ncats, fleas)) + xLim + yLim
  }
  
  #plots <- ggplot(subset(cat,class1=="0"), aes(ncats, fleas)) + xLim + yLim
  #plots <- ggplot(subset(cat,class1=="1"), aes(ncats, fleas)) + xLim + yLim
  #plots <- ggplot(subset(cat,class1=="2"), aes(ncats, fleas)) + xLim + yLim
  #plots <- ggplot(subset(cat,class1=="3"), aes(ncats, fleas)) + xLim + yLim
  
  ##Arranges all 4 plots on the same screen, this requires the gridExtra package.
  grid.arrange(plot[[1]], plot[[2]], plot[[3]], plot[[4]], nrow=2, ncol=2)
}

## Functions have been used so that when the codee is loaded, it is not automatically executed. Automatic executing 
## increases the time taken for the code to load, as not all code must be ran each time during model testing.

