##############################
##Cell Cycle Lab Report Code##
##############################

##Inialise the Appropriate libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(plotly)

##############################

## LOAD DATA
##INPUT:  2 CSV files loaded from the working directory
##OUTPUT: Mdata, a list of CSV data frames with 2 columns.

my.Data <- function(){
  Mdata <- list()
  Mdata[[1]] <- read.csv("MyData.csv", header = T, sep=",")
  Mdata[[2]] <- read.csv("ClassData.csv", header= T, sep=",")
  return(Mdata)
}

## DRAW GRAPH
##INPUT:  smMeth, the Method for line estimation; smForm, the formula used
##        creation of the line; smFam, defines the family of the type of line estimator used.
##OUTPUT: a graphics grid that contains 2 ggplots of the data obtain in my.Data().

my.Draw <- function(smMeth, smForm, smFam){
  ##Creates local variable from my.Data()
  dat <- my.Data()
  ##Defines "plots" as a list, this allows the variable to be used in the loop
  plots <- list()
  
  ##Loop cycles 2 times, once for each plot. The loop creates a list of ggplots
  for(i in 1:2){
    plots[[i]] <- ggplot(data = dat[[i]], aes(Time, SI)) + 
      geom_point() +
      stat_smooth(method = smMeth, formula = smForm, method.args = list(family=smFam)) + 
      xlab("Time (Minutes)") + 
      ylab("Septation Index (%)") +
      scale_colour_brewer(palette="Paired")+
      ggtitle("Septation over time during synchronous mitosis in S. Pombe")
  }
  ##Each item within plots[] is diplayed on the graphics grid
  grid.arrange(plots[[1]], plots[[2]], nrow=1, ncol=2)
}

