##############################
##Cell Cycle Lab Report Code##
##############################

library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(plotly)

##############################

## LOAD DATA
my.Data <- function(){
  Mdata <- list()
  Mdata[[1]] <- read.csv("MyData.csv", header = T, sep=",")
  Mdata[[2]] <- read.csv("ClassData.csv", header= T, sep=",")
  return(Mdata)
}

## DRAW GRAPH
my.Draw <- function(smMeth, smForm, smFam){
  dat <- my.Data()
  plots <- list()
  for(i in 1:2){
    plots[[i]] <- ggplot(data = dat[[i]], aes(Time, SI)) + 
      geom_point() +
      stat_smooth(method = smMeth, formula = smForm, method.args = list(family=smFam)) + 
      xlab("Time (Minutes)") + 
      ylab("Septation Index (%)") +
      scale_colour_brewer(palette="Paired")+
      ggtitle("Septation over time during synchronous mitosis in S. Pombe")
  }
  grid.arrange(plots[[1]], plots[[2]], nrow=2, ncol=1)
}








