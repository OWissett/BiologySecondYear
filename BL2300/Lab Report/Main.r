library("ggplot2")
library("gridExtra")
library("dplyr")

dat1 <- read.csv("data1.csv", header=T, sep=",")
dat2 <- read.csv("data2.csv", header=T, sep=",")
dat3 <- read.csv("data3.csv", header=T, sep=",")

##Defines "plots" as a list, this allows the variable to be used in the loop
plots <- list()
plots[[1]] <- ggplot(data = dat1, aes(Time, absorb, colour=factor(Test), shape=factor(Test))) +
  geom_point() + 
  ylim(0, 0.07) + 
  geom_smooth(method = lm, formula = y~x) +
  scale_color_brewer(palette="Dark2")

plots[[2]] <- ggplot(data = dat2, aes(Time, absorb, colour=factor(Test), shape=factor(Test))) + 
  geom_point() + 
  ylim(0, 0.07) + 
  geom_smooth(method = lm, formula = y~x) +
  scale_color_brewer(palette="Dark2")

plots[[3]] <- ggplot(data = dat3, aes(Time, absorb, colour=factor(Test), shape=factor(Test))) + 
  geom_point() + 
  ylim(0, 0.07) + 
  geom_smooth(method = lm, formula = y~x) +
  scale_color_brewer(palette="Dark2")

##Each item within plots[] is diplayed on the graphics grid
grid.arrange(plots[[1]], plots[[2]], plots[[3]], nrow=1,ncol=3)