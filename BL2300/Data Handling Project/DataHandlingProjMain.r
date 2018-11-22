###################################
## BL2300: Data Handling Project ##
###################################
## Dependent Variable: Systolic Blood Pressure
## Independent Variables: Smoke, Age
## Control Variables: Sex
## 
## HYPOTHESIS: There will be an increase in Systolic BP in smokers as age increases
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
library("ggthemes")

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
  #                  MIXED SEXES W/O Cholesterol
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
  #                     Males W/O Cholesterol
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
  #                    Females - W/O Cholesterol
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
  
  
  
  #######################################################
  #                  MIXED SEXES W/ Cholesterol
  #######################################################
  
  
  ## Sysbp Depends on Smoking, Age and Cholesterol, with no iteractions
  ## AIC:  603.3
  model[[13]] <- glm(sysbp~age+smoke+cholest, data=dat)
  
  ## Sysbp depends on smoking, age and Cholesterol, with interaction between cholesterol and smoking
  ## AIC:  605
  model[[14]] <- glm(sysbp~age+smoke+cholest+smoke*cholest, data=dat)
  
  ## Sysbp depends on Smoking, age and Cholesterol, with interaction
  ##AIC: 
  model[[15]] <- glm(sysbp~age, data=dat)
  
  ##Sysbp depends only on smoking
  ##AIC: 
  model[[16]] <- glm(sysbp~smoke, data=dat)
  
  #######################################################
  #                     Males W/ Cholesterol
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
  #                    Females - W/OCholesterol
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

##Purpose: Print a given string onto a graphical plot
##INPUT: my_stext, a string that is printed onto the plot
##OUTPUT: a plot that is outputted to the current graphics writer.
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
##OUTPUT:  Text file called GLM_sum.txt containing summary and AIC data 

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
##         model: glm/lm (assuming MODE==1) or list of GLMs (assuming MODE==0)
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
  }else{print("Enter Valid Parameters: 0 - List Test Mode, 1 - Single Test Mode")}
}

##Purpose: Create plots the different data with Regression lines plotted
##         using the best fitting model
##INPUT: Models for the dat, mdat and fdat
##OUTPUT: 3 plots are outputted to the graphics writer.

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
  plots[[1]] <- ggplot(dat, aes(age, sysbp, shape=dat$class, colour=dat$class)) + 
    geom_point() +
    geom_line(data=pred[[1]], aes(age, sysbp_pred)) +
    ggtitle("Effect of Age and Smoking Status on Systolic Blood Pressure", "Mixed Sex") + 
    scale_colour_manual(name ="Sex and Smoking Status", 
                       labels =c("Male Non-Smokers", 
                                 "Male Smokers",
                                 "Female Non-Smokers", 
                                 "Female Smokers"),
                       values = c("darkcyan", "firebrick1", "darkcyan","firebrick1")) +
    scale_shape_manual(name ="Sex and Smoking Status", 
                       labels =c("Male Non-Smokers", 
                                 "Male Smokers",
                                 "Female Non-Smokers", 
                                 "Female Smokers"),
                       values = c(1,1,2,2)) +
    xlab("Age of Individual (Years)") +
    ylab("Systolic Blood Pressure (mmHg)") +
    theme_classic() +
    xlim(29,76) +
    ylim(100, 200)
    
  
  ##Males
  plots[[2]] <- ggplot(mdat, aes(age, sysbp, shape=mdat$smoke, colour=mdat$smoke)) + 
    geom_point() +
    geom_line(data=pred[[2]], aes(age, sysbp_pred, linetype = )) +
    ggtitle("Effect of Age and Smoking on Systolic Blood Pressure", "Males") +
    scale_colour_manual(name ="Smoking Status", 
                       labels =c("Non-Smoker", "Smoker"),
                       values = c("darkcyan", "firebrick1")) +
    scale_shape_manual(name ="Smoking Status",
                       labels =c("Non-Smoker", "Smoker"),
                       values = c(1,1)) +
    xlab("Age of Individual (Years)") +
    ylab("Systolic Blood Pressure (mmHg)") +
    theme_classic() +
    xlim(29,76) +
    ylim(100, 200)
  
  ##Females
  plots[[3]] <- ggplot(fdat, aes(age, sysbp, shape=fdat$smoke, colour=fdat$smoke)) + 
    geom_point() +
    geom_line(data=pred[[3]], aes(age, sysbp_pred)) +
    ggtitle("Effect of Age and Smoking on Systolic Blood Pressure", "Females") +
    scale_colour_manual(name ="Smoking Status",
                        labels =c("Non-Smoker", "Smoker"),
                        values = c("darkcyan", "firebrick1")) +
    scale_shape_manual(name ="Smoking Status",
                       labels =c("Non-Smoker", "Smoker"),
                       values = c(2,2)) +
    xlab("Age of Individual (Years)") +
    ylab("Systolic Blood Pressure (mmHg)") +
    theme_classic() +
    xlim(29,76) +
    ylim(100, 200)
    
    
  
  grid.arrange(plots[[1]])
  grid.arrange(plots[[2]], plots[[3]], ncol=2)
  
}

##Purpose: To perform an ANOVA test on dat
my.ANOVA <- function(){
  
  ##Shows a random sample from dat.
  set.seed(1234)
  dplyr::sample_n(dat, 10)
  
  group_by(dat, factor(class))%>%
    summarise(
      count =n(),
      mean = mean(sysbp, na.rm=T),
      sd = sd(sysbp, na.rm=T)
    )
  
  sink("ANOVA_output.txt", append=F)
  res.aov <- aov(sysbp~class, data = dat)
  print(summary(res.aov))
  print(TukeyHSD(res.aov)) ##CHEEKY BOI
  sink()
}

####################################
##     NOTES  / DIARY             ##
####################################
## QUESTIONS TO ASK IN LAB:
##  * Ask Prof. Creswell during lab whether I can add 
## cholesterol to the graph using the colour function, even if it isnt included 
## as a covariate within the model or whether it must be added to the model and
## interactions must be tested or whether I should leave it out as it is not 
## necessary for the assignment...
## * Ask whether both regression lines should have the same gradient, as there is only 1 factor?
## * Ask about the splitting the populations is a sufficient control, and if the control is include within the model?
##
##
## 20/11/18 - Day 1
##
## The best models were found to be 1, 5, 9. These models do not include
## an interaction between age and smoking. I found that I have been able to
## show 4 independent variable using, x axis = age, shape = smoking + gender
## and colour gradient = cholesterol. I am unsure whether this is stistically 
## correct.
##
## To Do: * Finish the labelling of my.Graph's graphs and the legends - COMPLETED 21/11/18
##        * Solve the issue with my.ModelPlot, I want to be able to print the call 
##          formula from the summary data of the model[[i]], this is not super 
##          important as it could be done manually, but this is a pain in the bum. - COMPLETED 21/11/18
##        * Imrpove visual style of the document and clean up code and files.
##
##\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
##
##  21/11/18 - Day 2
##
## Fixed my.ModelPlot issue with Formula output. Modified the my.ModelPlot function 
## allowing it to act on lists and an singlular models.
## 
## Currently investigating the affect of including cholesterol in the model. 
## 
## To Do: * Study Models including cholesterol - Do this during the lab after asking Prof Creswell
##        * 
## 



