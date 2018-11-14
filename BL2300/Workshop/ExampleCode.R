##Import data file
cat <- read.table("H:/temp/AssessedPractical/catfleas4.csv", header=T, sep="," )
cat
head(cat)

##The data file contains the mean density of cat fleas (in 20 x 5cm squared quadrats) found in n = 89 different  houses
## We want to know what determines flea density per house
# We have measurements of 
## Whether the house has carpet = 1 or not = 0 (carpet)
## the number of cats (cats)
## if the cat has been treated for fleas in the last week (trtmt) 0 = no treatment 1 = treatment

## We have reason to think that the effect of treatment depends on 
## whether there are carpets present or not

###Test the hypotheses that:
## 1. Houses with treated cats have fewer fleas, but the effect of treatment may depend on whether a house has carpets
## 2. Houses with more cats have more fleas
## 3. Houses with carpets have more fleas
###NOTE YOU DON'T HAVE TO TEST THE HYPOTHESES WITH SEPARATE MODELS - THEY CAN BE TESTED SIMULTANEOUSLY IN THE SAME MODEL

##Examine data 
################VERY IMPORTANT!!!!!!!!!!!!!! Set factors
## Determine the best model to predict cat fleas - you are looking for the best, simplest model to test
## all three hypotheses simutaneously (i.e. only one final model)
## Illustrate and interpret your results


#####IGNORE THIS IF YOU WANT TO KEEP THINGS SIMPLE
####################################################################################################
####################################################################################################
###################################################################################################
###Some useful code for advanced plotting, i.e. Illustrate results with standard errors
###

### Make a variable that allows you to plot different symbols for treatment and carpet types
cat$class[cat$carpet==0 & cat$trtmt==0] <- 0
cat$class[cat$carpet==0 & cat$trtmt==1] <- 1
cat$class[cat$carpet==1 & cat$trtmt==0] <- 2
cat$class[cat$carpet==1 & cat$trtmt==1] <- 3
cat$class1 <- factor(cat$class)

#################################Plotting predicted lines
######Using predict function to obtain predicted values
##doing this manually is tricky when you have several variables in a model 
str(cat)
##create blank data frame to put in range of values to calculate predicted lines

#####For scatter plot (i.e. fleas by cat with different lines for carpets and treatments
fit1 <- data.frame('carpet' = rep(c("0", "1", "0", "1"), each = 1000),
                   'class' = rep(c("0", "1", "2", "3"), each = 1000),
                   'trtmt' = rep(c("0", "0", "1", "1"), each = 1000),
                   'ncats' = c(seq(-0.06, 4.06, length.out = 1000),
                               seq(-0.06, 4.06, length.out = 1000),
                               seq(-0.06, 4.06, length.out = 1000),
                               seq(-0.06, 4.06, length.out = 1000)))
head(fit1)
###work out predicted values
fit2 <- data.frame(fit1, predict(model2, fit1, se.fit = T))
head(fit2)

##For bar chart using predicted values; for 1 cat
fit3 <- data.frame('carpet' = rep(c("0", "1", "0", "1"), each = 1),
                   'class' = rep(c("0", "1", "2", "3"), each = 1),
                   'trtmt' = rep(c("0", "0", "1", "1"), each = 1),
                   'ncats' = rep(c(1), each=4)) ##here we set ncats to 1
fit3
###work out predicted values
fit4 <- data.frame(fit3, predict(model2, fit3, se.fit = T)) ##NOTE CHANGE model2 to your best model name
fit4
str(fit4)

##################################################More helpful code
##create a new variable so we can colour points according to treatment
## that works well in ggplot format
cat$treat3[cat$class==0] <- "red"
cat$treat3[cat$class==1] <- "blue"
cat$treat3[cat$class==2] <- "black"
cat$treat3[cat$class==3] <- "purple"

##Points overlap so we will "jitter" them so they don't
##add a small value to number of cats - different for each class so they
##don't overlap
##don't worry about the warning message here
## check all is OK by looking at the data
cat$ncats1[cat$class==0] <- cat$ncats
cat$ncats1[cat$class==1] <- cat$ncats + 0.06
cat$ncats1[cat$class==2] <- cat$ncats - 0.06
cat$ncats1[cat$class==3] <- cat$ncats + 0.12
cat

###when adding each group of points - specifying different symbols and colours
##some example code
points(fleas ~ ncats1, pch=class, col=treat3, data=cat)

###subset predicted values for easier code for line plotting
class0 <- subset(fit2, class==0) ###NOTE CHANGE fit2 here and below to whatever dataframe name you want to subset
class1 <- subset(fit2, class==1)
class2 <- subset(fit2, class==2)
class3 <- subset(fit2, class==3)
