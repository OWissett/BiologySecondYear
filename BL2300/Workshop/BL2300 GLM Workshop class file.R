###Import and check data
exp1 <- read.table("H:/temp/AssessedPractical/Graphing.csv", header=T, sep="," )
exp1

###First model
model11 <- lm(cellcount ~ drug + dosage , data=exp1)
summary(model11)


###illustrate results
par(mfrow=c(1,1))
plot(cellcount ~ dosage, data=exp1, pch=drug1, ylab="Cell count", xlab="Dosage")

abline(80.65, -2.03)

newx <- seq(0,20,length=1000)

newyA <- (-2.0 * newx) + (80.65 + (- 11.4))
lines(newx, newyA, col="red")
text(17,35, "Drug A", col="red")

newyc <- (-2.0 * newx) + (80.65 + (0))
lines(newx, newyc, col="blue")
text(17,60, "No treatment", col="blue")


###test model assumptions
hist(resid(model11))
par(mfrow=c(2,2))
plot(model11)


###remove outlier
exp1$sel <- ifelse(exp1$drug1==0 & exp1$dosage==6,(0),(1)) ##Find outlier first
exp1
dim(exp1)
exp2 <- subset(exp1, sel==1) 
dim(exp2) ## Creates new set that does not include the outlier

####################################################model without outlier
model12 <- lm(cellcount ~ drug + dosage , data=exp2)
summary(model12)

##################################################################
##################################################################
###1. illustrate results

par(mfrow=c(1,1))
plot(cellcount ~ dosage, data=exp2, pch=drug1, ylab="Cell count", xlab="Dosage")

##################################################################
##################################################################

###model with two separate lines?
##illustrate model
expA <- subset(exp2, drug1==1)
expA
expC <- subset(exp2, drug1==0)
expC

par(mfrow=c(1,1))

plot(cellcount ~ dosage, data=exp1, type="n", ylab="Cell count", xlab="Dosage")
points(expA$dosage, expA$cellcount, pch=1, col="red")
points(expC$dosage, expC$cellcount, pch=2, col="blue")

modelA <- lm(cellcount ~ dosage, data=expA)
summary(modelA)

newx <- seq(0,20,length=1000)

newyA <- (-2.93 * newx) + (78.69)
lines(newx, newyA, col="red")
text(17,35, "Drug A", col="red")

modelC <- lm(cellcount ~ dosage, data=expC)
summary(modelC)

newyC <- (-1.37 * newx) + (75.48)
lines(newx, newyC, col="blue")
text(17,60, "Control", col="blue")


#######test whether interaction is significant
model13 <- lm(cellcount ~ drug + dosage + drug*dosage, data=exp2) ##Covariate * factor ==> Gradient
summary(model13)

##################################################################
##################################################################
###2. test model assumptions
hist(resid(model13))
par(mfrow=c(2,2))
plot(model13)

##################################################################
##################################################################

##################################################################
##################################################################

##########################################check you have the simplest model
AIC(model12, model13)

###All possible models
##make sure you use the same data set for each model or AIC doesn't work
model13 <- lm(cellcount ~ drug + dosage + drug*dosage , data=exp2) ##Best Model
model12 <- lm(cellcount ~ drug + dosage , data=exp2)
model14 <- lm(cellcount ~ drug  , data=exp2)
model15 <- lm(cellcount ~ dosage  , data=exp2)
###intercept only
model16 <- lm(cellcount ~ 1  , data=exp2) ##Worst Model
summary(model16)
AIC( model13, model12,  model15,  model14, model16)

