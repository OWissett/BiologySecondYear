
##Import and check data, file must exist as a CSV using comma delimiting 
exp1 <- read.table("H:/temp/Graphing.csv", header=T, sep="," )


##check data
exp1
str(exp1)


##Think about factors and covariates
plot(cellcount ~ drug1, data=exp1)

##classifying a variable as a factor
exp1$drug2 <- factor(exp1$drug1)
str(exp1)
plot(cellcount ~ drug2, data=exp1)


##example of Boxplot
plot(cellcount ~ drug, data=exp1)


##example of Bar chart
##Model to get mean values and standard errors to plot a bar chart
model1 <- lm(cellcount ~ drug, data=exp1)
summary(model1)

##########################ALL BAR CHARTS IN UNIVERSE#####################
  ##illustrate results with a bar chart
  y.means <- c(59.3, 59.3 +(-11.4)) ##mean predicted values 
  y.se <- c(3.23, 4.57)
  
  ##Use this bit of code below to CREATE A FUNCTION TO draw error bars 
  error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
      stop("vectors must be same length")
    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)}
  
  ##line below plots 1 graph on the page c(2,2) would plot 4 graphs
  par(mfrow=c(1,1))
  
  ##plots bar chart of what you have stated above is y.means
  barx <- barplot(y.means,ylim=c(0,70), col=c("","Grey"))
  
  ##adds standard errors of what you have stated above is y.se
  error.bar(barx,y.means, y.se)
  
  ##adds labels
  mtext(side=3,"Affect of Drug A on cell count", cex=1.5)
  mtext(side=2, adj=0.5,line=2.8,"Mean Cell Count +/- 1SE")
  mtext(side=1, line=1, adj = 0.25, "Control")
  mtext(side=1, line=1, adj = 0.75, "Treatment")
  
######################################################################################

##example of Histograms
plot(hist(resid(model1)))
hist(exp1$cellcount)


##example of a scatter plot
plot(cellcount ~ dosage, data=exp1)

##subset to split the two treatments up for separate plotting
expA <- subset(exp1, drug1==1)
expA
expC <- subset(exp1, drug1==0)
expC

##plot graph without points
plot(cellcount ~ dosage, data=exp1, type="n", ylab="Cell count", xlab="Dosage")

##add each group of points
points(expA$cellcount ~ expA$dosage, pch=1, col="red")
points(expC$cellcount ~ expC$dosage, pch=2, col="blue")

##work out the gradient and the intercept of the line of best fit
##i.e. a linear model - work it out for each group separately, e.g. expA treatment group first
modelA <- lm(cellcount ~ dosage, data=expA)
summary(modelA)

##calculate a range of x variables to use to calculate predicted y values
## when the x and y values are plotted they will form a straight line
## showing what the model predicts (i.e. the line of best fit)
newx <- seq(0,20,length=1000)

##use the parameters from the summary table - the gradient and the intercept
##to calculate predicted values of y
## i.e. y = mx + c (formula for any straight line)
summary(modelA)
newyA <- (-2.93 * newx) + (78.69)

##example predicted points
#points(1, 75.75, pch=15)
#points(10, 49.38, pch=15)
#points(20, 20.08, pch=15)

##plot the fitted line
lines(newyA ~ newx, col="red")

##add a text label to identify the line
text(17,35, "Drug A", col="red")

##add standard error lines
##add standard error of intercept to intercept, and of the parameter estimate
## to the parameter estimate to get upper limit (+1 SE)
newyAupper <- ((-2.93+0.10) * newx) + (78.69+1.2)
lines(newyAupper ~newx, col="red", lty=3)
##subtract standard error of intercept from intercept, and of the parameter estimate
## from the parameter estimate to get lower limit (-1 SE)
newyAlower <- ((-2.93-0.10) * newx) + (78.69-1.2)
lines(newyAlower ~newx, col="red", lty=3)

###repeat for the control treatment
modelC <- lm(cellcount ~ dosage, data=expC)
summary(modelC)

newyC <- (-1.14 * newx) + (71.22)
lines(newyC ~newx, col="blue")
text(17,60, "Control", col="blue")

##add standard error lines
newyCupper <- ((-1.14+0.32) * newx) + (71.22+3.82)
lines(newyCupper ~newx, col="blue", lty=3)
newyClower <- ((-1.14-0.32) * newx) + (71.22-3.82)
lines(newyClower ~newx, col="blue", lty=3)

###add an overall title for the graph
mtext(side=3,"Affect of Treatment A on cell count", cex=1.5, line=1.5)

############################################################Practice

