
###################################################################################################
# Abraham Moyal 
# 24.10.22
# Semester Project
# OD measurement of E.coli culture
###################################################################################################


###################################################################################################

# Initialize time point vector of sampling (OD measurements) in minutes
#----------------------------------------------------------------------
x <- c(0,40,75,140,203,280,348,408,555,680,1262,1336,1412,1554)



# Initialize OD values for shake flask 1:
#----------------------------------------

y1 <- c(0.05,0.11,0.15,0.26,0.5,1.1,1.72,3.2,6.2,6.2,5.8,5,5.4,5.8)


# Initialize OD values for shake flask 2:
#----------------------------------------
y2 <- c(0.05,0.1,0.15,0.26,0.52,0.92,1.76,3.2,6,6,5.6,5,5.4,5.8)


# Exponential fit of the form y=a*exp(bt) true for the first few measuremts
#--------------------------------------------------------------------------

model1 <- nls(y1~y1[1]*exp(a*x),start = list( a=0.000001))


# Logistic fit of the form y=K/(1+(K-y(0)/y(0))*exp(-rt)) overall better fit:
#----------------------------------------------------------------------------
model2 <- nls(y1 ~ a/ ( 1 +   (( (a-y1[1])/y1[1])*exp(-r*x)) )   ,start = list( a = 20,r = 0.1))



layout(matrix(c(1,2),ncol = 1), heights = c(8,2))


# Plot data points (raw data) of first flask:
#--------------------------------------------

par(mar=c(4.5,4.6,3,3))
plot(x,y1, ylab="",xlab="Time [min]",main="Growth Curve (Flask 1)",xlim=c(0,1500),ylim=c(0,10))

mtext(2,text=expression(OD[600]*"(t)"),line = 2.5) # added ylabel 





# Add regression fits to plot (model1)
#-------------------------------------

new_x <- seq(x[1],x[length(x)]+100,length=3000) # creates new sequence of x


new_y <- predict(model1,list(x=new_x)) # predicted seuqnce of y 


lines(new_x,new_y,lty = 2,col="red",lwd=2) # add line


points(x,y1,pch=16)



# Add regression fits to plot (model2)
#-------------------------------------

new_x <- seq(x[1],x[length(x)]+100,length=3000)


new_y <- predict(model2,list(x=new_x))



lines(new_x,new_y,lty = 2,col="blue",lwd=2)



points(x,y1,pch=16)



# Highlight datapoints used for further analysis:
#------------------------------------------------

segments(x0=x[6],y0=-200,y1=y1[6],lty=2) #vertical line
segments(x0=-200, y0=y1[6], x1=x[6],lty=2)

segments(x0=x[12],y0=-200,y1=y1[12],lty=2) #vertical line
segments(x0=-200, y0=y1[12], x1=x[12],lty=2)


# Addition of legend:
#--------------------

legend("topleft",col = c("red","blue"),
       legend = c(paste0("Exponential regression"),
                  paste0("Logistic regression")),
       lty = 2,lwd = 2,bty="n")




# Add additional information such as Growth rate and Doubling time:
#------------------------------------------------------------------
par(mar=c(0,2,0,0))
plot(0,0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
legend("topleft",legend = c(paste0("Growth rate: ",round(summary(model1)$parameters[1],digits=3), " and doubling time: ", round(log(2)/summary(model1)$parameters[1],digits=3)),
                          paste0("Growth rate: ",round(summary(model2)$parameters[2],digits=3), " and doubling time: " ,round(log(2)/summary(model2)$parameters[2],digits=3))),
       text.col=c("red","blue"),bty="n")




###################################################################################################

# Second plot
#------------


###################################################################################################


# Exponential fit of the form y=a*exp(bt) true for the first few measuremts
#--------------------------------------------------------------------------

model1 <- nls(y2~y2[1]*exp(a*x),start = list( a=0.000001))


# Logistic fit of the form y=K/(1+(K-y(0)/y(0))*exp(-rt)) overall better fit:
#----------------------------------------------------------------------------
model2 <- nls(y2 ~ a/ ( 1 +   (( (a-y2[1])/y2[1])*exp(-r*x)) )   ,start = list( a = 20,r = 0.1))



layout(matrix(c(1,2),ncol = 1), heights = c(8,2))


# Plot data points (raw data) of first flask:
#--------------------------------------------

par(mar=c(4.5,4.6,3,3))
plot(x,y2, ylab="",xlab="Time [min]",main="Growth Curve (Flask 2)",xlim=c(0,1500),ylim=c(0,10))

mtext(2,text=expression(OD[600]*"(t)"),line = 2.5) # added ylabel 





# Add regression fits to plot (model1)
#-------------------------------------

new_x <- seq(x[1],x[length(x)]+100,length=3000) # creates new sequence of x


new_y <- predict(model1,list(x=new_x)) # predicted seuqnce of y 


lines(new_x,new_y,lty = 2,col="red",lwd=2) # add line


points(x,y2,pch=16)



# Add regression fits to plot (model2)
#-------------------------------------

new_x <- seq(x[1],x[length(x)]+100,length=3000)


new_y <- predict(model2,list(x=new_x))



lines(new_x,new_y,lty = 2,col="blue",lwd=2)



points(x,y2,pch=16)



# Highlight datapoints used for further analysis:
#------------------------------------------------

segments(x0=x[6],y0=-200,y1=y2[6],lty=2) #vertical line
segments(x0=-200, y0=y2[6], x1=x[6],lty=2)

segments(x0=x[12],y0=-200,y1=y2[12],lty=2) #vertical line
segments(x0=-200, y0=y2[12], x1=x[12],lty=2)


# Addition of legend:
#--------------------

legend("topleft",col = c("red","blue"),
       legend = c(paste0("Exponential regression"),
                  paste0("Logistic regression")),
       lty = 2,lwd = 2,bty="n")




# Add additional information such as Growth rate and Doubling time:
#------------------------------------------------------------------
par(mar=c(0,2,0,0))
plot(0,0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
legend("topleft",legend = c(paste0("Growth rate: ",round(summary(model1)$parameters[1],digits=3), " and doubling time: ", round(log(2)/summary(model1)$parameters[1],digits=3)),
                            paste0("Growth rate: ",round(summary(model2)$parameters[2],digits=3), " and doubling time: " ,round(log(2)/summary(model2)$parameters[2],digits=3))),
       text.col=c("red","blue"),bty="n")


















