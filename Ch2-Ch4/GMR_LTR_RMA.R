#############################################
# Figure 3.1
#############################################
library(MASS)



# Single linear regression (SLR)

par(mfrow=c(1,3))
set.seed(2)
for (i in 1:1000) {
  data<-mvrnorm(35,c(5,6), Sigma=matrix(c(5,5*.7,5*.7,5),2,2), tol=1, empirical=T)
  if (all(data>0) && all(data<10)) break
}
colnames(data)<-c("x","y")
data<-data.frame(data)
data$x<-data$x*10

minx<-0 
maxx<-max(pretty(data$x)) 
miny<-0 
maxy<-max(pretty(data$y))

# Plot
plot(data, axes=F, xlab="", ylab="", main="", pch=16,xlim=c(minx,maxx), ylim=c(miny,maxy), col="black")
abline(lm(y~x,data), lwd=2)
l<- lm(y~x,data)
b<-l$coef[2]
a<-l$coef[1]
axis(1, pos=0, tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
mtext("X", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y",2, line=2)
title_text <- expression("(a) " * bold("LSR"))
mtext(title_text, side = 3, line = 1)
l<-lm(y~x,data)
b<-l$coef[2]
a<-l$coef[1]
segments(data$x,fitted(lm(y~x,data)),data$x, data$y, col="black", lwd=1)
points(y~x,data, pch=16)
data1<-data




# GMR, LTR, RMA

# Example data
set.seed(2)
for (i in 1:1000) {
  data<-mvrnorm(35,mu=c(5,6), Sigma=matrix(c(5, 5*0.7, 5*0.7, 5), 2, 2), tol=1, empirical=T)
  if (all(data>0) && all(data<10)) break
}
colnames(data)<-c("x","y")
data<-data.frame(data)
data$x<-data$x*10
x <- data$x
y <-  data$y
minx<-0
maxx<-max(pretty(x)) 
miny<-0 
maxy<-max(pretty(y))
# Mean values
x_mean <- mean(x)
y_mean <- mean(y)

# Sum of squares
Sxx <- sum((x - x_mean)^2)
Syy <- sum((y - y_mean)^2)
Sxy <- sum((x - x_mean) * (y - y_mean))
# Correlation coefficient
r <- cor(x, y)

# GMR coefficients
beta_G <- sign(r) * sqrt(Syy / Sxx)
alpha_G <- y_mean - beta_G * x_mean
alpha_X <- (x_mean - (sign(r) * sqrt(Sxx / Syy)) * y_mean)
beta_X <- (sign(r) * sqrt(Sxx / Syy))

SLR <- lm(y~x)
alpha_LS <- coef(SLR)[1]
beta_LS <- coef(SLR)[2]

# Fitted values
y_range <- seq(min(y), max(y), length.out=35)
y_hat <- alpha_G + beta_G * x
x_hat <- alpha_X + beta_X * y

# Plot
plot(data,axes=F, main="", xlab="", ylab="", pch=19,xlim=c(minx,maxx), ylim=c(miny,maxy))
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
mtext("X", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y",2, line=2)
title_text <- expression("(b) " * bold("GMR, LTR, RMA"))
mtext(title_text, side = 3, line = 1)

# vertical lines for deviations
for(i in 1:length(x)) {
  segments(x[i], y[i], x[i], y_hat[i], lty="79")
}

# horizontal lines for deviations
for(i in 1:length(x)) {
  segments(x[i], y[i], x_hat[i], y[i], lty="79")
}

i<-41
for(i in 1:length(data$y)){
  polygon(c(x[i],x[i],x_hat[i],x[i]), c(y[i],y_hat[i],y[i],y[i]),col="lightgray", density=50)
}
i<-35
polygon(c(x[i],x[i],x_hat[i],x[i]), c(y[i],y_hat[i],y[i],y[i]),col="gray")
segments(x,y,x,y_hat, col="black", lwd=1,lty="79")
segments(x,y,x_hat,y, col="black", lwd=1, lty="79")
points(x,y, pch=16)
abline(a = alpha_G, b = beta_G, lwd=2)




# All together

plot(data, axes=F, main="", xlab="", ylab="", pch=19,xlim=c(minx,maxx), ylim=c(miny,maxy))
axis(1, pos=0,tick=T, tcl=-.5,lab=T,at=pretty(data$x)[-1])
lines(c(0,max(pretty(data$x))), c(0,0))
mtext("X", 1, line=2)
axis(2,pos=0,las=1,tcl=-.5,tick=T, lab=T, at=pretty(data$y)[-1])
lines(c(0,0),c(0,max(pretty(data$y))))
mtext("Y",2, line=2)
title_text <- expression("(c) " * bold("All"))
mtext(title_text, side = 3, line = 1)

abline(a = alpha_G, b = beta_G, lwd=1)
abline(lm(y~x,data), lwd=1, col="black", lty=1)

text(70,3.5,"LSR", pos=3, col="black",cex=1)
arrows(80,4,80,7.8,length=.05)
lines(c(75,80),c(4,4))

text(88,1.5,"GMR, LTR & RMA",pos=2,col="black",cex=1)
arrows(49,1.5,8,1.5,length=.05)
