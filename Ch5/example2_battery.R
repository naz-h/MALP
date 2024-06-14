#-------------------------------------------------------------------------------
# Example 2
#-------------------------------------------------------------------------------
# This example use the dataset called 'Battery Failure data' (Table 7.5) from
# the book Applied Multivariate statistical analysis 6ed by Johnson & Wichern.
#
# Description: Satellite applications motivated the development of a silver-zinc  
# battery. The dataset below contains failure data collected to characterize the 
# performance of the battery during its life cycle.
# X1 = Charge rate (amps)
# X2 = Discharge rate
# X3 = Dept of discharge (% of rated ampere-hours)
# X4 = Temperature (celcius)
# X5 = End of charge voltage (volts)
# Y  = Cycles to failure

battery <- read.table("T7-5.DAT", header=F)
colnames(battery) <- c("X1", "X2", "X3", "X4", "X5", "Y")


CCC <- function(x,y){
  2*cov(x,y)/(var(x)+var(y)+(mean(x)-mean(y))^2)
}

PCC <- function(x,y){
  cor(x,y)
}

MSE <- function(x,y){
  mean((x-y)^2)
}

# correlation matrix
cor(battery)

# distribution of response variable
hist(battery$Y)


# p = 1
fit_LS1 <- lm(log(Y) ~ X4, data=battery) 
summary(fit_LS1) # Multiple correlation = 0.7189
beta_LS1 <- fit_LS1$coefficients
Yhat_LS1 <- fit_LS1$fitted.values
gamma <- cor(log(battery$Y), Yhat_LS1)
muY <- mean(log(battery$Y))
beta_MA1 <- c((1-1/gamma)*muY+beta_LS1[1]/gamma, beta_LS1[-1]/gamma)
names(beta_MA1) <- names(beta_LS1)
Yhat_MA1 <- beta_MA1[1] + beta_MA1[2] * battery$X4
est_coef <- cbind(beta_LS1, beta_MA1)
pcc <- c(PCC(log(battery$Y), Yhat_LS1), PCC(log(battery$Y), Yhat_MA1))  
ccc <- c(CCC(log(battery$Y), Yhat_LS1), CCC(log(battery$Y), Yhat_MA1))  
mse <- c(MSE(log(battery$Y), Yhat_LS1), MSE(log(battery$Y), Yhat_MA1))
RES1 <- cbind(PCC=pcc, CCC=ccc, MSE=mse)
rownames(RES1) <- c("LS", "MA" )
round(RES1, 3)
round(est_coef, 3)


# p = 2
fit_LS2 <- lm(log(Y) ~ X4 + X2, data=battery) 
summary(fit_LS2) # multiple correlation = 0.7749839
beta_LS2 <- fit_LS2$coefficients
Yhat_LS2 <- fit_LS2$fitted.values
gamma <- cor(log(battery$Y), Yhat_LS2)
muY <- mean(log(battery$Y))
beta_MA2 <- c((1-1/gamma)*muY+beta_LS2[1]/gamma, 
              beta_LS2[2]/gamma, 
              beta_LS2[3]/gamma)
names(beta_MA2) <- names(beta_LS2)
Yhat_MA2 <- beta_MA2[1] + beta_MA2[2] * battery$X4 + beta_MA2[3] * battery$X2
est_coef <- cbind(beta_LS2, beta_MA2)
pcc <- c(PCC(log(battery$Y), Yhat_LS2), PCC(log(battery$Y), Yhat_MA2))  
ccc <- c(CCC(log(battery$Y), Yhat_LS2), CCC(log(battery$Y), Yhat_MA2))  
mse <- c(MSE(log(battery$Y), Yhat_LS2), MSE(log(battery$Y), Yhat_MA2))
RES2 <- cbind(PCC=pcc, CCC=ccc, MSE=mse)
rownames(RES2) <- c("LS", "MA" )
round(RES2, 3)
round(est_coef, 3)

# p = 3
fit_LS3 <- lm(log(Y) ~ X4 + X2 + X1, data=battery) 
summary(fit_LS3) # multiple correlation = 0.7820486
beta_LS3 <- fit_LS3$coefficients
Yhat_LS3 <- fit_LS3$fitted.values
gamma <- cor(log(battery$Y), Yhat_LS3)
muY <- mean(log(battery$Y))
beta_MA3 <- c((1-1/gamma)*muY+beta_LS3[1]/gamma, 
              beta_LS3[2]/gamma, 
              beta_LS3[3]/gamma,beta_LS3[4]/gamma)
names(beta_MA3) <- names(beta_LS3)
Yhat_MA3 <- beta_MA3[1] + beta_MA3[2] * battery$X4 + beta_MA3[3] * battery$X2 + beta_MA3[4] * battery$X1
est_coef <- cbind(beta_LS3, beta_MA3)
pcc <- c(PCC(log(battery$Y), Yhat_LS3), PCC(log(battery$Y), Yhat_MA3))  
ccc <- c(CCC(log(battery$Y), Yhat_LS3), CCC(log(battery$Y), Yhat_MA3))  
mse <- c(MSE(log(battery$Y), Yhat_LS3), MSE(log(battery$Y), Yhat_MA3))
RES3 <- cbind(PCC=pcc, CCC=ccc, MSE=mse)
rownames(RES3) <- c("LS", "MA" )
round(RES3, 3)
round(est_coef, 3)


# p = 4
fit_LS4 <- lm(log(Y) ~ X4 + X2 + X1 + X5, data=battery) 
summary(fit_LS4) # multiple correlation = 0.8080223
beta_LS4 <- fit_LS4$coefficients
Yhat_LS4 <- fit_LS4$fitted.values
gamma <- cor(log(battery$Y), Yhat_LS4)
muY <- mean(log(battery$Y))
beta_MA4 <- c((1-1/gamma)*muY+beta_LS4[1]/gamma, 
              beta_LS4[2]/gamma, beta_LS4[3]/gamma, 
              beta_LS4[4]/gamma, beta_LS4[5]/gamma)
names(beta_MA4) <- names(beta_LS4)
Yhat_MA4 <- beta_MA4[1] + beta_MA4[2] * battery$X4 + beta_MA4[3] * battery$X2 + 
  beta_MA4[4] * battery$X1 + beta_MA4[5] * battery$X5
est_coef <- cbind(beta_LS4, beta_MA4)
pcc <- c(PCC(log(battery$Y), Yhat_LS4), PCC(log(battery$Y), Yhat_MA4))  
ccc <- c(CCC(log(battery$Y), Yhat_LS4), CCC(log(battery$Y), Yhat_MA4))  
mse <- c(MSE(log(battery$Y), Yhat_LS4), MSE(log(battery$Y), Yhat_MA4))
RES4 <- cbind(PCC=pcc, CCC=ccc, MSE=mse)
rownames(RES4) <- c("LS", "MA" )
round(RES4, 3)
round(est_coef, 3)


# p = 5
fit_LS5 <- lm(log(Y) ~ X4 + X2 + X1 + X5 + X3, data=battery) 
summary(fit_LS5) # multiple correlation = 0.8144323
beta_LS5 <- fit_LS5$coefficients
Yhat_LS5 <- fit_LS5$fitted.values
gamma <- cor(log(battery$Y), Yhat_LS5)
muY <- mean(log(battery$Y))
beta_MA5 <- c((1-1/gamma)*muY+beta_LS5[1]/gamma, 
              beta_LS5[2]/gamma, beta_LS5[3]/gamma, 
              beta_LS5[4]/gamma, beta_LS5[5]/gamma, beta_LS5[6]/gamma)
names(beta_MA5) <- names(beta_LS5)
Yhat_MA5 <- beta_MA5[1] + beta_MA5[2] * battery$X4 + beta_MA5[3] * battery$X2 + 
  beta_MA5[4] * battery$X1 + beta_MA5[5] * battery$X5 + beta_MA5[6] * battery$X3
est_coef <- cbind(beta_LS5, beta_MA5)
pcc <- c(PCC(log(battery$Y), Yhat_LS5), PCC(log(battery$Y), Yhat_MA5))  
ccc <- c(CCC(log(battery$Y), Yhat_LS5), CCC(log(battery$Y), Yhat_MA5))  
mse <- c(MSE(log(battery$Y), Yhat_LS5), MSE(log(battery$Y), Yhat_MA5))
RES5 <- cbind(PCC=pcc, CCC=ccc, MSE=mse)
rownames(RES5) <- c("LS", "MA" )
round(RES5, 3)
round(est_coef, 3)

xax <- seq(1,9,by=2)
pcc.ls.ma <- rbind(RES1[,1], RES2[,1], RES3[,1], RES4[,1], RES5[,1])
ccc.ls.ma <- rbind(RES1[,2], RES2[,2], RES3[,2], RES4[,2], RES5[,2])
mse.ls.ma <- rbind(RES1[,3], RES2[,3], RES3[,3], RES4[,3], RES5[,3])

# Prediction performance plot
par(mar=c(5,4,4,4))
plot(1:5, pcc.ls.ma[,1], xaxt='n', main="Prediction Performance",
     xlab="Subset", ylab="PCC and CCC",ylim=c(0.68,0.85),
     type='l', lwd=3, col="lightblue", cex=0.8)
axis(1, at=1:5, labels=LETTERS[1:5])
lines(1:5, ccc.ls.ma[,1], col="blue", lty=3, lwd=1, type="b", pch=16, cex=0.8) # lslp
lines(1:5, ccc.ls.ma[,2], col="red", lty=2, lwd=1, type="b", pch=16, cex=0.8) # malp
legend("topleft", c("PCC","CCC:LSLP","CCC:MALP","MSE:LSLP","MSE:MALP"),
       lty=c(1,3,2,4,5), col=c("lightblue","blue","red","blue","red"), 
       pch=c(NA,16,16,4,4),cex=0.75)
box()
par(new=TRUE)
plot(1:5, mse.ls.ma[,1], xaxt='n', main="Prediction Performance",
     xlab="", ylab="", ylim=c(min(mse.ls.ma[,1]),max(mse.ls.ma[,2])+0.2),
     type='b', lty=4, col="blue",  lwd=1, axes=FALSE, pch=4, cex=0.8) # lslp
axis(4, ylim=c(min(mse.ls.ma[,1]),max(mse.ls.ma[,2])+0.2),las=3)
lines(1:5, mse.ls.ma[,2], col="red",type="b", lty=5, lwd=1, pch=4, cex=0.8) # malp
mtext("MSE", side=4, line=2)


# y vs yhat plots
yhat.ls <- cbind(Yhat_LS1, Yhat_LS2, Yhat_LS3, Yhat_LS4, Yhat_LS5)
yhat.ma <- cbind(Yhat_MA1, Yhat_MA2, Yhat_MA3, Yhat_MA4, Yhat_MA5)

par(mar=c(3.5, 4.1, 2, 2.1),mfrow=c(1,2))

plot(log(battery$Y), yhat.ls[,1], cex=0.6, xlab="", ylab="yhat_LS", main="LSLP: subset A")
mtext("log(Y)", side=1, line=2)
abline(a=0, b=1, lty=2)
plot(log(battery$Y), yhat.ma[,1], cex=0.6, pch=16,
     xlab="", ylab="yhat_MA", main="MALP: subset A")
mtext("log(Y)", side=1, line=2)
abline(a=0, b=1, lty=2)

plot(log(battery$Y), yhat.ls[,2], cex=0.6, xlab="", ylab="yhat_LS", main="LSLP: subset B")
abline(a=0, b=1, lty=2)
mtext("log(Y)", side=1, line=2)
plot(log(battery$Y), yhat.ma[,2], cex=0.6, pch=16,
     xlab="", ylab="yhat_MA", main="MALP: subset B")
mtext("log(Y)", side=1, line=2)
abline(a=0, b=1, lty=2)

plot(log(battery$Y), yhat.ls[,3], cex=0.6, xlab="", ylab="yhat_LS", main="LSLP: subset C")
abline(a=0, b=1, lty=2)
mtext("log(Y)", side=1, line=2)
plot(log(battery$Y), yhat.ma[,3], cex=0.6, pch=16,
     xlab="", ylab="yhat_MA", main="MALP: subset C")
mtext("log(Y)", side=1, line=2)
abline(a=0, b=1, lty=2)

plot(log(battery$Y), yhat.ls[,4], cex=0.6, xlab="", ylab="yhat_LS", main="LSLP: subset D")
abline(a=0, b=1, lty=2)
mtext("log(Y)", side=1, line=2)
plot(log(battery$Y), yhat.ma[,4], cex=0.6, pch=16,
     xlab="", ylab="yhat_MA", main="MALP: subset D")
abline(a=0, b=1, lty=2)
mtext("log(Y)", side=1, line=2)

plot(log(battery$Y), yhat.ls[,5], cex=0.6, xlab="", ylab="yhat_LS", main="LSLP: subset E")
abline(a=0, b=1, lty=2)
mtext("log(Y)", side=1, line=2)
plot(log(battery$Y), yhat.ma[,5], cex=0.6, pch=16,
     xlab="", ylab="yhat_MA", main="MALP: subset E")
abline(a=0, b=1, lty=2)
mtext("log(Y)", side=1, line=2)