library(MASS)
library(mvtnorm)

####################################################
# Figure 3.2: MA and LS Regression
####################################################

set.seed(12345)
x <- 20 + rnorm(2000)*3
y <- 10 + (x-20)/6 + rnorm(2000)*sqrt(0.75)

mu_y <- mean(y)
fit <- lm(y ~ x)
b <- coef(fit)
yhat <- fitted(fit)
g <- cor(y, yhat)
b1 <- (1 - 1/g) * mu_y + b[1]/g # intercept
b2 <- b[2]/g # slope

par(mfrow=c(1,1))
plot(x,y, col="lightgray",pch=20, ylim = c(6,14), xlim=c(6,31), ylab = "")
abline(b1,  b2, col = "red", lty=1, lwd = 2)
abline(lm(y ~ x), col = "blue", lty=2, lwd = 2)
legend("topleft",c("Maximum agreement","Least-sqaures"),lty=c(1,2), lwd=c(2,2), 
       col=c("red","blue"), cex = 0.8)

simpleMA_data <- data.frame(x=x,y=y)
#write.table(simpleMA_data, file = "simpleMA_data.txt", sep = "\t", row.names = FALSE)



####################################################
# Figure 3.3: y vs yhat
####################################################
ccc_LS <- CCC(y, fitted(fit))
pcc_LS <- cor(y, fitted(fit))

y_fit <- b1 + b2*x
ccc_MA <- CCC(y, y_fit)
pcc_MA <- cor(y, y_fit)

LS_y_ytilde_data <- data.frame(x=y, y=fitted(fit))
MA_y_ytilde_data <- data.frame(x=y, y=y_fit)
#write.table(LS_y_ytilde_data, file = "SMA_LS_data.txt", sep = "\t", row.names = FALSE)
#write.table(MA_y_ytilde_data, file = "SMA_MA_data.txt", sep = "\t", row.names = FALSE)


par(mfrow=c(1,2))
plot(y, y_fit, ylim = c(6,14), xlim=c(6,14), xlab = "Observed", ylab = "Predicted",
     main = "Maximum agreement\n CCC = 0.515, PCC = 0.515", col = "lightgray", pch = 20)
abline(a=0,b=1, col="black", lty=3, lwd=3)

plot(y, fitted(fit), ylim = c(6,14), xlim=c(6,14), xlab = "Observed", ylab = "Predicted",
     main = "Least-sqaures\n CCC = 0.419, PCC = 0.515", col = "lightgray", pch = 20)
abline(a=0,b=1, col="black", lty=3, lwd=3)