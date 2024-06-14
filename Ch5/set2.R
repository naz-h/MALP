source("set2_func.R")


# Generate samples when p = 1
set.seed(123)
d1_1p <- set2_gen_data_p(n=1000, beta=c(10,1), para=c(2,50), p=1, B=5000)
set.seed(123)
d2_1p <- set2_gen_data_p(n=1000, beta=c(10,1), para=c(3,5), p=1, B=5000)
set.seed(123)
d3_1p <- set2_gen_data_p(n=1000, beta=c(10,1), para=c(5,2), p=1, B=5000)

# Generate samples when p = 2
set.seed(123)
d1 <- set2_gen_data_p(n=1000, beta=c(10,1, 0.5), para=c(2,50), p=2, B=5000)
set.seed(123)
d2 <- set2_gen_data_p(n=1000, beta=c(10,1, 0.5), para=c(3,5), p=2, B=5000)
set.seed(123)
d3 <- set2_gen_data_p(n=1000, beta=c(10,1, 0.5), para=c(5,2), p=2, B=5000)


set2_m1_p1 <- set2_test(d1_1p, B=5000)
set2_m2_p1 <- set2_test(d2_1p, B=5000)
set2_m3_p1 <- set2_test(d3_1p, B=5000)
set2_m1_p1$MA_eval; set2_m1_p1$LS_eval; set2_m1_p1$XY_eval
set2_m2_p1$MA_eval; set2_m2_p1$LS_eval; set2_m2_p1$XY_eval
set2_m3_p1$MA_eval; set2_m3_p1$LS_eval; set2_m3_p1$XY_eval

t(round(set2_m1_p1$XY_eval, 3))
t(round(set2_m3_p1$MA_eval[,-5], 3))
t(round(set2_m3_p1$LS_eval[,-5], 3))

set2_m1_p2 <- set2_test_p(d1, B=5000)
set2_m2_p2 <- set2_test_p(d2, B=5000)
set2_m3_p2 <- set2_test_p(d3, B=5000)
set2_m1_p2$MA_eval; set2_m1_p2$LS_eval; set2_m1_p2$XY_eval
set2_m2_p2$MA_eval; set2_m2_p2$LS_eval; set2_m2_p2$XY_eval
set2_m3_p2$MA_eval; set2_m3_p2$LS_eval; set2_m3_p2$XY_eval


t(round(set2_m3_p2$XY_eval, 3))
t(round(set2_m3_p2$MA_eval, 3))
t(round(set2_m3_p2$LS_eval, 3))


# Prediction performance result
par(mfrow=c(1,2),mar=c(5,4,4,4))
# p = 1
plot(c(set2_m1_p1$MA_eval[1,3],set2_m2_p1$MA_eval[1,3],set2_m3_p1$MA_eval[1,3]), 
     xaxt='n',xlab="Parameter set", ylab="PCC & CCC",ylim=c(0,1.5), type="o", lwd=3, col="lightblue",
     main = "Set 2: p = 1")
axis(1, at=1:3, labels=c(1:3))
points(c(set2_m1_p1$LS_eval[1,4],set2_m2_p1$LS_eval[1,4],set2_m3_p1$LS_eval[1,4]), type="o", pch=18, lty=3, col="blue")
points(c(set2_m1_p1$MA_eval[1,4],set2_m2_p1$MA_eval[1,4],set2_m3_p1$MA_eval[1,4]), type="o", pch=18, col="red", lty=2)
legend("topright", c("PCC","CCC:LSLP","CCC:MALP","MSE:LSLP","MSE:MALP"),
       lty=c(1,3,2,4,5), col=c("lightblue","blue","red","blue","red"), 
       pch=c(NA,16,16,4,4),cex=0.75)
par(new=TRUE)
plot(c(set2_m1_p1$LS_eval[1,6],set2_m2_p1$LS_eval[1,6],set2_m3_p1$LS_eval[1,6]),ylim=c(0,4678),
     type="o", axes=F, pch=4, ylab="", xlab="", col="blue", lty=3)
points(c(set2_m1_p1$MA_eval[1,6],set2_m2_p1$MA_eval[1,6],set2_m3_p1$MA_eval[1,6]), type="o", pch=4, col="red", lty=2)
axis(4, ylim=c(0,4678),las=3)
mtext("MSE", side=4, line=2)

# p = 2
plot(c(set2_m1_p2$MA_eval[1,3],set2_m2_p2$MA_eval[1,3],set2_m3_p2$MA_eval[1,3]), 
     xaxt='n',xlab="Parameter set", ylab="PCC & CCC",ylim=c(0,1.5), type="o", lwd=3, col="lightblue",
     main = "Set 2: p = 2")
axis(1, at=1:3, labels=c(1:3))
points(c(set2_m1_p2$LS_eval[1,4],set2_m2_p2$LS_eval[1,4],set2_m3_p2$LS_eval[1,4]), type="o", pch=18, lty=3, col="blue")
points(c(set2_m1_p2$MA_eval[1,4],set2_m2_p2$MA_eval[1,4],set2_m3_p2$MA_eval[1,4]), type="o", pch=18, col="red", lty=2)
legend("topright", c("PCC","CCC:LSLP","CCC:MALP","MSE:LSLP","MSE:MALP"),
       lty=c(1,3,2,4,5), col=c("lightblue","blue","red","blue","red"), 
       pch=c(NA,16,16,4,4),cex=0.75)
par(new=TRUE)
plot(c(set2_m1_p2$LS_eval[1,6],set2_m2_p2$LS_eval[1,6],set2_m3_p2$LS_eval[1,6]),ylim=c(0,4678),
     type="o", axes=F, pch=4, ylab="", xlab="", col="blue", lty=3)
points(c(set2_m1_p2$MA_eval[1,6],set2_m2_p2$MA_eval[1,6],set2_m3_p2$MA_eval[1,6]), type="o", pch=4, col="red", lty=2)
axis(4, ylim=c(0,4678),las=3)
mtext("MSE", side=4, line=2)


# beta coef boxplots p = 1
par(mfrow=c(1,2))
boxplot(cbind(set2_m1_p1$est_coefs[,c(1,3)],set2_m2_p1$est_coefs[,c(1,3)],set2_m3_p1$est_coefs[,c(1,3)]),
        names = c("MA:1","LS:1","MA:2","LS:2","MA:3","LS:3"), xaxs = F)
mtext(expression(paste("Panel A: ", beta[0])), side=3)
legend("topright", c(expression(paste("1:",rho==0.04,"  ")), 
                     expression(paste("2:",rho==0.5,"  ")), 
                     expression(paste("3:",rho==0.9,"  ")) ), pch=NA)
boxplot(cbind(set2_m1_p1$est_coefs[,c(2,4)],set2_m2_p1$est_coefs[,c(2,4)],set2_m3_p1$est_coefs[,c(2,4)]),
        names = c("MA:1","LS:1","MA:2","LS:2","MA:3","LS:3"), xaxs = F)
mtext(expression(paste("Panel B: ", beta[1])), side=3)
legend("topright", c(expression(paste("1:",rho==0.04,"  ")), 
                     expression(paste("2:",rho==0.5,"  ")), 
                     expression(paste("3:",rho==0.9,"  ")) ), pch=NA)

# beta coef boxplots p = 2
par(mfrow=c(1,3))
boxplot(cbind(set2_m1_p2$est_coefs[,c(1,4)],set2_m2_p2$est_coefs[,c(1,4)],set2_m3_p2$est_coefs[,c(1,4)]),
        names = c("MA:1","LS:1","MA:2","LS:2","MA:3","LS:3"), xaxs = F)
mtext(expression(paste("Panel A: ", beta[0])), side=3)
legend("topright", c(expression(paste("1:",rho==0.04,"  ")), 
                     expression(paste("2:",rho==0.5,"  ")), 
                     expression(paste("3:",rho==0.8,"  ")) ), pch=NA)
boxplot(cbind(set2_m1_p2$est_coefs[,c(2,5)],set2_m2_p2$est_coefs[,c(2,5)],set2_m3_p2$est_coefs[,c(2,5)]),
        names = c("MA:1","LS:1","MA:2","LS:2","MA:3","LS:3"), xaxs = F)
mtext(expression(paste("Panel B: ", beta[1])), side=3)
legend("topright", c(expression(paste("1:",rho==0.04,"  ")), 
                     expression(paste("2:",rho==0.5,"  ")), 
                     expression(paste("3:",rho==0.8,"  ")) ), pch=NA)
boxplot(cbind(set2_m1_p2$est_coefs[,c(3,6)],set2_m2_p2$est_coefs[,c(3,6)],set2_m3_p2$est_coefs[,c(3,6)]),
        names = c("MA:1","LS:1","MA:2","LS:2","MA:3","LS:3"), xaxs = F)
mtext(expression(paste("Panel C: ", beta[2])), side=3)
legend("topright", c(expression(paste("1:",rho==0.04,"  ")), 
                     expression(paste("2:",rho==0.5,"  ")), 
                     expression(paste("3:",rho==0.8,"  ")) ), pch=NA)


# Difference, Abs. difference, Euclidean dist.
coef_diff1 <- cbind(set2_m1_p1$comp_coef, set2_m1_p2$comp_coef)
coef_diff2 <- cbind(set2_m2_p1$comp_coef, set2_m2_p2$comp_coef)
coef_diff3 <- cbind(set2_m3_p1$comp_coef, set2_m3_p2$comp_coef)



#Fit Ellipse
library("ellipse")
x1 = d1_1p[[1]][,1]
y1 = d1_1p[[1]][,2]
x2 = d2_1p[[1]][,1]
y2 = d2_1p[[1]][,2]
x3 = d3_1p[[1]][,1]
y3 = d3_1p[[1]][,2]

eli1 = ellipse::ellipse(cor(x1,y1),scale=c(sd(x1),sd(y1)), centre=c(mean(x1), mean(y1)), level = 0.95, npoints = 250)
eli2 = ellipse::ellipse(cor(x2,y2),scale=c(sd(x2),sd(y2)), centre=c(mean(x2), mean(y2)), level = 0.95, npoints = 250)
eli3 = ellipse::ellipse(cor(x3,y3),scale=c(sd(x3),sd(y3)), centre=c(mean(x3), mean(y3)), level = 0.95, npoints = 250)

#Draw ellipse and points
par(mfrow=c(1,3))
plot(eli1[,1], eli1[,2], type = "l",col="gray48", 
     lwd = 1.8, lty = "D3", xlab="x", ylab="y", main=expression(paste("Set 2: ",rho==0.04,"  "))) 
abline(a=set2_m1_p1$MA_eval[1,1], b=set2_m1_p1$MA_eval[2,1], col="red", lty=5,lwd = 2, )
abline(a= set2_m1_p1$LS_eval[1,1] , b=set2_m1_p1$LS_eval[2,1]  , col="blue", lty=3,lwd = 2, )
legend("topleft", c("MALP","LSLP"), lty=c(5,3), col=c("red", "blue"))

plot(eli2[,1], eli2[,2], type = "l",col="gray48", 
     lwd = 1.8, lty = "D3", xlab="x", ylab="y", main=expression(paste("Set 2: ",rho==0.5,"  ")))
abline(a=set2_m2_p1$MA_eval[1,1], b=set2_m2_p1$MA_eval[2,1], col="red", lty=5,lwd = 2, )
abline(a= set2_m2_p1$LS_eval[1,1] , b=set2_m2_p1$LS_eval[2,1]  , col="blue", lty=3,lwd = 2, )
legend("topleft", c("MALP","LSLP"), lty=c(5,3), col=c("red", "blue"))

plot(eli3[,1], eli3[,2], type = "l",col="gray48", 
     lwd = 1.8, lty = "D3", xlab="x", ylab="y", main=expression(paste("Set 2: ",rho==0.9,"  "))) 
abline(a=set2_m3_p1$MA_eval[1,1], b=set2_m3_p1$MA_eval[2,1], col="red", lty=5,lwd = 2, )
abline(a= set2_m3_p1$LS_eval[1,1] , b=set2_m3_p1$LS_eval[2,1]  , col="blue", lty=3,lwd = 2, )
legend("topleft", c("MALP","LSLP"), lty=c(5,3), col=c("red", "blue"))

