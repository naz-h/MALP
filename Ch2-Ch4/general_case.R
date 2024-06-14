##########################################################
# Figure 4.1: Illustration for the General Case
##########################################################
library(MASS)
library(mvtnorm)
library(mnormt)
library(mixtools)

n <- 1000

#-------------------------------------------------------------------------------
# Panel A
#-------------------------------------------------------------------------------
rho_A = 0.2
mu_A = c(0, 0)
sigma1_A = c(1, 0.5)
sigma2_A = 1
Sigma12_1_A = sigma1_A[1] * sigma2_A * rho_A
Sigma12_2_A = sigma1_A[2] * sigma2_A * rho_A

VarCov1_A <- matrix(c(sigma2_A, Sigma12_1_A, Sigma12_1_A, sigma1_A[1]^2), nrow = 2) 
VarCov2_A <- matrix(c(sigma2_A, Sigma12_2_A, Sigma12_2_A, sigma1_A[2]^2), nrow = 2)

data_A <- rmvnorm(n, mu = mu_A, sigma = VarCov1_A)
Y <- data_A[, 1]  
X <- data_A[, -1]
data_A <- data.frame(X,Y)

# sigma1 = 1
beta0_LS1_A <- mu_A[1] - rho_A * (sigma1_A[1]/sigma2_A) * mu_A[2]
beta1_LS1_A <- rho_A * (sigma1_A[1]/sigma2_A)
beta0_MA1_A <- mu_A[1] - sign(rho_A) * (sigma1_A[1]/sigma2_A) * mu_A[2]
beta1_MA1_A <- sign(rho_A) * (sigma1_A[1]/sigma2_A)

# sigma1 = 0.5
beta0_LS2_A <- mu_A[1] - rho_A * (sigma1_A[2]/sigma2_A) * mu_A[2]
beta1_LS2_A <- rho_A * (sigma1_A[2]/sigma2_A)
beta0_MA2_A <- mu_A[1] - sign(rho_A) * (sigma1_A[2]/sigma2_A) * mu_A[2]
beta1_MA2_A <- sign(rho_A) * (sigma1_A[2]/sigma2_A)


plot(data_A$X, data_A$Y, col = "white", pch=1, xlab = "x", ylab = "y", cex = 0.6,xlim=c(-1.3,1.3), ylim=c(-1.3,1.3))
mtext(text=bquote(bold("Panel A:"~mu[1]==0~";"~mu[2]==0~";"~sigma[2]==1~";"~rho == 0.2)~";"~c == 0.5), 
      side = 3)
# MALP
#abline(a=beta0_MA1, b=beta1_MA1, lty = "D3", lwd = 3, col = "red")
segments(-1.3, beta0_MA1_A+beta1_MA1_A*-1.3, 1.3, beta0_MA1_A+beta1_MA1_A*1.3, lty = "D3", lwd = 3, col = "red")
#abline(a=beta0_MA1, b=beta1_MA1, lty = 1, lwd = 3, col = "black")
segments(-1.3, beta0_MA2_A+beta1_MA2_A*-1.3, 1.3, beta0_MA2_A+beta1_MA2_A*1.3, lty = 1, lwd = 3, col = "red")

# LSLP
#abline(a=beta0_LS1, b=beta1_LS1, lty = "D3", lwd = 3, col = "blue")
segments(-1.2, beta0_LS1_A+beta1_LS1_A*-1.2, 1.2, beta0_LS1_A+beta1_LS1_A*1.2, lty = "D3", lwd = 3, col = "blue")
#abline(a=beta0_LS2, b=beta1_LS2, lty = 1, lwd = 3, col = "blue")
segments(-1.16, beta0_LS2_A+beta1_LS2_A*-1.16, 1.16, beta0_LS2_A+beta1_LS2_A*1.16, lty = 1, lwd = 3, col = "blue")

# Ref line
abline(h=0, v=0, lty = 2, col = "gray")

# Contour curves
ellipse(mu=mu_A, sigma=VarCov1_A, alpha = 0.5, npoints = 200, col="gray48", 
        lwd = 1.8, lty = "D3") 
ellipse(mu=mu_A, sigma=VarCov2_A, alpha = 0.5, npoints = 200, col="gray48", 
        lwd = 1.8)
legend("topleft", c(expression(paste(h[MA]: sigma[1]==0.5,"  ")), 
                    expression(paste(h[MA]: sigma[1]==1,"  ")),
                    expression(paste(h[LS]: sigma[1]==0.5,"  ")),
                    expression(paste(h[LS]: sigma[1]==1,"  "))),
       lty = c(1,2,1,2), lwd=c(2,2,2,2), col = c("red", "red", "blue", "blue"), cex=0.8)


#-------------------------------------------------------------------------------
# Panel B
#-------------------------------------------------------------------------------
rho_B = 0.5
mu_B = c(0, 0)
sigma1_B = c(1, 0.5)
sigma2_B = 1
Sigma12_1_B = sigma1_B[1] * sigma2_B * rho_B
Sigma12_2_B = sigma1_B[2] * sigma2_B * rho_B

VarCov1_B <- matrix(c(sigma2_B, Sigma12_1_B, Sigma12_1_B, sigma1_B[1]^2), nrow = 2) 
VarCov2_B <- matrix(c(sigma2_B, Sigma12_2_B, Sigma12_2_B, sigma1_B[2]^2), nrow = 2)

data_B <- rmvnorm(n, mu = mu_B, sigma = VarCov1_B)
Y <- data_B[, 1]  
X <- data_B[, -1]
data_B <- data.frame(X,Y)

# sigma1 = 1
beta0_LS1_B <- mu_B[1] - rho_B * (sigma1_B[1]/sigma2_B) * mu_B[2]
beta1_LS1_B <- rho_B * (sigma1_B[1]/sigma2_B)
beta0_MA1_B <- mu_B[1] - sign(rho_B) * (sigma1_B[1]/sigma2_B) * mu_B[2]
beta1_MA1_B <- sign(rho_B) * (sigma1_B[1]/sigma2_B)

# sigma1 = 0.5
beta0_LS2_B <- mu_B[1] - rho_B * (sigma1_B[2]/sigma2_B) * mu_B[2]
beta1_LS2_B <- rho_B * (sigma1_B[2]/sigma2_B)
beta0_MA2_B <- mu_B[1] - sign(rho_B) * (sigma1_B[2]/sigma2_B) * mu_B[2]
beta1_MA2_B <- sign(rho_B) * (sigma1_B[2]/sigma2_B)


plot(data_B$X, data_B$Y, col = "white", pch=1, xlab = "x", ylab = "y", cex = 0.6,xlim=c(-1.3,1.3), ylim=c(-1.3,1.3))
mtext(text=bquote(bold("Panel B:"~mu[1]==0~";"~mu[2]==0~";"~sigma[2]==1~";"~rho == 0.5)~";"~c == 0.5), 
      side = 3)
# MALP
segments(-1.3, beta0_MA1_B+beta1_MA1_B*-1.3, 1.3, beta0_MA1_B+beta1_MA1_B*1.3, lty = 6, lwd = 3, col = "red")
segments(-1.2, beta0_MA2_B+beta1_MA2_B*-1.2, 1.2, beta0_MA2_B+beta1_MA2_B*1.2, lty = 1, lwd = 3, col = "red")

# LSLP
segments(-1.2, beta0_LS1_B+beta1_LS1_B*-1.2, 1.2, beta0_LS1_B+beta1_LS1_B*1.2, lty = 6, lwd = 3, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3))
segments(-1.17, beta0_LS2_B+beta1_LS2_B*-1.17, 1.17, beta0_LS2_B+beta1_LS2_B*1.17, lty = 1, lwd = 3, col = "blue")

# Ref line
abline(h=0, v=0, lty = 2, col = "gray")

# Contour curves
ellipse(mu=mu_B, sigma=VarCov1_B, alpha = 0.5, npoints = 200, col="gray48", 
        lwd = 1.8, lty = 6) 
ellipse(mu=mu_B, sigma=VarCov2_B, alpha = 0.5, npoints = 200, col="gray48", 
        lwd = 1.8)
legend("topleft", c(expression(paste(h[MA]: sigma[1]==0.5,"  ")), 
                    expression(paste(h[MA]: sigma[1]==1,"  ")),
                    expression(paste(h[LS]: sigma[1]==0.5,"  ")),
                    expression(paste(h[LS]: sigma[1]==1,"  "))),
       lty = c(1,6,1,6), lwd=c(2,2,2,2), col = c("red", "red", "blue", "blue"), cex=0.8)


#-------------------------------------------------------------------------------
# Panel C
#-------------------------------------------------------------------------------
rho_C = 0.8
mu_C = c(0, 0)
sigma1_C = c(1, 0.5)
sigma2_C = 1
Sigma12_1_C = sigma1_C[1] * sigma2_C * rho_C
Sigma12_2_C = sigma1_C[2] * sigma2_C * rho_C

VarCov1_C <- matrix(c(sigma2_C, Sigma12_1_C, Sigma12_1_C, sigma1_C[1]^2), nrow = 2) 
VarCov2_C <- matrix(c(sigma2_C, Sigma12_2_C, Sigma12_2_C, sigma1_C[2]^2), nrow = 2)

data_C <- rmvnorm(n, mu = mu_C, sigma = VarCov1_C)
Y <- data_C[, 1]  
X <- data_C[, -1]
data_C <- data.frame(X,Y)

# sigma1 = 1
beta0_LS1_C <- mu_C[1] - rho_C * (sigma1_C[1]/sigma2_C) * mu_C[2]
beta1_LS1_C <- rho_C * (sigma1_C[1]/sigma2_C)
beta0_MA1_C <- mu_C[1] - sign(rho_C) * (sigma1_C[1]/sigma2_C) * mu_C[2]
beta1_MA1_C <- sign(rho_C) * (sigma1_C[1]/sigma2_C)

# sigma1 = 0.5
beta0_LS2_C <- mu_C[1] - rho_C * (sigma1_C[2]/sigma2_C) * mu_C[2]
beta1_LS2_C <- rho_C * (sigma1_C[2]/sigma2_C)
beta0_MA2_C <- mu_C[1] - sign(rho_C) * (sigma1_C[2]/sigma2_C) * mu_C[2]
beta1_MA2_C <- sign(rho_C) * (sigma1_C[2]/sigma2_C)


plot(data_C$X, data_C$Y, col = "white", pch=1, xlab = "x", ylab = "y", cex = 0.6,xlim=c(-1.3,1.3), ylim=c(-1.3,1.3))
mtext(text=bquote(bold("Panel C:"~mu[1]==0~";"~mu[2]==0~";"~sigma[2]==1~";"~rho == 0.8)~";"~c == 0.5), 
      side = 3)
# MALP
segments(-1.3, beta0_MA1_C+beta1_MA1_C*-1.3, 1.3, beta0_MA1_C+beta1_MA1_C*1.3, lty = 3, lwd = 3, col = "red")
segments(-1.2, beta0_MA2_C+beta1_MA2_C*-1.2, 1.2, beta0_MA2_C+beta1_MA2_C*1.2, lty = 1, lwd = 3, col = "red")

# LSLP
segments(-1.2, beta0_LS1_C+beta1_LS1_C*-1.2, 1.2, beta0_LS1_C+beta1_LS1_C*1.2, lty = 3, lwd = 3, col = "blue")
segments(-1.17, beta0_LS2_C+beta1_LS2_C*-1.17, 1.17, beta0_LS2_C+beta1_LS2_C*1.17, lty = 1, lwd = 3, col = "blue")

# Ref line
abline(h=0, v=0, lty = 2, col = "gray")

# Contour curves
ellipse(mu=mu_C, sigma=VarCov1_C, alpha = 0.5, npoints = 200, col="gray48", 
        lwd = 1.8, lty = 3) 
ellipse(mu=mu_C, sigma=VarCov2_C, alpha = 0.5, npoints = 200, col="gray48", 
        lwd = 1.8)
legend("topleft", c(expression(paste(h[MA]: sigma[1]==0.5,"  ")), 
                    expression(paste(h[MA]: sigma[1]==1,"  ")),
                    expression(paste(h[LS]: sigma[1]==0.5,"  ")),
                    expression(paste(h[LS]: sigma[1]==1,"  "))),
       lty = c(1,3,1,3), lwd=c(2,2,2,2), col = c("red", "red", "blue", "blue"), cex=0.8)


#-------------------------------------------------------------------------------
# Panel D
#-------------------------------------------------------------------------------
rho_D = c(0.2, 0.5, 0.8)
mu_D = c(0, 0)
sigma1_D = 1
sigma2_D = 1
Sigma12_1_D1 = sigma1_D * sigma2_D * rho_D[1]
Sigma12_2_D1 = sigma1_D * sigma2_D * rho_D[1]

Sigma12_1_D2 = sigma1_D * sigma2_D * rho_D[2]
Sigma12_2_D2 = sigma1_D * sigma2_D * rho_D[2]

Sigma12_1_D3 = sigma1_D * sigma2_D * rho_D[3]
Sigma12_2_D3 = sigma1_D * sigma2_D * rho_D[3]

VarCov1_D <- matrix(c(sigma2_D, Sigma12_1_D1, Sigma12_1_D1, sigma1_D^2), nrow = 2) 
VarCov2_D <- matrix(c(sigma2_D, Sigma12_1_D2, Sigma12_1_D2, sigma1_D^2), nrow = 2) 
VarCov3_D <- matrix(c(sigma2_D, Sigma12_1_D3, Sigma12_1_D3, sigma1_D^2), nrow = 2) 


data_D <- rmvnorm(n, mu = mu_D, sigma = VarCov1_D)
Y <- data_D[, 1]  
X <- data_D[, -1]
data_D <- data.frame(X,Y)

# rho = 0.2
beta0_LS_D1 <- mu_D[1] - rho_D[1] * (sigma1_D/sigma2_D) * mu_D[2]
beta1_LS_D1 <- rho_D[1] * (sigma1_D/sigma2_D)
beta0_MA_D1 <- mu_D[1] - sign(rho_D[1]) * (sigma1_D/sigma2_D) * mu_D[2]
beta1_MA_D1 <- sign(rho_D[1]) * (sigma1_D/sigma2_D)

# rho = 0.5
beta0_LS_D2 <- mu_D[1] - rho_D[2] * (sigma1_D/sigma2_D) * mu_D[2]
beta1_LS_D2 <- rho_D[2] * (sigma1_D/sigma2_D)
beta0_MA_D2 <- mu_D[1] - sign(rho_D[2]) * (sigma1_D/sigma2_D) * mu_D[2]
beta1_MA_D2 <- sign(rho_D[2]) * (sigma1_D/sigma2_D)

# rho = 0.8
beta0_LS_D3 <- mu_D[1] - rho_D[3] * (sigma1_D/sigma2_D) * mu_D[2]
beta1_LS_D3 <- rho_D[3] * (sigma1_D/sigma2_D)
beta0_MA_D3 <- mu_D[1] - sign(rho_D[3]) * (sigma1_D/sigma2_D) * mu_D[2]
beta1_MA_D3 <- sign(rho_D[3]) * (sigma1_D/sigma2_D)


plot(data_D$X, data_D$Y, col = "white", pch=1, xlab = "x", ylab = "y", cex = 0.6,xlim=c(-1.3,1.3), ylim=c(-1.3,1.3))
mtext(text=bquote(bold("Panel D:"~mu[1]==0~";"~mu[2]==0~";"~sigma[1]==1~","~sigma[2]==1)~";"~c == 0.5), 
      side = 3)
# MALP
segments(-1.3, beta0_MA_D1+beta1_MA_D1*-1.3, 1.3, beta0_MA_D1+beta1_MA_D1*1.3, lty = 1, lwd = 3, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
segments(-1.3, beta0_MA_D2+beta1_MA_D2*-1.3, 1.3, beta0_MA_D2+beta1_MA_D2*1.3, lty = 6, lwd = 3, col = "red")
segments(-1.3, beta0_MA_D3+beta1_MA_D3*-1.3, 1.3, beta0_MA_D3+beta1_MA_D3*1.3, lty = 3, lwd = 3, col = "red")

#LSLP
segments(-1.2, beta0_LS_D1+beta1_LS_D1*-1.2, 1.2, beta0_LS_D1+beta1_LS_D1*1.2, lty = "D3", lwd = 3, col = "blue")
segments(-1.2, beta0_LS_D2+beta1_LS_D2*-1.2, 1.2, beta0_LS_D2+beta1_LS_D2*1.2, lty = 6, lwd = 3, col = "blue")
segments(-1.2, beta0_LS_D3+beta1_LS_D3*-1.2, 1.2, beta0_LS_D3+beta1_LS_D3*1.2, lty = 3, lwd = 3, col = "blue")


abline(h=0, v=0, lty = 2, col = "gray")
# Contour curves
ellipse(mu=mu_D, sigma=VarCov1_D, alpha = 0.5, npoints = 200, col="gray48", 
        lwd = 1.8, lty = "D3") 
ellipse(mu=mu_D, sigma=VarCov2_D, alpha = 0.5, npoints = 200, col="gray48", 
        lwd = 1.8, lty = 6) 
ellipse(mu=mu_D, sigma=VarCov3_D, alpha = 0.5, npoints = 200, col="gray48", 
        lwd = 1.8, lty = 3) 

legend("topleft", c(expression(paste(h[MA]: rho,": all")), 
                    expression(paste(h[LS]: rho==0.2,"  ")),
                    expression(paste(h[LS]: rho==0.5,"  ")),
                    expression(paste(h[LS]: rho==0.8,"  "))),
       lty = c(1,2,6,3), lwd=c(2,2,2,2), col = c("red", "blue", "blue", "blue"), cex=0.8)