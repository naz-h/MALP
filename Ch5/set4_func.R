#-------------------------------------------------------------------------------
# Set 4 functions
#-------------------------------- Packages -------------------------------------
library(mvtnorm)
library(MASS)
library(mnormt)
library(clusterGeneration)
library(matrixStats)
library(mnonr)
#---------------------------- Basic functions-----------------------------------
CCC <- function(x,y){
  2*cov(x,y)/(var(x)+var(y)+(mean(x)-mean(y))^2)
}

PCC <- function(x,y){
  cor(x,y)
}

MSE <- function(x,y){
  mean((x-y)^2)
}


set4_gen_data_p <- function(n, beta, para, B, p = p) {
  x_values <- replicate(B, matrix(NA,nrow=n,ncol=p), simplify = FALSE)
  y_values <- replicate(B, matrix(NA,nrow=n,ncol=1), simplify = FALSE)
  XY <- replicate(B, matrix(NA, nrow = n, ncol = p + 1), simplify = FALSE)
  for (i in 1:B) {
    # Generate e_x and e_y
    if(p == 1){
      ex <- (runif(n, min = 0, max = 1) - 0.5) * sqrt(12)
      ey <- (runif(n, min = 0, max = 1) - 0.5) * sqrt(12)
      x <- para[1] * ex
      y <- beta[1] + beta[2] * x + para[2] * ey
    } else {
      ex <- matrix((runif(n * p, min = 0, max = 1) - 0.5) * sqrt(12), ncol = p)
      ey <- (runif(n, min = 0, max = 1) - 0.5) * sqrt(12)
      x <- para[1] * ex
      y <- beta[1] + x%*%beta[-1] + para[2]*ey 
    }
    # Generate x and y
    x_values[[i]] <- x
    y_values[[i]] <- y
    XY[[i]] <- data.frame(x=x_values[[i]],y=y_values[[i]])
  }
  return(XY)
}


set4_test <- function(dat, B){
  beta.LS <- matrix(NA, nrow = B, ncol = 2)
  beta_MA <- matrix(NA, nrow = B, ncol = 2)
  gamma <- numeric(B); ccc.LS <- numeric(B); mse.LS <- numeric(B)
  ccc.MA <- numeric(B); pcc.MA <- numeric(B); mse.MA <- numeric(B)
  muY <- numeric(B)
  cor_samp <- numeric(B); ccc_samp <- numeric(B)
  for (i in 1:B) {
    # compute LSLP
    fit_LS <- lm(dat[[i]][,2] ~ dat[[i]][,1]) 
    beta.LS[i, ] <- coef(fit_LS) 
    Yhat_LS <- fitted(fit_LS) 
    gamma[i] <- cor(Yhat_LS, dat[[i]][,2]) 
    ccc.LS[i] <- CCC(Yhat_LS, dat[[i]][,2])
    mse.LS[i] <- MSE(Yhat_LS, dat[[i]][,2])
    # compute MALP
    muY[i] <- mean(dat[[i]][,2]) 
    beta_MA[i,] <- c((1 - 1 / gamma[i]) * muY[i] + beta.LS[i,1] / gamma[i], beta.LS[i,-1] / gamma[i])
    Yhat_MA <- beta_MA[i,1] + beta_MA[i,2] * dat[[i]][,1]
    pcc.MA[i] <- cor(Yhat_MA, dat[[i]][,2]) 
    ccc.MA[i] <- CCC(Yhat_MA, dat[[i]][,2])
    mse.MA[i] <- MSE(Yhat_MA, dat[[i]][,2])
    # data evaluation
    cor_samp[i] <- PCC(dat[[i]][,1], dat[[i]][,2])
    ccc_samp[i] <- CCC(dat[[i]][,1], dat[[i]][,2])
  }
  corXY <- mean(cor_samp); cccXY <- mean(ccc_samp)
  E_beta.MA <- colMeans(beta_MA); V_beta.MA <- apply(beta_MA, 2, var)
  E_beta.LS <- colMeans(beta.LS); V_beta.LS <- apply(beta.LS, 2, var)
  E_cor.LS <- mean(gamma); E_ccc.LS <- mean(ccc.LS); V_ccc_LS <- var(ccc.LS); E.mse.LS <- mean(mse.LS)
  E_cor.MA <- mean(pcc.MA); E_ccc.MA <- mean(ccc.MA); V_ccc_MA <- var(ccc.MA); E.mse.MA <- mean(mse.MA)
  
  
  # report
  diff <- beta_MA-beta.LS
  abs_diff <- abs(beta_MA-beta.LS)
  euc_dist <-sqrt((beta_MA - beta.LS)^2)
  
  mean_diff <- apply(diff, 2, mean)
  var_diff <- apply(diff, 2, var)
  abs_mean_diff <-apply(abs_diff, 2, mean)
  abs_var_diff <-apply(abs_diff, 2, var)
  mean_euc_dist <- apply(euc_dist, 2, mean)
  var_euc_dist <- apply(euc_dist, 2, var)
  m.d <- cbind(mean_diff, var_diff)
  a.m.d <- cbind(abs_mean_diff, abs_var_diff)
  e.d <- cbind(mean_euc_dist, var_euc_dist)
  
  return(list(XY_eval=cbind(E_PCC_XY=corXY, E_CCC_XY=cccXY), 
              
              LS_eval=cbind(E_coef_LS=E_beta.LS, V_coef_LS=V_beta.LS,
                            E_PCC_LS=E_cor.LS, E_CCC_LS=E_ccc.LS,
                            V_CCC_LS=V_ccc_LS, E_MSE_LS=E.mse.LS),
              
              MA_eval=cbind(E_coef_MA=E_beta.MA, V_coef_MA = V_beta.MA,
                            E_PCC_MA=E_cor.MA, E_CCC_MA=E_ccc.MA, 
                            V_CCC_MA=V_ccc_MA, E_MSE_MA=E.mse.MA),
              
              est_coefs = data.frame(b0_MA=beta_MA[,1], b1_MA=beta_MA[,2], 
                                     b0_LS=beta.LS[,1], b1_LS=beta.LS[,2]),
              comp_coef = round(rbind(t(m.d), t(a.m.d), t(e.d)),3)))
}


set4_test_p <- function(dat, B, p=2){
  beta.LS <- matrix(NA, nrow = B, ncol = p+1)
  beta_MA <- matrix(NA, nrow = B, ncol = p+1)
  gamma <- numeric(B); ccc.LS <- numeric(B); mse.LS <- numeric(B)
  ccc.MA <- numeric(B); pcc.MA <- numeric(B); mse.MA <- numeric(B)
  muY <- numeric(B)
  cor_samp <- numeric(B); ccc_samp <- numeric(B)
  for (i in 1:B) {
    # compute LSLP
    fit_LS <- lm(dat[[i]][,p+1] ~ dat[[i]][,1] + dat[[i]][,p]) 
    beta.LS[i, ] <- coef(fit_LS) 
    Yhat_LS <- fitted(fit_LS) 
    gamma[i] <- cor(Yhat_LS, dat[[i]][,p+1]) 
    ccc.LS[i] <- CCC(Yhat_LS, dat[[i]][,p+1])
    mse.LS[i] <- MSE(Yhat_LS, dat[[i]][,p+1])
    # compute MALP
    muY[i] <- mean(dat[[i]][,p+1]) 
    beta_MA[i,] <- c((1 - 1 / gamma[i])*muY[i] + beta.LS[i,1]/gamma[i], 
                     beta.LS[i,2] / gamma[i], beta.LS[i,p+1] / gamma[i])
    Yhat_MA <- beta_MA[i,1] + beta_MA[i,2] * dat[[i]][,1] + beta_MA[i,3] * dat[[i]][,p]
    pcc.MA[i] <- cor(Yhat_MA, dat[[i]][,p+1]) 
    ccc.MA[i] <- CCC(Yhat_MA, dat[[i]][,p+1])
    mse.MA[i] <- MSE(Yhat_MA, dat[[i]][,p+1])
    # data evaluation
    cor_samp[i] <- PCC(dat[[i]][,1], dat[[i]][,p+1])
    ccc_samp[i] <- CCC(dat[[i]][,1], dat[[i]][,p+1])
  }
  corXY <- mean(cor_samp); cccXY <- mean(ccc_samp)
  E_beta.MA <- colMeans(beta_MA); V_beta.MA <- apply(beta_MA, 2, var)
  E_beta.LS <- colMeans(beta.LS); V_beta.LS <- apply(beta.LS, 2, var)
  E_cor.LS <- mean(gamma); E_ccc.LS <- mean(ccc.LS); V_ccc_LS <- var(ccc.LS); E.mse.LS <- mean(mse.LS)
  E_cor.MA <- mean(pcc.MA); E_ccc.MA <- mean(ccc.MA); V_ccc_MA <- var(ccc.MA); E.mse.MA <- mean(mse.MA)
  
  
  # report
  diff <- beta_MA-beta.LS
  abs_diff <- abs(beta_MA-beta.LS)
  euc_dist <-sqrt((beta_MA - beta.LS)^2)
  
  mean_diff <- apply(diff, 2, mean)
  var_diff <- apply(diff, 2, var)
  abs_mean_diff <-apply(abs_diff, 2, mean)
  abs_var_diff <-apply(abs_diff, 2, var)
  mean_euc_dist <- apply(euc_dist, 2, mean)
  var_euc_dist <- apply(euc_dist, 2, var)
  m.d <- cbind(mean_diff, var_diff)
  a.m.d <- cbind(abs_mean_diff, abs_var_diff)
  e.d <- cbind(mean_euc_dist, var_euc_dist)
  
  return(list(XY_eval=cbind(E_PCC_XY=corXY, E_CCC_XY=cccXY), 
              
              LS_eval=cbind(E_coef_LS=E_beta.LS, V_coef_LS=V_beta.LS,
                            E_PCC_LS=E_cor.LS, E_CCC_LS=E_ccc.LS,
                            V_CCC_LS=V_ccc_LS, E_MSE_LS=E.mse.LS),
              
              MA_eval=cbind(E_coef_MA=E_beta.MA, V_coef_MA = V_beta.MA,
                            E_PCC_MA=E_cor.MA, E_CCC_MA=E_ccc.MA, 
                            V_CCC_MA=V_ccc_MA, E_MSE_MA=E.mse.MA),
              
              est_coefs = data.frame(b0_MA=beta_MA[,1], b1_MA=beta_MA[,2],b2_MA=beta_MA[,3], 
                                     b0_LS=beta.LS[,1], b1_LS=beta.LS[,2],b2_LS=beta_MA[,3]),
              comp_coef = round(rbind(t(m.d), t(a.m.d), t(e.d)),3)))
}
