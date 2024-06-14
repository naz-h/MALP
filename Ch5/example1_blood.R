#-------------------------------------------------------------------------------
# Example 1
#-------------------------------------------------------------------------------
# Data file:
# These data represent glycosylated hemoglobin (hba1c) readings reported in DCCT
# percentages and random blood glucose (rbg) readings reported in mmol/l.  The
# readings are derived from 349 diabetic patients attending a hospital out-
# patient department at the Karl Bremer District Hospital in Western Cape, South 
# Africa.
# The original data were published as a scatter plot in a Masters thesis (p.12):
# Daramola O.F. (2012).  Assessing the validity of random blood glucose testing
# for monitoring glycemic control and predicting HbA1c values in type 2 
# diabetics at Karl Bremer hospital. Masters Thesis [Family Medicine and Primary Care]. 
# Stellenbosch University: Stellenbosch, South Africa.http://scholar.sun.ac.za/handle/10019.1/80458
glucose <- read.table("bloodglucose.txt",sep="\t", header=T)
#glucose <- read.table("https://lib.stat.cmu.edu/datasets/hba1c_bloodGlucose.dat",sep="\t", header=T, skip=30)
head(glucose)
summary(glucose)


CCC <- function(x,y){
  2*cov(x,y)/(var(x)+var(y)+(mean(x)-mean(y))^2)
}

PCC <- function(x,y){
  cor(x,y)
}

MSE <- function(x,y){
  mean((x-y)^2)
}

# Scatter plot of raw data
par(mfrow=c(1,2))
plot(glucose$rbg, glucose$hba1c, xlim = c(2,40), ylim=c(2,40), 
     cex=0.6, xlab="RBG", ylab="HbA1c", main="Panel A: Scatter plot of original data")
abline(a=0,b=1, lty=2)


# Scatter plot of transformed data
rgb2 <- glucose$rbg-1.4
rgb3 <- 0.45*glucose$rbg +3.5


round(rbind(PCC(glucose$rbg, glucose$hba1c), # PCC of x and y
      CCC(glucose$rbg, glucose$hba1c), # CCC of x and y
      CCC(rgb2, glucose$hba1c), # CCC of x2 and y
      CCC(rgb3, glucose$hba1c)), 3) # CCC of x3 and y



plot(rgb2, glucose$hba1c, xlim = c(2,40), ylim=c(2,40), pch=1,
     cex=0.6, xlab="Transformed RBG", ylab="HbA1c", main="Panel B: Scatter plot of transformed data")
abline(a=0,b=1, lty=2)
points(rgb3, glucose$hba1c,pch=3,cex=0.6)
legend("topleft", c("Y = X-1.4", "Y = 0.45X+3.5"), pch = c(1,3), cex=0.8)


# Check agreement between x and y
rho <- cor(glucose$hba1c, glucose$rbg)
ccc <- c(CCC(glucose$hba1c, glucose$rbg),
         CCC(glucose$hba1c, rgb2),
         CCC(glucose$hba1c, rgb3))
res <- cbind(PCC=rho, CCC=ccc)
rownames(res) <- c("Raw", "Transformation 1" , "Transformation 2")
knitr::kable(res)


# MALP and LSLP coefficient estimate
fit_LS <- lm(hba1c ~ rbg, data=glucose)
beta_LS <- fit_LS$coefficients
Yhat_LS <- fit_LS$fitted.values
gamma <- cor(glucose$hba1c, Yhat_LS)
muY <- mean(glucose$hba1c)
beta_MA <- c((1-1/gamma)*muY+beta_LS[1]/gamma, beta_LS[-1]/gamma)
names(beta_MA) <- names(beta_LS)
beta_LS
beta_MA

par(mfrow=c(1,1))
plot(glucose$rbg, glucose$hba1c,
     cex=0.6, xlab="RBG", ylab="HbA1c", main="MALP vs LSLP")
abline(a=beta_MA[1], b=beta_MA[2], col="red")
abline(a=beta_LS[1], b=beta_LS[2], col="blue", lty=2)
legend("topleft", c("MALP", "LSLP"), lty=c(1,2), col=c("red","blue"), cex=0.8)


# prediction performance
pred_eval <- function(XY, B) {
  n <- dim(XY)[1]
  XY <- XY[sample(n, replace=TRUE),] # shuffle the rows of dataset
  # split data
  train_dat <-  XY[1:(n %/% 2),] 
  test_dat <- XY[-(1:((n %/% 2)+1)),]
  RES <- matrix(NA, ncol = 6, nrow = B)
  m <- dim(train_dat)[1]
  training <- replicate(B, matrix(NA, nrow = m, ncol = 2), simplify = FALSE)
  testing <- replicate(B, matrix(NA, nrow = m, ncol = 2), simplify = FALSE)
  
  # replicate B times of training and testing data
  for(i in 1:B){
    training[[i]] <- train_dat
    testing[[i]] <- test_dat
  }
  # realistic case
  for (i in 1:B) {
    EPRED <- EST_PRED_VEC(training[[i]], testing[[i]][,1])
    y_MA <- EPRED[,1]
    y_LS <- EPRED[,2]
    RES[i,] <- c(PCC(testing[[i]][,2], y_MA),
                 PCC(testing[[i]][,2], y_LS),
                 CCC(testing[[i]][,2], y_MA),
                 CCC(testing[[i]][,2], y_LS),
                 MSE(testing[[i]][,2], y_MA),
                 MSE(testing[[i]][,2], y_LS))}
  colnames(RES) <- c("MALP","LSLP","MALP","LSLP","MALP","LSLP")
  return(RES)
}

DAT <- cbind(glucose$rbg,glucose$hba1c)
set.seed(123)
v <- pred_ev(DAT, B=2000)
round(colMeans(v), 3) # 0.745 0.745 0.745 0.696 5.871 5.167 

# illustrative case
Yhat_MA <- beta_MA[1] + beta_MA[2] * glucose$rbg
pcc <- c(PCC(glucose$hba1c, Yhat_MA),
         PCC(glucose$hba1c, Yhat_LS))
ccc <- c(CCC(glucose$hba1c, Yhat_MA),
         CCC(glucose$hba1c, Yhat_LS))
mse <- c(MSE(glucose$hba1c, Yhat_MA),
         MSE(glucose$hba1c, Yhat_LS))
res <- cbind(PCC=pcc, CCC=ccc, MSE=mse)
rownames(res) <- c("MA", "LS" )
round(res, 3)
