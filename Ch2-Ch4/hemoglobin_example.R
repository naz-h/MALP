# Plots for the Hemoglobin example in Section 2.3.

hemoglobin <- read.delim("hemoglobin.txt")

CCC <-function(x,y){
  2*cov(x,y) / (var(x) + var(y) + (mean(x) - mean(y))^2)
}

################################################
# Figure 2.3
################################################
# Original data
plot(hemoglobin$Hb1, hemoglobin$Hb2, pch=16,ylim = c(9,17),xlim = c(9,17),
     main="Orginial data")
abline(lm(Hb2 ~ Hb1, data=hemoglobin), col = "gray", lty=2)
abline(a=0,b=1, col="black", lty=1)
text(paste("PCC:", round(cor(hemoglobin$Hb1, hemoglobin$Hb2), 4)), x = 10, y = 16.5, cex=0.8)
text(paste("CCC:", round(CCC(hemoglobin$Hb1, hemoglobin$Hb2),4)), x = 10, y = 16, cex=0.8)



################################################
# Figure 2.4
################################################
# Location shift
shifted_data <- hemoglobin
shifted_data$Hb1 <- shifted_data$Hb1-2
shifted_data$Hb2 <- shifted_data$Hb2 

plot(shifted_data$Hb1, shifted_data$Hb2, pch = 16, ylim = c(7, 20), xlim = c(7, 20), 
     main = "(a) Location shift")
abline(lm(Hb2 ~ Hb1, data = shifted_data), col = "gray", lty = 2)
abline(a = 0, b = 1, col = "black", lty = 1)
text(paste("PCC:", round(cor(shifted_data$Hb1, shifted_data$Hb2), 4)), x = 8.5, y = 19, cex=0.8)
text(paste("CCC:", round(CCC(shifted_data$Hb1, shifted_data$Hb2),4)), x = 8.5, y = 18, cex=0.8)
#write.table(shifted_data, file = "Hb_location.txt", sep = "\t", row.names = FALSE)


# location and scale shift
shifted_data <- hemoglobin
shifted_data$Hb1 <- shifted_data$Hb1 - 2 
shifted_data$Hb2 <- shifted_data$Hb2 * 2-12 

plot(shifted_data$Hb1, shifted_data$Hb2, pch = 16, ylim = c(7, 25), xlim = c(7, 25), main = "(b) Location and Scale Shift")
abline(lm(Hb2 ~ Hb1, data = shifted_data), col = "gray", lty = 2)
abline(a = 0, b = 1, col = "black", lty = 1)
text(paste("PCC:", round(cor(shifted_data$Hb1, shifted_data$Hb2), 4)), x = 9, y = 24, cex=0.8)
text(paste("CCC:", round(CCC(shifted_data$Hb1, shifted_data$Hb2), 4)), x = 9, y = 23, cex=0.8)
#write.table(shifted_data, file = "Hb_location_scale.txt", sep = "\t", row.names = FALSE)


# Scale shift 
shifted_data <- hemoglobin
shifted_data$Hb1 <- shifted_data$Hb1
shifted_data$Hb2 <- shifted_data$Hb2-1

plot(shifted_data$Hb1, shifted_data$Hb2, pch = 16, ylim = c(9, 18), xlim = c(9, 18), 
     main = "(c) Scale Shift")
abline(lm(Hb2 ~ Hb1, data = shifted_data), col = "gray", lty = 2)
abline(a = 0, b = 1, col = "black", lty = 1)
text(paste("PCC:", round(cor(shifted_data$Hb1, shifted_data$Hb2), 4)), x = 10, y = 17.5, cex=0.8)
text(paste("CCC:", round(CCC(shifted_data$Hb1, shifted_data$Hb2), 4)), x = 10, y = 17, cex=0.8)
#write.table(shifted_data, file = "Hb_scale.txt", sep = "\t", row.names = FALSE)



################################################
# Not included
################################################
# Bland Altman
plot(hemoglobin$avg, hemoglobin$difference, pch = 16, ylim = c(0, 2), xlim = c(10,18), 
     main = "Bland-Altman plot", xlab = "Average of two hemoglobin values (in g/dL)",
     ylab="Difference between two hemoglobin values (in g/dL)")
abline(h=mean(hemoglobin$difference) + 1.96 * sd(hemoglobin$difference), col = "black", lty = 2) # upper
abline(h=mean(hemoglobin$difference) - 1.96 * sd(hemoglobin$difference), col = "black", lty = 2) # lower
abline(h=mean(hemoglobin$difference), col = "black", lty = 1)

BA_data <- data.frame(avg=hemoglobin$avg, diff=hemoglobin$difference)
#write.table(BA_data, file = "Hb_BA.txt", sep = "\t", row.names = FALSE)
