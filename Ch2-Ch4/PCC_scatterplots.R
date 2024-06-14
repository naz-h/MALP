library(mvtnorm)

#############################################
# Figure 2.2
#############################################


# (I) linear positive correlation
x = rnorm(300, mean = 100, sd = 20)
y = x + rnorm(300, mean = 50, sd = 10)
postive_data <- data.frame(x = x, y = y)

plot(postive_data$x, postive_data$y, pch=20)
#abline(lm(postive_data$y ~ postive_data$x, data = postive_data), col = "red", lwd = 2)
text(paste("PCC:", round(cor(postive_data$x, postive_data$y), 3)), x = 70, y = 200)
#write.table(postive_data, file = "gaussian.txt", sep = "\t", row.names = FALSE)



# (II) Gaussian with outliers
x = rnorm(270, mean = 100, sd = 10)
y = x + rnorm(270, mean = 50, sd = 10)
norm_data <- data.frame(x = x, y = y)
# Generate outliers
outliers_x <- rnorm(30, mean = 160, sd = 5)
outliers_y <- outliers_x + rnorm(30, mean = 50, sd = 10)
outliers_data <- data.frame(x = outliers_x, y = outliers_y)
# Combine the original dataset with outliers
combined_data <- rbind(norm_data, outliers_data)

# Plot the combined dataset
plot(combined_data$x, combined_data$y, pch = 20)
#abline(lm(combined_data$y ~ combined_data$x, data = combined_data), col = "red", lwd = 2)
text(paste("PCC:", round(cor(combined_data$x, combined_data$y), 3)), x = 90, y = 215)
#write.table(combined_data, file = "gaussian_outliers.txt", sep = "\t", row.names = FALSE)



# (III) Quadratic-shaped distribution (non-linear)
x <- seq(-4, 4, length.out = 300)
y <- -x^2 + rnorm(length(x))
nonlin_data <- data.frame(x = x, y = y)

plot(nonlin_data$x, nonlin_data$y, pch=20)
#abline(lm(nonlin_data$y ~ nonlin_data$x, data = nonlin_data), col = "red", lwd = 2)
text(paste("PCC:", round(cor(nonlin_data$x, nonlin_data$y), 3)), x = -3, y = 0.5)
#write.table(nonlin_data, file = "quad_data.txt", sep = "\t", row.names = FALSE)



# (IV) circular distribution (no correlation)
set.seed(1)
# Generate random angles
theta <- runif(300, 0, 2 * pi)
# Generate random radius within the specified range
radius <- runif(300, 0, 3)

# Convert polar coordinates to Cartesian coordinates
x <- radius * cos(theta)
y <- radius * sin(theta)
circle_data <- data.frame(x = x, y = y)

# Plot the circular cloud of points
plot(circle_data$x, circle_data$y, pch = 20, ylim = c(-4,4), xlim = c(-4,4))
#abline(lm(circle_data$y ~ circle_data$x), col = "red", lwd = 3, lty=2)
text(paste("PCC:", round(cor(circle_data$x, circle_data$y), 3)), x = -3, y = 3)
#write.table(circle_data, file = "circle_data.txt", sep = "\t", row.names = FALSE)



# donut-shaped distribution
set.seed(123)
# Simulate data in a circular or donut-like pattern
theta <- seq(0, 2 * pi, length.out = 300)
radius <- 0.5 + rnorm(300, sd = 0.04)
# Convert polar coordinates to Cartesian coordinates
x <- radius * cos(theta)
y <- radius * sin(theta)
# Create a data frame
donut_data <- data.frame(x = x, y = y)

plot(donut_data$x, donut_data$y, pch=20)
#abline(lm(donut_data$y ~ donut_data$x, data = donut_data), col = "red", lwd = 2)
text(paste("PCC:", round(cor(donut_data$x, donut_data$y), 2)), x = -0.45, y = 0.5)
#write.table(donut_data, file = "donut_data.txt", sep = "\t", row.names = FALSE)
