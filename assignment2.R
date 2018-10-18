rm(list = ls())
set.seed(1)
#question #1 ----

#creating the variables and the error
x <- rnorm(99, 0, 20)
error <- rnorm(99, 0, 5)
y <- 0.5 * x  + error

#data frame with 99 observations
df99 <- data.frame(x, y)

#model for 99 observations
model99 <- lm(y ~ x, data = df99)
summary(model99)


#creating outlier and a data frame with 100 observations
outlier <- c(-250, 100)
df100 <- rbind(df99, outlier)

#model for 100 observations
model100 <- lm(y ~ x, data = df100)
summary(model100)

#creating scatterplot
plot(df100$x, df100$y, xlab = "X", ylab = "Y", main = "Effect of an outlier in a regression")
abline(model99, col = "blue")
abline(model100, col = "red")

#question #2 ----
library(Matching)
library(foreign)

data("lalonde")

#separating only the control group
control <- lalonde[!(lalonde$treat == 1),]

#creating model
control.lm <- lm(re78 ~ age + educ + re74 + re75 + educ*re74 + educ*re75 + age*re74 + age*re75 + re74*re75, data = control)
summary(control.lm)

library(arm)
control.sim <- sim(control.lm, n.sim = 10000)
sim.coef <- control.sim@coef

median_educ <- median(lalonde$educ)
median_re74 <- median(lalonde$re74)
median_re75 <- median(lalonde$re75)

storagedf_1 <- matrix(NA, nrow = 10000, ncol = 39)

for(age in c(17:55)) {
  for(i in 1:10000) 
  {
    beta <- sum(control.sim@coef[i,]*(c(1, age, median_educ, median_re74, median_re75, median_educ*median_re74, median_educ*median_re75, age*median_re74, age*median_re75, median_re74*median_re75)), control.sim@sigma[i])
    storagedf_1[i, age - 16] <- beta
  }
}
conf.intervals <- apply(storagedf_1, 2, quantile, probs = c(0.005, 0.995))

plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(0,20000), xlab = "Age", ylab = "re78")

counter = 1
for(age in 17:55) {
  segments(x0 = age,  y0 = conf.intervals[counter], x1 = age, y1 = conf.intervals[counter + 1])
  counter = counter + 2
}


quantile_educ <- quantile(lalonde$educ, probs = c(0.9))
quantile_re74 <- quantile(lalonde$re74, probs = c(0.9))
quantile_re75 <- quantile(lalonde$re75, probs = c(0.9))

storagedf_2 <- matrix(NA, nrow = 10000, ncol = 39)

for(age in c(17:55)) {
  for(i in 1:10000) 
  {
    beta <- sum(control.sim@coef[i,]*(c(1, age, quantile_educ, quantile_re74, quantile_re75, quantile_educ*quantile_re74, quantile_educ*quantile_re75, age*quantile_re74, age*quantile_re75, quantile_re74*quantile_re75)))
    storagedf_2[i, age - 16] <- beta
  }
}
conf.intervals2 <- apply(storagedf_2, 2, quantile, probs = c(0.005, 0.995))

plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-3000,15000))

counter = 1
for(age in 17:55) {
  segments(x0 = age,  y0 = conf.intervals2[counter], x1 = age, y1 = conf.intervals2[counter + 1])
  counter = counter + 2
}




