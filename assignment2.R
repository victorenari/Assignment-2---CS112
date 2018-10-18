rm(list = ls())

#question #1 ----

#creating the variables and the error
set.seed(1)
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
library(arm)
set.seed(1)
data("lalonde")

#separating only the control group
control <- lalonde[!(lalonde$treat == 1),]

#creating model
control.lm <- lm(re78 ~ age + educ + re74 + re75 + educ*re74 + educ*re75 + age*re74 + age*re75 + re74*re75, data = control)
summary(control.lm)

#simulating 10000 times
control.sim <- sim(control.lm, n.sim = 10000)
sim.coef <- control.sim@coef

#calculating the median of the predictors
median_educ <- median(lalonde$educ)
median_re74 <- median(lalonde$re74)
median_re75 <- median(lalonde$re75)

#calculating the confidence intervals for re78 using the median of the predictors, for every unit of age
storagedf_1 <- matrix(NA, nrow = 10000, ncol = 39)
for(age in c(17:55)) {
  for(i in 1:10000) 
  {
    re78.median <- sum(control.sim@coef[i,]*(c(1, age, median_educ, median_re74, median_re75, median_educ*median_re74, median_educ*median_re75, age*median_re74, age*median_re75, median_re74*median_re75)), control.sim@sigma[i])
    storagedf_1[i, age - 16] <- re78.median
  }
}
conf.intervals <- apply(storagedf_1, 2, quantile, probs = c(0.025, 0.975))

#plotting 1st graph
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(0,22000), xlab = "Age", ylab = "re78", main = "re78 95% prediction intervals holding predictors at their medians")
counter = 1
for(age in 17:55) {
  segments(x0 = age,  y0 = conf.intervals[counter], x1 = age, y1 = conf.intervals[counter + 1])
  counter = counter + 2
}

#calculating the quantile of the predictors
quantile_educ <- quantile(lalonde$educ, probs = c(0.9))
quantile_re74 <- quantile(lalonde$re74, probs = c(0.9))
quantile_re75 <- quantile(lalonde$re75, probs = c(0.9))

#calculating the confidence intervals for re78 using the 90% quantile of the predictors, for every unit of age
storagedf_2 <- matrix(NA, nrow = 10000, ncol = 39)
for(age in c(17:55)) {
  for(i in 1:10000) 
  {
    re78.quantile <- sum(control.sim@coef[i,]*(c(1, age, quantile_educ, quantile_re74, quantile_re75, quantile_educ*quantile_re74, quantile_educ*quantile_re75, age*quantile_re74, age*quantile_re75, quantile_re74*quantile_re75)), control.sim@sigma[i])
    storagedf_2[i, age - 16] <- re78.quantile
  }
}
conf.intervals2 <- apply(storagedf_2, 2, quantile, probs = c(0.025, 0.975))

#plotting 2nd graph
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-3000,22000), xlab = "Age", ylab = "re78", main = "re78 95% prediction intervals holding predictors at their 90% quantiles")
counter = 1
for(age in 17:55) {
  segments(x0 = age,  y0 = conf.intervals2[counter], x1 = age, y1 = conf.intervals2[counter + 1])
  counter = counter + 2
}

#creating table 1
storage1.median <- 0
for (i in 1:39) {
  storage1.median[i] <- median(storagedf_1[i])
}
df1 <- data.frame( "Age" = 17:55, "2.5%" = conf.intervals[1,], "Median" = storage1.median, "97.5%" = conf.intervals[2,])

#creating table 2
storage2.median <- 0
for (i in 1:39) {
  storage2.median[i] <- median(storagedf_2[i])
}
df2 <- data.frame( "Age" = 17:55, "2.5%" = conf.intervals2[1,], "Median" = storage2.median, "97.5%" = conf.intervals2[2,])


#question #3 ----
#question $4 ----
rsquared <- function(observed, predicted) {
  ess <- sum((predicted - observed)^2)
  tss <- sum((observed - mean(observed))^2)
  rsqrd <- 1-ess/tss
  return(rsqrd)
}

y <- c(2,2,4)
y2 <- c(1, 2, 3)
test <- rsquared(y, y2)
test
