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
    re78.median <- sum(control.sim@coef[i,]*(c(1, age, median_educ, median_re74, median_re75, median_educ*median_re74, median_educ*median_re75, age*median_re74, age*median_re75, median_re74*median_re75)), rnorm(1, 0, control.sim@sigma[i]))
    storagedf_1[i, age - 16] <- re78.median
  }
}
conf.intervals <- apply(storagedf_1, 2, quantile, probs = c(0.025, 0.975))

#plotting 1st graph
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-10000,20000), xlab = "Age", ylab = "re78", main = "re78 95% prediction intervals holding predictors at their medians")
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
    re78.quantile <- sum(control.sim@coef[i,]*(c(1, age, quantile_educ, quantile_re74, quantile_re75, quantile_educ*quantile_re74, quantile_educ*quantile_re75, age*quantile_re74, age*quantile_re75, quantile_re74*quantile_re75)), rnorm(1, 0, control.sim@sigma[i]))
    storagedf_2[i, age - 16] <- re78.quantile
  }
}
conf.intervals2 <- apply(storagedf_2, 2, quantile, probs = c(0.025, 0.975))

#plotting 2nd graph
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-10000,20000), xlab = "Age", ylab = "re78", main = "re78 95% prediction intervals holding predictors at their 90% quantiles")
counter = 1
for(age in 17:55) {
  segments(x0 = age,  y0 = conf.intervals2[counter], x1 = age, y1 = conf.intervals2[counter + 1])
  counter = counter + 2
}

#creating table 1
storage1.median <- 0
for (i in 1:39) {
  storage1.median[i] <- median(storagedf_1[i,])
}
df1 <- data.frame( "Age" = 17:55, "2.5%" = conf.intervals[1,], "Median" = storage1.median, "97.5%" = conf.intervals[2,])

#creating table 2
storage2.median <- 0
for (i in 1:39) {
  storage2.median[i] <- median(storagedf_2[i,])
}
df2 <- data.frame( "Age" = 17:55, "2.5%" = conf.intervals2[1,], "Median" = storage2.median, "97.5%" = conf.intervals2[2,])


#question #3 ----
library(boot)
set.seed(1)
nsw <- read.dta("nsw.dta")

nsw.model <- lm(re78 ~ treat, data = nsw)
summary(nsw.model)

boot.coef <- function(model, index) return(coef(model)[2])

boot.ci <- function(model){
  coef <- boot(model, boot.coef, 1000)
  coef.ci <- quantile(coef$t0, c(0.025, 0.975))
  return(coef.ci)
}

conf.int1 <- boot.ci(nsw.model)
conf.int2 <- confint(nsw.model, level = 0.95)[2,1:2]

df3 <- data.frame("Simulation" = conf.int1, "Formula" = conf.int2)

#question #4 ----

#function that calculates R-squared using observed and predicted Ys as inputs
rsquared <- function(observed, predicted) {
  rss <- sum((observed - predicted)^2) #residual sum of squares
  tss <- sum((observed - mean(observed))^2) #total sum of squares
  return(1-rss/tss)
}

nsw.predict <- predict(nsw.model) #calculating predicted Ys

r.squared <- rsquared(nsw$re78, nsw.predict) #R-squared calculated using the function created
r.squared2 <- summary(nsw.model)$r.squared #R-squared from the summary function

print(c(r.squared, r.squared2)) #printing the two R-squared

#question #5 ----

#removing data_id column to avoid an error
nsw.data <- nsw[,-1]

#logistic regression model
nsw.logit <- glm(treat ~. -re78, data = nsw.data, family = binomial)
summary(nsw.logit)

#creating treatment and control groups data frames
nsw.treat <- nsw.data[!(nsw.data$treat == 0),]
nsw.control <- nsw.data[!(nsw.data$treat == 1),]

#prediction probabilities for treatment and control
predicted.treat <- predict(nsw.logit, newdata = nsw.treat, type = "response")
predicted.control <- predict(nsw.logit, newdata = nsw.control, type = "response")

#treatment histogram
hist(predicted.treat,
     main="Distribution of the treatment group's estimated probabilities", 
     xlab="Probabilities", 
     xlim=c(0.3,0.6),
     ylim=c(0,25),
     freq=FALSE,
     col="red")

#control histogram
hist(predicted.control,
     main="Distribution of the control group's estimated probabilities", 
     xlab="Probabilities", 
     xlim=c(0.3,0.6),
     ylim=c(0,25),
     freq=FALSE,
     col="blue")

