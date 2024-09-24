## YIMING SHEN 20891774
## 20220523
## assignment 1 question 4

# (a)
x <- flowrate$PressureDrop
y <- flowrate$FlowRate
plot(x,y,xlab="Pressure Drop (in inches of water)", 
     ylab="Flow Rate (in m^3/min)", 
     main="Scatterplot: Pressure Drop VS. Flow Rate",
     pch=1, cex=0.7, col="navy", las=1, lwd=1)

# (b)
m1 <- lm(FlowRate~PressureDrop, data=flowrate)
summary(m1)
abline(coef(m1),lwd=2,lty=2,col="red")

# (c)
# T-test
# from summary directly
d <- coefficients(summary(m1))[2,3]
d
p_value <- coefficients(summary(m1))[2,4]
p_value
# manually calculate
beta1_hat <- coefficients(summary(m1))[2,1]
beta1_hat
SE1 <- coefficients(summary(m1))[2,2]
SE1
n <- nrow(flowrate)
n
d <- (beta1_hat-0)/SE1
d
p_value1 <- 2*(1-pt(abs(d),n-2))
p_value1

# (d)
# manually calculate
sigma_sq_hat <- sum(m1$residuals^2) / (n-2)
xbar <- mean(x)
Sxx <- sum( (x-xbar)^2 )
c <- qt(0.975, n-2)
x_p <- 10
fitted_value_10 <- coefficients(summary(m1))[1,1] + coefficients(summary(m1))[2,1]*x_p
fitted_value_10
# lower bound
lb_est <- fitted_value_10 - c * sqrt(sigma_sq_hat*(1/n + (x_p-xbar)^2/(Sxx)))
# upper bound
ub_est <- fitted_value_10 + c * sqrt(sigma_sq_hat*(1/n + (x_p-xbar)^2/(Sxx)))
lb_est
ub_est
# check with R
est_10 <- predict(m1, data.frame(PressureDrop = 10),
                  interval = "confidence", level = 0.95)
est_10

# (e)
# manually calculate
# lower bound
lb_pre <- fitted_value_10 - c * sqrt(sigma_sq_hat*(1 + 1/n + (x_p-xbar)^2/(Sxx)))
# upper bound
ub_pre <- fitted_value_10 + c * sqrt(sigma_sq_hat*(1 + 1/n + (x_p-xbar)^2/(Sxx)))
lb_pre
ub_pre
# check with R
pre_10 <- predict(m1, data.frame(PressureDrop = 10),
                  interval = "prediction", level = 0.95)
pre_10


## assignment01 question05
set.seed(20891774)
# (a)
# (i)
x_i <- c(8,4,7,8,6,2,3,5,10,5)
beta_0 <- 5
beta_1 <- 1
sigm_a <- 1
y_i <- rnorm(10,beta_0+beta_1*x_i,sigm_a)
y_i
# (ii)
m2 <- lm(y_i~x_i)
summary(m2)
# (iii)
beta_1_hat <- coefficients(summary(m2))[2,1]
beta_1_hat
# (iv)
v <- 0
for (i in 1:1000) {
  y_ii <- rnorm(10,beta_0+beta_1*x_i,sigm_a)
  v[i] <- coefficients(summary(lm(y_ii~x_i)))[2,1]
}
v

# (b)
hist(v,xlab="Value of the estimated coefficients for beta1", 
     main="Histogram: Distribution of 1000 estimated beta1", 
     xlim = c(0.45,1.55),
     ylim = c(0,300))

# (c)
v_1 <- 0
for (i in 1:1000) {
  y_c <- rnorm(10,beta_0+beta_1*x_i,sigm_a)
  v_1[i] <- coefficients(summary(lm(y_c~x_i)))[1,1]
}

hist(v_1,xlab="Value of the estimated coefficients for beta0", 
     main="Histogram: Distribution of 1000 estimated beta0",
     ylim = c(0,250))

# (d)
sigm_a_new <- 0.5
v_2 <- 0
for (i in 1:1000) {
  y_iii <- rnorm(10,beta_0+beta_1*x_i, sigm_a_new)
  v_2[i] <- coefficients(summary(lm(y_iii~x_i)))[2,1]
}
hist(v_2,xlab="Value of the estimated coefficients for beta1", 
     main="Histogram: Distribution of 1000 estimated beta1 (new sigm = 0.5)",
     xlim = c(0.45,1.55),
     ylim = c(0,300))









