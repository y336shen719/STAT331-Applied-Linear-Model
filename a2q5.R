## Yiming Shen
## Assignment02 Question05

# (a)
fit = lm(ChangeMS ~ TvAd + NewspaperAd + RetailerIncentive, data = advertising)
summary(fit)

# (c)
# for TV adverstising
beta1_hat <- coefficients(summary(fit))[2,1]
beta1_hat
SE_beta1 <- coefficients(summary(fit))[2,2]
# construct 95% CI
k_tv <- 1/3
# lower bound
lb_tv <- k_tv*beta1_hat - qt(0.975,4)*k_tv*SE_beta1
# upper bound
ub_tv <- k_tv*beta1_hat + qt(0.975,4)*k_tv*SE_beta1
lb_tv; ub_tv

# for newspaper adverstising
beta2_hat <- coefficients(summary(fit))[3,1]
beta2_hat
SE_beta2 <- coefficients(summary(fit))[3,2]
# construct 95% CI
k_nw <- 1/2
# lower bound
lb_nw <- k_nw*beta2_hat - qt(0.975,4)*k_nw*SE_beta2
# upper bound
ub_nw <- k_nw*beta2_hat + qt(0.975,4)*k_nw*SE_beta2
lb_nw; ub_nw


# (d)
## predicting using R
a_vec <- matrix(c(1,0,2,0), ncol=1)
y_p <- t(a_vec) %*% coef(fit)
y_p

## 95% PI
var_beta <- vcov(fit)
var_beta
sigma_est <- sigma(fit) ; sigma_est
se <- sqrt(sigma_est^2+t(a_vec) %*% var_beta %*% a_vec); se
lb_pi = y_p - qt(0.975, 4) * se 
ub_pi = y_p + qt(0.975, 4) * se 
lb_pi; ub_pi



