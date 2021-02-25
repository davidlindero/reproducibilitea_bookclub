## Kata Szita, look for preregistration of last study. Very thorough setup already. Both hypothesis' and 
## testing already decided (or at least, a probable suggestion). 



# 9.4 Partial pooling: 

# forecast 42%, sd 5. 
# data 54%

# a) 
# posterior mean is 49%, how large is the sample size? 

# 49 = (0.42/(0.05^2) + 0.54/(x^2)) / (1/(0.05^2) + 1/(x^2))

# x = sqrt(0.54*(1-.54)/num)

# b) posterior prob. of >50%



# 9.8

# gain is 500-300 with error of 200, gain for one ad can be modelled as avg 200 w se 200. 

# source("./exercises_6.R")




simulate_n_sums <- function(n) {
    # x_data <- runif(n, min=0, max=xmax)
    
    nr_sums <- c()
    for (i in seq(1, n, by=1)) {
        curr_nums <- rnorm(20, mean=200, sd=200)
        nr_sums[i] <- sum(curr_nums)
    }
    return(nr_sums)
}

# many_sums <- simulate_n_sums(500)
# print(many_sums)
# png('./simulated_nums.png')
# hist(many_sums)
# dev.off()

simple_simulate_n_datapoints <- function(a, b, n, th, xmin=-1, xmax=1) {
    x_data <- runif(n, min=xmin, max=xmax)
    errors <- rnorm(n, mean=0, sd=th)
    y_data <- a + b*x_data + errors

    all_data <- data.frame(x_data, y_data)

    return(all_data)
}


# 9.10
library("rstanarm")
library("plyr")
source("./exercises_6.R")

ns_vec <- c()
ns_vec <- seq(10, 1000, by=10)
# for (i in seq(1, 1000, by=10)) {
#     ns_vec[i] <- as.integer(10^(i/100))
# }

lm_coeffs = c()
lm_intercepts = c()
stan_coeffs = c()
stan_intercepts = c()


# all_data <- simple_simulate_n_datapoints(a=1, b=0.1, n=100, th=0.5, xmin=-1, xmax=1)   
# lm_trad <- lm(y_data ~ x_data, data=all_data) 
# print(lm_trad$coefficients)
# quit()

for (ii in ns_vec) {
    all_data <- simple_simulate_n_datapoints(a=1, b=0.1, n=ii, th=0.5, xmin=-1, xmax=1)   

    stan_lm_xy <- stan_glm(y_data ~ x_data, data=all_data, 
                  prior=normal(0, 0.2), prior_intercept=normal(0, 1))
    
    # stan_coeffs <- summary(stan_lm_xy)$coefficients
    lm_trad <- lm(y_data ~ x_data, data=all_data) 
    # trad_coeffs <- c(coef(lm_trad)["x_data"], coef(lm_trad)["(Intercept)"])


    stan_coeffs = c(stan_coeffs, coef(stan_lm_xy)["x_data"])
    stan_intercepts = c(stan_intercepts, coef(stan_lm_xy)["(Intercept)"])

    lm_coeffs <- c(lm_coeffs, coef(lm_trad)["x_data"])
    lm_intercepts <- c(lm_intercepts, coef(lm_trad)["(Intercept)"])

    # print(stan_coeffs)
    # print(lm_intercepts)
    # quit()
}

# print(lm_intercepts)
pdf('./lm_intercepts.pdf')
plot(lm_intercepts)
dev.off()

pdf('./lm_coeffs.pdf')
plot(lm_coeffs)
dev.off()

pdf('./stan_coeffs.pdf')
plot(stan_coeffs)
dev.off()

pdf('./stan_int.pdf')
plot(stan_intercepts)
dev.off()

# 3d-plot likelihood-style. 

# catchy-ish prior, kolla cauchy


# plyrFunc_lm <- function(x){
#   mod <- lm(y_data ~ x_data, data = x)
#   return(summary(mod)$coefficients[2,3])
#   }

# tStats_lm <- ddply(all_data, .(a), plyrFunc)



# print(ns_vec)

# all_data <- simple_simulate_n_datapoints(a=1, b=0.1, n=1000, th=0.5, xmin=-1, xmax=1)
# stan_lm_xy <- stan_glm(y_data ~ x_data, data=all_data, 
#                   prior=normal(0, 0.2), prior_intercept=normal(0, 1))
# lm_trad <- lm(y_data ~ x_data, data=all_data)
# print(stan_lm_xy)
# print('    --    ')
# print(lm_trad)



