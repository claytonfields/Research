#### INitial code for research project
library(tidyverse)


## Create Simulated Data
# Intercepts
alpha1 = 5
alpha2 = 35
alpha3 = 0
# Constant slope
beta = .5

# intervals for t
t1 = 1:180
t2 = 181:420
t3 = 421:600
t = 1:600

# linear equation models
mu1 = alpha1 + beta*t1
mu2 = alpha2 + beta*t2
mu3 = alpha3 + beta*t3
mu = c(mu1,mu2,mu3)

# Error term
set.seed(2244)
eps = rnorm(600,0,5)

# Generate y
y = mu + eps
plot(t,y)


generate_linear_data = function(n=600,max_m  =5, max_alpha=50, min_len=10){
# n = 600
# max_m = 5
# max_alpha = 60
# min_len = 10
m = sample(1:max_m, 1)  # m: number of changepoints
tau = sample(min_len:(n-min_len), m, replace = F) # randomly  m points 
tau = sort(tau)
# mu = c()
t_1 = 1:(tau[1]-1)
alpha_1 = sample(1:max_alpha,1)
mu = alpha1 + beta*t_1
if(m>1){
  for(i in 1:(m-1)){
    alpha_i = sample(1:max_alpha,1)
    t_i = tau[i]:(tau[i+1]-1)
    mu_i = alpha_i + beta*t_i
    
    mu = c(mu,mu_i)
  }
}
t_n = tau[m]:n
alpha_n = sample(1:max_alpha,1)
mu_n = alpha_n + beta*t_n
mu = c(mu,mu_n)

eps = rnorm(600,0,5)
y = mu + eps
return(y)
}
t = 1:n
plot(t,y)






## Test case for testing the Fitness function: test1 = test2 ?
tau1 = 181
tau2 = 421
tau = c(tau1,tau2)

n=600
n1 = tau1-1
n2 = tau2-tau1
n3 = n+1-tau2

x1 = c(rep(1,n1),rep(0,n2+n3))
x2 = c(rep(0,n1),rep(1,n2),rep(0,n3))
x3 = c(rep(0,n1+n2),rep(1,n3))
x = cbind(x1,x2,x3)

mod = lm(y~x1+x2+x3+t+0)
summary(mod)
test1 = logLik(mod)

t
y
tau
min_len=5






