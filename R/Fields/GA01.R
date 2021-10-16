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


### MDL: Objective Function
## Penalty Term (Called in Fitness() function)
Penalty = function (t, tau){
  # Provides Pentalty needed for MDL
  # t: a vector of discrete time series points
  # tau: a vector of change points

  m = length(tau)
  if(m<2){
    return(log(m+1))
  } else {
    return(log(m+1)+ sum(log(tau[2:m])) + .5*sum(log(diff(tau))))
  }
}

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


# MDL score
Fitness = function(t,y,tau, min_len=5){
  # Provides MDL fitness score for a piecewise linear model 
  # Intended for use in GA methods below
  # t: vector of discrete time values
  # y: vecotr of true y values associated with t
  # tau: vector of changepint times for picewise model
  m = length(tau)
  n = length(t)
  if(m==0){ # No chnagepoints, basic linear model
    mod = lm(y~t)
  } else if(m==1){ # One changepoint, special case
    x1 = c(rep(1, tau[1]-1), rep(0, n+1-tau[1]))
    x2 = c(rep(0, tau[1]-1), rep(1, n+1-tau[1]))
    x = cbind(x1,x2)
    mod = lm(y~x+t+0)
  } else {# two or more changepoints
    x = c(rep(1,tau[1]-1), rep(0, n +1 - tau[1]))
    i = 1
    while(i <= m-1){
      temp = c(rep(0,tau[i]-1), rep(1,tau[i+1]-tau[i]), rep(0,n + 1 - tau[i+1]))
      x = cbind(x,temp)
      i = i+1
    }
    xm = c(rep(0, tau[m]-1), rep(1, n +1- tau[m]))
    x = cbind(x,xm)
    mod = lm(y~x+t+0)
  }
  lik = logLik(mod)
  return(-2*lik + 1.5*Penalty(t,tau))
} # End Fitness()



test2 = Fitness(t,y,tau)

### GA
## Initialize population
Initial_pop = function(t,col_size=100, min_len=6, max_m=10){ 
  n = length(t) # Number of observations
  m = 0 # number of change points
  crms = list(c())
  i=1
  
  while(length(crms)<col_size){
    #print(i)
    i=i+1
    m = sample(1:max_m, 1)  # m: number of changepoints
    tau = sample(min_len:(n-min_len), m, replace = F) # randomly  m points 
    tau = sort(tau)
    if(m>1){ # discard and rerun loop if number are too close together
      if(any(abs(diff(tau)<min_len))){
        i = i-1
        next()
      }
    }
    tau_name = toString(tau)
    crms[[i]] = tau
    crms = crms[!duplicated(crms)]
  }
    return(crms)
} # End Initial_pop()

crms = Initial_pop(t)

## Probablity for parents of next generation
get_probs = function(crms,t,y){
  fit_scores = c()
  retval = list()
  for(i in 1:length(crms)){
    fit_scores[i] = Fitness(t,y,unlist(crms[i],use.names = FALSE))
  }
  rank_scores = rank(-fit_scores)
  probs = rank_scores/sum(rank_scores)
  print(crms[which.max(probs)])
  best_crm = crms[which.max(probs)]
  best_score = min(fit_scores)
  # print(best_score)
  # print(fit_scores[which.max(probs)])
  # retval = cbind(crms,fit_scores,probs)
  retval[[1]] = probs
  retval[[2]] = best_crm
  retval[[3]] = best_score
  # return(probs)
  return(retval)
} # End get_probs()


probs = get_probs(crms,t,y)


## Sample Parent Crms
next_gen = function(crms,probs,t,popsize=100, min_len=10){
  n = length(t)
  next_gen_list = list()
  # Add fittest crms to next gen
  best = unlist(crms[which.max(probs)], use.names = FALSE)
  next_gen_list[[toString(best)]] = best
  i = 2
  # Generate Next Generation
  while(length(next_gen_list) < popsize){
    parents = unlist(sample(crms,2,prob=probs,replace = FALSE),use.names = FALSE)
    parents = unique(parents)
    parents = sort(parents)
    # keep each point with .5 probabilty
    keep = c()
    for(k in 1:length(parents)){
      
      if(runif(1) < .5){
        keep = c(keep,k)
      }
    }
    child = parents[keep]
    # random shift for remaining points
    shift = sample(c(-1,0,1),length(child),prob = c(0.3,0.4,0.3),replace = TRUE)
    child = child + shift
    # Mutation
    # For each non-change point assign small mutation probabilty, generate uniform random and if that 
    # number is < mutation probabilty that non-changepoint becomes a changeppoint by adding to crm
    # Sample*mutation prob = Expected number of mutations
    remain = t[-child]
    m_prob = runif(length(remain))
    idx = which(m_prob < .0001)
    if(length(idx) != 0 ){
      child = c(child,remain[idx])
      child = sort(child)
    }
    child = unique(child)
    # Don't allow boundry values
    child = child[child<=550]
    child = child[child>=10]
    if(length(child)>1){ # discard and rerun loop if number are too close together
      if(any(abs(diff(child)<min_len))){
        next()
      }
    }
    child_name = toString(child)
    next_gen_list[[child_name]] = sort(child)
    # Remove if duplicate
    next_gen_list = next_gen_list[!duplicated(next_gen_list)]
    i = i+1
  }
  return(next_gen_list)
}


generate_linear_data = function(n=600,max_m = 10, max_alpha=100, min_len=10){
  m = sample(1:max_m, 1)  # m: number of changepoints
  # m = 1
  tau = sample(min_len:(n-min_len), m, replace = F) # randomly  m points 
  tau = sort(tau)
  # mu = c()
  t_1 = 1:(tau[1]-1)
  alpha_1 = sample(1:max_alpha,1)
  # beta_1 = runif(1,.1,5)
  mu = alpha1 + beta*t_1
  if(m>1){
    for(i in 1:(m-1)){
      alpha_i = sample(1:max_alpha,1)
      # beta_i = runif(1,.1,5)
      t_i = tau[i]:(tau[i+1]-1)
      mu_i = alpha_i + beta*t_i
      
      mu = c(mu,mu_i)
    }
  }
  t_n = tau[m]:n
  alpha_n = sample(1:max_alpha,1)
  # beta_n = runif(1,.1,5)
  mu_n = alpha_n + beta*t_n
  mu = c(mu,mu_n)
  print(tau)
  eps = rnorm(n,0,5)
  y = mu + eps
  retval = list()
  retval[['y']] = y
  retval[['tau']] = tau
  return(retval)
}
t = 1:n
temp = generate_linear_data()
y = unlist(temp[1])
tau = unlist(temp[2])
plot(t,y)



### Run GA for Changepoints
## How should I save best results from each iteration?
sim_number = 3
winners = list()
for(i in 1:sim_number){
  set.seed(i)
  best_crms = c()
  best_scores = c()
  crms = Initial_pop(t)
  max_iter = 200
  # s
  for(j in 1:max_iter){
    vals = get_probs(crms,t,y)
    probs = unlist(vals[1])
    best_crms[[j]] = unlist(vals[2],use.names = FALSE)
    best_scores[[j]] =  unlist(vals[3])
    crms = next_gen(crms,probs,t)
  }
  best =  unlist(best_crms[which.min(best_scores)])
  winners[[i]] = best
}  

#plot best score versus iteraton
#plot best crms locations overlay onto plot of actual data

### Next?
## How much and what type of testing do we conduct here? 150
# iterations 150
## How are we to measure accuracy?
## How many times do we run simulations on this data?
## In what configurations?


