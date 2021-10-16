#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul  2 22:27:31 2021

@author: claytonfields
"""



#### INitial code for research project, python version
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.formula.api as smf
from statsmodels.regression.linear_model import OLS

## Create Simulated Data
# Intercepts
alpha1 = 5
alpha2 = 35
alpha3 = 0
# Constant slope
beta = .5

# intervals for t
t1 = np.arange(1,181,1)
t2 = np.arange(181,421,1)
t3 = np.arange(421,601,1)
t = np.arange(1,601,1).reshape(-1,1)

# linear equation models
mu1 = alpha1 + beta*t1
mu2 = alpha2 + beta*t2
mu3 = alpha3 + beta*t3
mu = np.hstack((mu1,mu2,mu3)).reshape(-1,1)

# Error term
rng = np.random.default_rng(2244)
eps = rng.normal(size=600,scale=5).reshape(-1,1)

# Generate y
y = mu + eps
plt.plot(t,y)


### MDL: Objective Function
## Penalty Term (Called in Fitness() function)
def Penalty(t, tau):
  # Provides Pentalty needed for MDL
  # t: a vector of discrete time series points
  # tau: a vector of change points

  m = tau.shape[0]
  if(m<2):
    return(np.log(m+1))
  else:
    return(np.log(m+1)+ sum(np.log(tau[1:m])) + .5*sum(np.log(diff(tau))))
  


## Test case for testing the Fitness function: test1 = test2 ?
tau1 = 181
tau2 = 421
tau = np.array([tau1,tau2]).reshape(-1,1)

n=600
n1 = tau1-1
n2 = tau2-tau1
n3 = n+1-tau2

x1 = np.vstack((np.ones((n1,1)),np.zeros((n2+n3,1))))
x2 = np.vstack((np.zeros((n1,1)),np.ones((n2,1)),np.zeros((n3,1))))
x3 = np.vstack((np.zeros((n1+n2,1)),np.ones((n3,1))))
x = np.hstack((x1,x2,x3))

import statsmodels.formula.api as smf

# TODO; Find equivalent to lm, probably stats models
# mod = lm(y~x1+x2+x3+t+0)
mod = OLS(y,np.hstack((x,t)))
results = mod.fit()
test1 = mod.loglike(results.params)

model=smf.ols('Y~X',data=data)
results = model.fit()

# MDL score
def fitness(t,y,tau, min_len=5):
  # Provides MDL fitness score for a piecewise linear model 
  # Intended for use in GA methods below
  # t: vector of discrete time values
  # y: vecotr of true y values associated with t
  # tau: vector of changepint times for picewise model
  m = tau.shape[0]
  n = t.shape[0]
  if(m==0):# No chnagepoints, basic linear model
    mod = OLS(y,t)
    
  elif(m==1): # One changepoint, special case
    x1 = np.vstack((np.ones((tau[1]-1,1)), np.zeros((n+1-tau[1],1))))
    x2 = np.vstack((np.zeros(( tau[1]-1,1)), np.ones(( n+1-tau[1],1))))
    x = np.hstack((x1,x2))
    mod = OLS(y,np.hstack((x,t)))
  else:# two or more changepoints
    x = np.vstack((np.ones((tau[1]-1,1)), np.ones(( n +1 - tau[1],1))))
    i = 0
    while(i < m-1):
      temp = np.vstack((np.zeros((tau[i]-1,1)), np.ones((tau[i+1]-tau[i],1)), np.zeros((n + 1 - tau[i+1],1))))
      x = np.hstack((x,temp))
      i = i+1
    
    xm = np.vstack((np.zeros((tau[m]-1,1)), np.ones((n +1- tau[m],1))))
    x = np.hstack((x,xm))
    mod = OLS(y,np.hstack((x,t)))
  results = mod.fit()
  lik = logLik(results.params)
  # return -2*lik + 1.5*Penalty(t,tau)
  return lik
# End Fitness()

test2 = fitness(t,y,tau)






m = tau.shape[0]
n = t.shape[0]
df = pd.DataFrame([t,y])
if(m==0):# No chnagepoints, basic linear model
  mod = smf.OLS('y ~ t')
  pass
elif(m==1): # One changepoint, special case
  x1 = np.vstack((np.ones((tau[1]-1,1)), np.zeros((n+1-tau[1],1))))
  x2 = np.vstack((np.zeros(( tau[1]-1,1)), np.ones(( n+1-tau[1],1))))
  x = np.hstack((x1,x2))
  mod = smf.OLS('y~x+t+0')
else:# two or more changepoints
  x = np.vstack((np.ones((tau[1]-1,1)), np.ones(( n +1 - tau[1],1))))
  i = 0
  while(i < m-1):
    temp = np.vstack((np.zeros((tau[i]-1,1)), np.ones((tau[i+1]-tau[i],1)), np.zeros((n + 1 - tau[i+1],1))))
    x = np.hstack((x,temp))
    i = i+1
  
  xm = np.vstack((np.zeros((tau[m]-1,1)), np.ones((n +1- tau[m],1))))
  x = np.hstack((x,xm))
    # mod = lm(y~x+t+0)
  






### GA
## Initialize population
def Initial_pop(t,col_size=100, min_len=6, max_m=10):
  n = t.shape[0] # Number of observations
  m = 0 # number of change points
  crms = [np.array()]
  i=0
  
  while(length(crms)<col_size):
    #print(i)
    
    m = rng.integers(1, max_m, endpoint=True).item()   # m: number of changepoints
    tau = rng.choice(np.arange(min_len ,n-min_len, size=m, replace = False)) # randomly  m points 
    tau = np.sort(tau)
    if(m>1): # discard and rerun loop if number are too close together
      if(np.any(np.abs(np.diff(tau)<min_len))):
        i = i-1
        next()
      
    
    tau_name = toString(tau)
    crms[[i]] = tau
    # crms = crms[!duplicated(crms)]
    i=i+1
    return crms
 # End Initial_pop()

crms = initial_pop(t)

## Probablity for parents of next generation
def get_probs(crms,t,y):
  # fit_scores = c()
  # retval = list()
  # for(i in 1:length(crms)){
  #   fit_scores[i] = Fitness(t,y,unlist(crms[i],use.names = FALSE))
  # }
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
 # End get_probs()


probs = get_probs(crms,t,y)


## Sample Parent Crms
def next_gen(crms,probs,t,popsize=100, min_len=10):
  n = length(t)
  next_gen_list = list()
  # Add fittest crms to next gen
  best = unlist(crms[which.max(probs)], use.names = FALSE)
  next_gen_list[[toString(best)]] = best
  i = 2
  # Generate Next Generation
  while(length(next_gen_list) < popsize):
    parents = unlist(sample(crms,2,prob=probs,replace = FALSE),use.names = FALSE)
    parents = unique(parents)
    parents = sort(parents)
    # keep each point with .5 probabilty
    keep = c()
    for k in range(1,length(parents)):
      
      if(runif(1) < .5):
        keep = c(keep,k)
      
    
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
    if(length(idx) != 0 ):
      child = c(child,remain[idx])
      child = sort(child)
    
    child = unique(child)
    # Don't allow boundry values
    child = child[child<=550]
    child = child[child>=10]
    if(length(child)>1):# discard and rerun loop if number are too close together
      if(any(abs(diff(child)<min_len))):
        next()
      
    
    child_name = toString(child)
    next_gen_list[[child_name]] = sort(child)
    # Remove if duplicate
    # next_gen_list = next_gen_list[!duplicated(next_gen_list)]
    i = i+1
  
  return(next_gen_list)



def generate_linear_data(n=600,max_m = 10, max_alpha=100, min_len=10):
  # m = sample(1:max_m, 1)  # m: number of changepoints
  # m = 1
  # tau = sample(min_len:(n-min_len), m, replace = F) # randomly  m points 
  tau = sort(tau)
  # mu = c()
  # t_1 = 1:(tau[1]-1)
  # alpha_1 = sample(1:max_alpha,1)
  # beta_1 = runif(1,.1,5)
  mu = alpha1 + beta*t_1
  if(m>1):
    for i in range(1,(m-1)):
      # alpha_i = sample(1:max_alpha,1)
      # beta_i = runif(1,.1,5)
      # t_i = tau[i]:(tau[i+1]-1)
      mu_i = alpha_i + beta*t_i
      
      mu = c(mu,mu_i)
    
  
  # t_n = tau[m]:n
  # alpha_n = sample(1:max_alpha,1)
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

# t = 1:n
temp = generate_linear_data()
y = unlist(temp[1])
tau = unlist(temp[2])
plot(t,y)



### Run GA for Changepoints
## How should I save best results from each iteration?
sim_number = 3
winners = list()
for i in range(1,sim_number):
  set.seed(i)
  best_crms = c()
  best_scores = c()
  crms = Initial_pop(t)
  max_iter = 200
  # s
  for j in range(1,max_iter):
    vals = get_probs(crms,t,y)
    probs = unlist(vals[1])
    best_crms[[j]] = unlist(vals[2],use.names = FALSE)
    best_scores[[j]] =  unlist(vals[3])
    crms = next_gen(crms,probs,t)
  
  best =  unlist(best_crms[which.min(best_scores)])
  winners[[i]] = best


#plot best score versus iteraton
#plot best crms locations overlay onto plot of actual data

### Next?
## How much and what type of testing do we conduct here? 150
# iterations 150
## How are we to measure accuracy?
## How many times do we run simulations on this data?
## In what configurations?


