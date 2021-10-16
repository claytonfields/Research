#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objective: This program applies the GA method to detect structural changes in a simple        ]*#
#*[            regression model setting.                                                          ]*#
#*[ Updated  : 07/30/2021                                                                         ]*#
#*[ Author   : Jaechoul Lee                                                                       ]*#
#*[-----------------------------------------------------------------------------------------------]*#
# Required packages
library(tidyverse)
library(strucchange)
library(dpseg)
library(segmented)
library(aspline)

### Setup library, data, and output directories
WD.lib = c('Fields/')
WD.inp = c('data/')
### Load the proposed GA and fitted model likelihood funtion packages
source(file=paste(WD.lib,"ga_cont_01.R",sep=""))
source(file=paste(WD.lib,"mle_lin_01.R",sep=""))

#*[-----------------------------------------------------------------------------
#*------------------]*#
### Case 1:Simulated Linear Data:
### Dataset 6: 5 changepoints
#*[-----------------------------------------------------------------------------------------------]*#

## Dataset 4
# Linear, 4 changepoints
# Parameters
n = 250
sigma = 3
m_true = 4
# Generate xi
xi_1 = 8
xi_2 = 21
xi_3 = 44
xi_4 = 60
xi = c(4,xi_1, xi_2, xi_3, xi_4)
# Generate X
# X = seq(from=0, to=100, length.out = n)
# X1 = X[X < xi_1]
# X2 = X[X >= xi_1 & X < xi_2]
# X3 = X[X >= xi_2 & X < xi_3]
# X4 = X[X >= xi_3 & X < xi_4]
# X5 = X[X >= xi_4]
X1 = seq(from=0,to=7.9, length.out = 30)
X2 = seq(from=8,to=20.9, length.out = 30)
X3 = seq(from=21, to = 43.9, length.out = 75)
X4 = seq(from= 44, to=59.9, length.out = 75)
X5 = seq(from = 60, to = 100, length.out = 40)
X = c(X1,X2,X3,X4,X5)
# Generate y
y1 = rep(16, length(X1))
y2 = 2*X2
y3 = rep(42, length(X3))
y4 = -X4 + 86
y5 = rep(26, length(X5))
y_true = c(y1,y2,y3,y4,y5)
eps = rnorm(n,0,sigma)
y = y_true + eps
# Plot
df0 = tibble(X,y,y_true)
ggplot(df0) + geom_point(aes(X,y),color='gray') +
  geom_line(aes(X,y_true),color='red') + 
  ggtitle('Linear Model: 4 Changepoints') 

### Find structural changes via the proposed GA method
## Utility Functions
get_df = function(){
  data.frame(
    i = integer(),
    seed_i = integer(),
    m = integer(),
    c1 = numeric(),
    c2 = numeric(),
    c3 = numeric(),
    c4 = numeric(),
    c5 = numeric(),
    c6 = numeric(),
    c7 = numeric(),
    c8 = numeric(),
    c9 = numeric(),
    c10 = numeric()
  )
}

# Data Structures
results_ga = get_df()
results_ar = get_df()
results_sc = get_df()
# results_dp = get_df()
# results_sg = get_df()

# Parameters for GA
p.mut = .01
max.itr = 150
x.inc = 45

# Parameters for aridge
k = 100
knots = seq(min(X), max(X), length = k + 2)[-c(1, k + 2)]
degree <- 1
pen = 10 ^ seq(-4, 4, 0.25)
x_seq = seq(min(X), max(X), length = 1000)
epsilon = .001

start_pos = 1
end_pos = 10
for(i in start_pos:end_pos){
  begin = proc.time()  
  seed_i = 1000*(i-1)+543
  eps = rnorm(n,0,sigma)
  y = y_true + eps
  dfi = tibble(X,y)
  print(paste('Iteration',i))
  
  # GA
  ga.out = ga.cpt_Norm(y=y, x=X,fitness=pnllik.MDL.M0Z, p.mut=p.mut, x.inc=x.inc,
                       max.itr=max.itr,seed=seed_i, is.print = FALSE)
  elapsed = proc.time() - begin
  print(elapsed)
  chrom.sol = ga.out$solution
  m = chrom.sol[1]
  row = c(i,seed_i,chrom.sol,rep(0,10-m))
  results_ga[nrow(results_ga)+1,] = row

  # ## Segmented
  # fit_lm = lm(y ~  X, data = dfi)  # intercept-only model
  # temp_sg = selgmented(fit_lm, return.fit = TRUE)
  # temp_sg2 = temp_sg$psi[,'Est.']
  # m_sg = length(temp_sg2)
  # row_sg = c(i,seed_i,m_sg,temp_sg2,rep(0,10-m_sg))
  # results_sg[nrow(results_sg)+1,] = row_sg
  # 
  # # Aridge
  # aridge <- aridge_solver(X, y,degree = degree,epsilon = epsilon)
  # temp = aridge$knots_sel[[which.min(aridge$ebic)]]
  # m_ar = length(temp)
  # row_ar = c(i,seed_i,m_ar,temp,rep(0,10-m_ar))
  # results_ar[nrow(results_ar)+1,r] = row_ar
  
  # ## dpseg
  # seg_mod = dpseg(X,y,minl = 100)
  # temp_dp = seg_mod$segments[-1,'x1']
  # m_dp = length(temp_dp)
  # row_dp = c(1,2,m_dp,temp_dp,rep(0,10-m_dp))
  # results_dp[nrow(results_dp)+1,] = row_dp
  
  # strucchange
  temp = breakpoints(y~X,h=25)
  bp_sc = X[temp$breakpoints]
  m_sc = length(bp_sc) 
  print(m_sc)
  
  row_sc = c(i,seed_i,m_sc,bp_sc,rep(0,10-m_sc))
  results_sc[nrow(results_sc)+1,] = row_sc
}

## Write to file
# write_csv(results_ga, paste('results_ga_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))
# write_csv(results_ar, paste('results_ar_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))
# write_csv(results_dp, paste('results_dp_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))
# write_csv(results_sg, paste('results_sg_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))





# ga
# 4     8.318895    20.493580    40.951326    60.951148
#sc
# 4   10.0   20.8   46.2   58.4
# true
# 4  8 21 44 60





