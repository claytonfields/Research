#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objective: This program applies the GA method to detect structural changes in a simple        ]*#
#*[            regression model setting.                                                          ]*#
#*[ Updated  : 07/30/2021                                                                         ]*#
#*[ Author   : Jaechoul Lee                                                                       ]*#
#*[-----------------------------------------------------------------------------------------------]*#
# Required packages
library(tidyverse)
library(strucchange)
# library(dpseg)
# library(segmented)
# library(aspline)

### Setup library, data, and output directories
WD.lib = c('Fields/experimental/')
WD.inp = c('data/')
### Load the proposed GA and fitted model likelihood funtion packages
source(file=paste(WD.lib,"ga_cont_02.R",sep=""))
source(file=paste(WD.lib,"mle_lin_02.R",sep=""))

#*[-----------------------------------------------------------------------------
#*------------------]*#
### Case 1:Simulated Linear Data:
### Dataset 6: 5 changepoints
#*[-----------------------------------------------------------------------------------------------]*#

## Dataset 3
# Linear, 2 changepoints
# Parameters
n = 150
sigma = 3
m_true = 2
#  Generate X
xi_1 = 26
xi_2 = 70
xi = c(2,xi_1, xi_2)
X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X >= xi_1 & X < xi_2]
X3 = X[X >= xi_2]
# Generate y
y1 = -.6*X1 + 67.4
y2 = 2*X2
y3 = -.4*X3+168
y_true = c(y1,y2,y3)
eps = rnorm(n,0,sigma)
y = y_true + eps
# Plot
df0 = tibble(X,y,y_true)
ggplot(df0) + geom_point(aes(X,y), color='gray') + 
  geom_line(aes(X,y_true), color='red') + 
  ggtitle('Linear Model: 2 Changepoint')

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
results_sc = get_df()

# Parameters for GA
p.mut = .01
max.itr = 150
a = 20
x.min = min(X)
x.max = max(X)

# Parameters for strucchange
h = 4


start_pos = 41
end_pos = 60
for(i in start_pos:end_pos){
  loop_start = proc.time()  
  seed_i = 1000*(i-1)+543
  set.seed(seed_i)
  eps = rnorm(n,0,sigma)
  y = y_true + eps
  dfi = tibble(X,y)
  print(paste('Iteration',i))
  print(y[1])
  
  # GA
  ga.out = ga.cpt_Norm(y=y, x=X,fitness=pnllik.MDL.M0Z, p.mut=p.mut, a=a, min.samp = 6,
                       max.itr=max.itr,seed=seed_i, is.print = FALSE, is.graphic = FALSE)
  chrom.sol = ga.out$solution
  m = chrom.sol[1]


  row = c(i,seed_i,chrom.sol,rep(0,10-m))
  results_ga[nrow(results_ga)+1,] = row
  print(m)

  
  
  # strucchange
  temp = breakpoints(y~X,h=h)
  bp_sc = X[temp$breakpoints]
  m_sc = length(bp_sc)
  print(m_sc)

  row_sc = c(i,seed_i,m_sc,bp_sc,rep(0,10-m_sc))
  results_sc[nrow(results_sc)+1,] = row_sc

  # # Time iteration
  loop_end = proc.time() - loop_start
  print(loop_end)
}




## Write to file
# write_csv(results_ga, paste('results_ga_',m_true,'_v02_i_',n,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))
# write_csv(results_ar, paste('results_ar_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))
# write_csv(results_dp, paste('results_dp_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))
# write_csv(results_sg, paste('results_sg_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))

## Utility Functions
prop_correct = function(df,m_true){
  
  correct = filter(df,m==m_true)
  good = nrow(correct)
  total = nrow(df)
  good/total
}
get_cp = function(df){
  df %>% select(4:13) %>% 
    gather(1:10,key = 'name',value = 'cp') %>%
    select(cp) %>% 
    filter(cp != 0)
}

print('Proportion Correct GA')
print(prop_correct(results_ga,m_true))
print('Proportion Correct SC')
print(prop_correct(results_sc,m_true))
# mle(y, X, x.min, x.max, x.inc, g, Confg,  fitness, z = NULL, gen.size = 200,
#               is.graphic = FALSE, Confg.pre.sol.lik= NULL )
