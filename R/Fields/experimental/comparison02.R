#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objective: This program applies the GA method to detect structural changes in a simple        ]*#
#*[            regression model setting.                                                          ]*#
#*[ Updated  : 07/30/2021                                                                         ]*#
#*[ Author   : Jaechoul Lee                                                                       ]*#
#*[-----------------------------------------------------------------------------------------------]*#
program_start = proc.time()  
# Required packages
library(tidyverse)
library(strucchange)
library(dpseg)
library(segmented)
library(aspline)

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

### Dataset 5
##  Linear, 4 changepoints
# Parameters
n = 1000
sigma = 3
m_true = 4

# xi
xi_1 = 8
xi_2 = 21
xi_3 = 44
xi_4 = 60
xi = c(xi_1, xi_2, xi_3, xi_4)
cp = c(m_true, xi)

#  Generate X
X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X >= xi_1 & X < xi_2]
X3 = X[X >= xi_2 & X < xi_3]
X4 = X[X >= xi_3 & X < xi_4]
X5 = X[X >= xi_4]

#Generate y
y1 = rep(16, length(X1))
y2 = 2*X2
y3 = rep(42, length(X3))
y4 = -X4 + 86
y5 = rep(26, length(X5))
y_true = c(y1,y2,y3,y4,y5)
eps = rnorm(n,0,sigma)
y = y_true + eps


df2 = tibble(X,y,y_true)
ggplot(df2) + geom_point(aes(X,y), color='gray') +
  geom_line(aes(X,y_true)) + 
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
    loglik = numeric(),
    penalty = numeric(),
    penlik = numeric(),
    truelik = numeric(),
    truepen = numeric(),
    truepenlike = numeric()
  )
}

# Data Structures
results_ga = get_df()
# results_ga_0001 =  get_df()
# results_ar = get_df()
# results_sc = get_df()
# results_dp = get_df()
# results_sg = get_df()

# Parameters for GA
p.mut = .1
max.itr = 150
x.inc = 1000
x.min = min(X)
x.max = max(X)

# Parameters for aridge
# k = 100
# knots = seq(min(X), max(X), length = k + 2)[-c(1, k + 2)]
# degree <- 1
# pen = 10 ^ seq(-4, 4, 0.25)
# x_seq = seq(min(X), max(X), length = 1000)
# epsilon = .001

# Parameters for strucchange
# h = 75


start_pos = 231
end_pos = 240
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
  ga.out = ga.cpt_Norm(y=y, x=X,fitness=pnllik.MDL.M0Z, p.mut=p.mut, x.inc=x.inc,
                       max.itr=max.itr,seed=seed_i, is.print = FALSE, is.graphic = FALSE)
  chrom.sol = ga.out$solution
  m = chrom.sol[1]
  log.lik = nloglik.M0Z_glm(y,NULL,X,chrom.sol) 
  pen.mdl = penalty.MDL(y,X,chrom.sol,x.min, x.max, x.inc)
  penlik = pnllik.MDL.M0Z(y,NULL,X,chrom.sol,x.min,x.max,x.inc) 
  
  true.log.lik = nloglik.M0Z_glm(y,NULL,X,cp) 
  true.pen.mdl = penalty.MDL(y,X,cp,x.min, x.max, x.inc)
  true.pen.lik = true.log.lik+true.pen.mdl
  
  row = c(i,seed_i,chrom.sol,rep(0,6-m),log.lik,pen.mdl,penlik,true.log.lik,true.pen.mdl,true.pen.lik)
  results_ga[nrow(results_ga)+1,] = row
  print(m)
  
  # ga.out = ga.cpt_Norm(y=y, x=X,fitness=pnllik.MDL.M0Z, p.mut=.0001, x.inc=x.inc,
  #                      max.itr=max.itr,seed=seed_i, is.print = FALSE)
  # 
  # chrom.sol = ga.out$solution
  # m = chrom.sol[1]
  # log.lik = nloglik.M0Z_glm(y,NULL,X,chrom.sol) 
  # pen.mdl = penalty.MDL(y,X,chrom.sol,x.min, x.max, x.inc)
  # penlik = pnllik.MDL.M0Z(y,NULL,X,chrom.sol,x.min,x.max,x.inc) 
  # 
  # true.log.lik = nloglik.M0Z_glm(y,NULL,X,cp) 
  # true.pen.mdl = penalty.MDL(y,X,cp,x.min, x.max, x.inc)
  # true.pen.lik = true.log.lik+true.pen.mdl
  # 
  # row = c(i,seed_i,chrom.sol,rep(0,6-m),log.lik,pen.mdl,penlik,true.log.lik,true.pen.mdl,true.pen.lik)
  # results_ga_0001[nrow(results_ga)+1,] = row
  # print(m)

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
  # temp = breakpoints(y~X,h=h)
  # bp_sc = X[temp$breakpoints]
  # m_sc = length(bp_sc)
  # print(m_sc)
  # 
  # row_sc = c(i,seed_i,m_sc,bp_sc,rep(0,10-m_sc))
  # results_sc[nrow(results_sc)+1,] = row_sc

  # # Time iteration
  # loop_end = proc.time() - begin
  # print(loop_end)
}




## Write to file
# write_csv(results_ga, paste('results_ga_',m_true,'_v02_i_',n,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))
# write_csv(results_ar, paste('results_ar_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))
# write_csv(results_dp, paste('results_dp_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))
# write_csv(results_sg, paste('results_sg_',m_true,'_v01_i_',start_pos,'_',end_pos,'_',p.mut,'_',max.itr,'_',x.inc, sep=''))


# program_end =  proc.time() - begin

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
# print(prop_correct(results_sc,m_true))
# mle(y, X, x.min, x.max, x.inc, g, Confg,  fitness, z = NULL, gen.size = 200,
#               is.graphic = FALSE, Confg.pre.sol.lik= NULL )
