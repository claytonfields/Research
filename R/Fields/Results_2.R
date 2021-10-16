library(segmented)
library(tidyverse)
library(changepoint.np)
library(cpm)
library(strucchange)
library(modelr)
library(dpseg)
# library(goepp/aspline)

#### Utility Functions
prop_correct = function(df,m_true){
  m = df['m']
  correct = m[m==m_true,]
  good = nrow(correct)
  total = nrow(m)
  good/total
}

get_cp = function(df){
  df %>% select(4:13) %>% 
    gather(1:10,key = 'name',value = 'cp') %>%
    select(cp) %>% 
    filter(cp != 0)
}


## Dataset 3
# Linear, 2 changepoints
# Parameters
n = 1000
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
ggsave('data/sim_study/examples/linear_2cpts.png')
write_csv(df0,'data/sim_study/examples/linear_2cpts.csv')


## Simulation study results: GA
# p.mut = .01
# max.itr = 150
# x.inc = 45
ga = read_csv('data/sim_study/2_changepoints/ga/results_ga_2_v01_i_1_1000_0.01_150_45')
# Proportion where m==m_true
ga_correct = prop_correct(ga,m_true)
ggplot(ga)+geom_histogram(aes(m))
# Visualize changepoints
ga_cp = get_cp(ga)
ggplot(ga_cp) + geom_histogram(aes(cp))


## Simulation study results: sg
sg = read_csv('data/sim_study/2_changepoints/sg/results_sg_2_v01_i_1_1000_0.01_150_45')
# Proportion where m==m_true
sg_correct = prop_correct(sg,m_true)
ggplot(sg)+geom_histogram(aes(m))
# Visualize changepoints
sg_cp = get_cp(sg)
ggplot(sg_cp) + geom_histogram(aes(cp))


## Simulation study results: dp
dp = read_csv('data/sim_study/2_changepoints/dp/results_dp_2_v01_i_1_1000_0.01_150_45')
# Proportion where m==m_true
dp_correct = prop_correct(dp,m_true)
ggplot(dp)+geom_histogram(aes(m))
# Visualize changepoints
dp_cp = get_cp(dp)
ggplot(dp_cp) + geom_histogram(aes(cp))


print(paste('Proportion ga, where m==m_true: ',ga_correct))
print(paste('Proportion dp, where m==m_true: ',dp_correct))
print(paste('Proportion sg, where m==m_true: ',sg_correct))
