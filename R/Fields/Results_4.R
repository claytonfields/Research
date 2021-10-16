library(tidyverse)
library(aspline)
library(splines2)
library(dpseg)
library(segmented)
library(strucchange)

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

## Dataset 4
# Linear, 4 changepoints
# Parameters
n = 150
sigma = 3
m_true = 4
# Generate xi
xi_1 = 8
xi_2 = 21
xi_3 = 44
xi_4 = 60
xi = c(4,xi_1, xi_2, xi_3, xi_4)
# Generate X
X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X >= xi_1 & X < xi_2]
X3 = X[X >= xi_2 & X < xi_3]
X4 = X[X >= xi_3 & X < xi_4]
X5 = X[X >= xi_4]
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
ggsave('sim_study_data/examples/linear_4cpts.png')
write_csv(df0,'sim_study_data/examples/linear_4cpts.csv')


## Simulation study results: GA
# p.mut = .01
# max.itr = 150
# x.inc = 45
# ga = read_csv('sim_study_data/4_changepoints/150_05_45/ga/results_ga_4_v01_i_1_1000_0.01_150_45')
ga1 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_1_77_150')
ga2 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_78_155_150')
ga3 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_156_233_150')
ga4 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_234_311_150')
ga5 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_312_389_150')
ga6 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_390_467_150')
ga7 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_468_545_150')
ga8 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_546_623_150')
ga9 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_624_701_150')
ga10 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_702_779_150')
ga11 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_780_857_150')
ga12 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_858_935_150')
ga13 = read_csv('sim_study_data/4_changepoints/n_150/ga/results_ga_4_v01_i_936_1000_150')
ga = rbind(ga1,ga2,ga3,ga4,ga5,ga6,ga7,ga8,ga10,ga11,ga12,ga13)

# Proportion where m==m_true
ga_correct = prop_correct(ga,m_true)
ggplot(ga)+geom_histogram(aes(m))
# Visualize changepoints
ga_cp = get_cp(ga)
ggplot(ga_cp) + geom_histogram(aes(cp))
# Average value of m
mean(ga$m)



## Strucchange
sc1 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_1_77_150')
sc2 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_78_155_150')
sc3 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_156_233_150')
sc4 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_234_311_150')
sc5 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_312_389_150')
sc6 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_390_467_150')
sc7 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_468_545_150')
sc8 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_546_623_150')
sc9 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_624_701_150')
sc10 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_702_779_150')
sc11 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_780_857_150')
sc12 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_858_935_150')
sc13 = read_csv('sim_study_data/4_changepoints/n_150/sc/results_sc_4_v01_i_936_1000_150')
sc = rbind(sc1,sc2,sc3,sc4,sc5,sc6,sc7,sc8,sc10,sc11,sc12,sc13)

# Proportion where m==m_true
sc_correct = prop_correct(sc,m_true)
ggplot(sc)+geom_histogram(aes(m))
# Visualize changepoints
sc_cp = get_cp(sc)
ggplot(sc_cp) + geom_histogram(aes(cp))
# Average value of m
mean(sc$m)











# ## Simulation study results: ar
# ar = read_csv('data/sim_study_data/4_changepoints/150_05_45/ar/results_ar_4_v01_i_1_1000_0.01_150_45')
# # Proportion where m==m_true
# ar_correct = prop_correct(ar,m_true)
# ggplot(ar)+geom_histogram(aes(m))
# # Visualize changepoints
# ar_cp = get_cp(ar)
# ggplot(ar_cp) + geom_histogram(aes(cp))
# # Average value of m
# mean(ar$m)
# 
# ## Simulation study results: sg
# sg = read_csv('data/sim_study/4_changepoints/150_05_45/sg/results_sg_4_v01_i_1_1000_0.01_150_45')
# sg_correct = prop_correct(sg,m_true)
# mean(sg$m)
# 
# ## Simulation study results: dp
# dp = read_csv('data/sim_study/4_changepoints/150_05_45/dp/results_dp_4_v01_i_1_1000_0.01_150_45')
# dp_correct = prop_correct(dp,m_true)
# mean(dp$m)
# 
# 
# print(paste('Proportion ga, where m==m_true: ',ga_correct))
# print(paste('Proportion ar, where m==m_true: ',ar_correct))
# print(paste('Proportion dp, where m==m_true: ',dp_correct))
# print(paste('Proportion sg, where m==m_true: ',sg_correct))
# 
# 
# 
# # strucchange
# temp = breakpoints(y~X,h=100,data = df0)
# bp_sc = temp$breakpoints
# m_sc = length(bp_sc) 
# print(bp_sc/10)


