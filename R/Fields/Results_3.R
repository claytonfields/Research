library(tidyverse)
library(changepoint.np)
library(cpm)
library(strucchange)
library(dpseg)

#### Utility Functions
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
# Linear, 3 changepoints
# Parameters
n = 150
sigma = 3
m_true = 3
#  Generate X
xi_1 = 21
xi_2 = 70
xi_3 = 85
xi = c(3,xi_1, xi_2, xi_3)
X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X>= xi_1 & X < xi_2]
X3 = X[X >= xi_2 & X < xi_3]
X4 = X[X >= xi_3]
# Generate y
y1 = 2*X1
y2 = 69 - 1.3*X2
y3 = rep(-22, length(X3))
y4 = X4 - 107
y_true = c(y1,y2,y3,y4)
eps = rnorm(n,0,sigma)
y = y_true + eps
# Plot
df0 = tibble(X,y,y_true)
ggplot(df0) + geom_point(aes(X,y), color='gray') +
  geom_line(aes(X,y_true), color='red') + 
  ggtitle('Linear Model: 3 Changepoint')
ggsave('data/sim_study/examples/linear_3cpts.png')
write_csv(df0,'data/sim_study/examples/linear_3cpts.csv')
  


## Simulation study results: GA
# p.mut = .01
# max.itr = 150
# x.inc = 45
# ga = read_csv('sim_study_data/4_changepoints/150_05_45/ga/results_ga_4_v01_i_1_1000_0.01_150_45')
ga1 = read_csv('sim_study_data/3_changepoints/n_150/ga/results_ga_3_v01_i_1_84_150')
ga2 = read_csv('sim_study_data/3_changepoints/n_150/ga/results_ga_3_v01_i_85_168_150')
ga3 = read_csv('sim_study_data/3_changepoints/n_150/ga/results_ga_3_v01_i_169_253_150')
ga4 = read_csv('sim_study_data/3_changepoints/n_150/ga/results_ga_3_v01_i_254_338_150')
ga5 = read_csv('sim_study_data/3_changepoints/n_150/ga/results_ga_3_v01_i_339_423_150')
ga6 = read_csv('sim_study_data/3_changepoints/n_150/ga/results_ga_3_v01_i_424_508_150')
ga7 = read_csv('sim_study_data/3_changepoints/n_150/ga/results_ga_3_v01_i_509_593_150')
ga8 = read_csv('sim_study_data/3_changepoints/n_150/ga/results_ga_3_v01_i_594_678_150')
# ga9 = read_csv('sim_study_data/3_changepoints/n_150/ga/results_ga_3_v01_i_679_763_150')
ga10 = read_csv('sim_study_data/3_changepoints/n_150/ga/results_ga_3_v01_i_764_848_150')
ga11 = read_csv('sim_study_data/3_changepoints/n_150/ga/results_ga_3_v01_i_849_932_150')
ga12 = read_csv('sim_study_data/3_changepoints/n_150/ga/results_ga_3_v01_i_933_1000_150')

ga = rbind(ga1,ga2,ga3,ga4,ga5,ga6,ga7,ga8,ga10,ga11,ga12)

# Proportion where m==m_true
ga_correct = prop_correct(ga,m_true)
ggplot(ga)+geom_histogram(aes(m))
# Visualize changepoints
ga_cp = get_cp(ga)
ggplot(ga_cp) + geom_histogram(aes(cp))
# Average value of m
mean(ga$m)



## Strucchange
sc1 = read_csv('sim_study_data/3_changepoints/n_150/sc/results_sc_3_v01_i_1_84_150')
sc2 = read_csv('sim_study_data/3_changepoints/n_150/sc/results_sc_3_v01_i_85_168_150')
sc3 = read_csv('sim_study_data/3_changepoints/n_150/sc/results_sc_3_v01_i_169_253_150')
sc4 = read_csv('sim_study_data/3_changepoints/n_150/sc/results_sc_3_v01_i_254_338_150')
sc5 = read_csv('sim_study_data/3_changepoints/n_150/sc/results_sc_3_v01_i_339_423_150')
sc6 = read_csv('sim_study_data/3_changepoints/n_150/sc/results_sc_3_v01_i_424_508_150')
sc7 = read_csv('sim_study_data/3_changepoints/n_150/sc/results_sc_3_v01_i_509_593_150')
sc8 = read_csv('sim_study_data/3_changepoints/n_150/sc/results_sc_3_v01_i_594_678_150')
# sc9 = read_csv('sim_study_data/3_changepoints/n_150/sc/results_sc_3_v01_i_679_763_150')
sc10 = read_csv('sim_study_data/3_changepoints/n_150/sc/results_sc_3_v01_i_764_848_150')
sc11 = read_csv('sim_study_data/3_changepoints/n_150/sc/results_sc_3_v01_i_849_932_150')
sc12 = read_csv('sim_study_data/3_changepoints/n_150/sc/results_sc_3_v01_i_933_1000_150')

sc = rbind(sc1,sc2,sc3,sc4,sc5,sc6,sc7,sc8,sc10,sc11,sc12)

# Proportion where m==m_true
sc_correct = prop_correct(sc,m_true)
ggplot(sc)+geom_histogram(aes(m))
# Visualize changepoints
sc_cp = get_cp(sc)
ggplot(sc_cp) + geom_histogram(aes(cp))
# Average value of m
mean(sc$m)




# ## Simulation study results: ar
# ar = read_csv('data/sim_study/3_changepoints/ar/results_ar_3_v01_i_1_1000_0.01_150_45')
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
# sg = read_csv('data/sim_study/3_changepoints/sg/results_sg_3_v01_i_1_1000_0.01_150_45')
# # Proportion where m==m_true
# sg_correct = prop_correct(sg,m_true)
# mean(sg$m)
# 
# 
# # Simulation study results: dp
# dp = read_csv('data/sim_study/3_changepoints/dp/results_dp_3_v01_i_1_1000_0.01_150_45')
# # Proportion where m==m_true
# dp_correct = prop_correct(dp,m_true)
# mean(dp$m)
# 
# 
# 
print(paste('Proportion ga, where m==m_true: ',ga_correct))
print(paste('Proportion sc, where m==m_true: ',sc_correct))
# print(paste('Proportion ar, where m==m_true: ',ar_correct))
# print(paste('Proportion dp, where m==m_true: ',dp_correct))
# print(paste('Proportion sg, where m==m_true: ',sg_correct))
# 
