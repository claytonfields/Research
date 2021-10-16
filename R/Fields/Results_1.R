library(tidyverse)

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


### Dataset 2
## Linear, 1 changepoints
# Parameters
n = 1000
sigma = 3
m_true = 1
#  Generate X
xi_1 = 32
xi = c(1,xi_1)
X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X >= xi_1]
# Generate y
y1 = rep(26.3, length(X1))
y2 = .82*X2
y_true = c(y1,y2)
eps = rnorm(n,0,3)
y = y_true + eps
# plot data
df0 = tibble(X,y,y_true)
ggplot(df0) + geom_point(aes(X,y), color='gray') + 
  geom_line(aes(X,y_true), color='red') + 
  ggtitle('Linear Model: 1 Changepoint')
ggsave('data/sim_study/examples/linear_1cpts.png')
# save data
write_csv(df0,'data/sim_study/examples/linear_1cpts.csv')


## Simulation study results: GA
# p.mut = .01
# max.itr = 150
# x.inc = 45
ga = read_csv('data/sim_study/1_changepoints/ga/results_ga_1_v01_i_1_1000_0.01_150_45')
# Proportion where m==m_true
ga_correct = prop_correct(ga,m_true)
ggplot(ga)+geom_histogram(aes(m))
# Visualize changepoints
ga_cp = get_cp(ga)
ggplot(ga_cp) + geom_histogram(aes(cp))
# write_csv(ga,'data/sim_study/1_changepoints/ga/results_ga_1_v01_i_1_1000_0.01_150_45')



## Simulation study results: dp
dp = read_csv('data/sim_study/1_changepoints/dp/results_dp_1_v01_i_1_1000_0.01_150_45')
# Proportion where m==m_true
dp_correct = prop_correct(dp,m_true)
ggplot(dp)+geom_histogram(aes(m))
# Visualize changepoints
dp_cp = get_cp(dp)
ggplot(dp_cp) + geom_histogram(aes(cp))
# write_csv(dp,'data/sim_study/1_changepoints/dp/results_dp_1_v01_i_1_1000_0.01_150_45')

## Simulation study results: sg
sg = read_csv('data/sim_study/1_changepoints/sg/results_sg_1_v01_i_1_1000_0.01_150_45')
# Proportion where m==m_true
sg_correct = prop_correct(sg,m_true)
ggplot(sg)+geom_histogram(aes(m))
# Visualize changepoints
sg_cp = get_cp(sg)
ggplot(sg_cp) + geom_histogram(aes(cp))


print(paste('Proportion ga, where m==m_true: ',ga_correct))
print(paste('Proportion dp, where m==m_true: ',dp_correct))
print(paste('Proportion sg, where m==m_true: ',sg_correct))



