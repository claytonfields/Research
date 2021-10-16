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

#### Create Dataset 
##Linear, no changepoints
# Parameters
n = 1000
sigma = 3
m_true = 0
#  Generate X,y
X = seq(from=0, to=100, length.out = n)
y_true = 1.245*X
eps = rnorm(n,0,sigma)
y = y_true + eps
# Plot Data
df0 = tibble(X,y,y_true)
ggplot(df0) + geom_point(aes(X,y), color='gray') + 
  geom_line(aes(X,y_true), color='red') + 
  ggtitle('Linear Model: No Changepoints')
ggsave('data/sim_study/examples/linear_0cpts.png')
# save data
write_csv(df0,'data/sim_study/examples/linear_0cpts.csv')

## Simulation study results: GA
# p.mut = .01
# max.itr = 150
# x.inc = 45
ga = read_csv('data/sim_study/0_changepoints/ga/results_ga_0_v01_i_1_1000_0.01_150_45')

# Proportion where m==m_true
ga_correct = prop_correct(ga,m_true)

## Simulation study results: Segmented
sg = read_csv('data/sim_study/0_changepoints/sg/results_sg_0_v01_i_1_1000_0.01_150_45')

# Proportion where m==m_true
sg_correct = prop_correct(sg,m_true)
# Visualize changepoints
ggplot(sg)+geom_histogram(aes(m))
sg_cp = get_cp(sg)
ggplot(sg_cp) + geom_histogram(aes(cp))

## Simulation study results: dpseg
dp = read_csv('data/sim_study/0_changepoints/dp/results_dp_0_v01_i_1_1000_0.01_150_45')
# Proportion where m==m_true
dp_correct = prop_correct(dp,m_true)


print(paste('Proportion ga, where m==m_true: ',ga_correct))
print(paste('Proportion dp, where m==m_true: ',dp_correct))
print(paste('Proportion sg, where m==m_true: ',sg_correct))


