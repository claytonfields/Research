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
n = 150
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
# ga = read_csv('data/sim_study/0_changepoints/ga/results_ga_0_v01_i_1_1000_0.01_150_45')
ga1 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_1_42_150_0.5.csv')
ga2 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_43_84_150_0.5.csv')
ga3 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_85_126_150_0.5.csv')
ga4 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_127_168_150_0.5.csv')
ga5 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_169_210_150_0.5.csv')
ga6 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_211_252_150_0.5.csv')
ga7 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_253_294_150_0.5.csv')
ga8 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_295_336_150_0.5.csv')
ga9 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_337_378_150_0.5.csv')
ga10 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_379_420_150_0.5.csv')
ga11 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_421_460_150_0.5.csv')
ga12 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_461_500_150_0.5.csv')
ga13 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_501_542_150_0.5.csv')
ga14 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_543_584_150_0.5.csv')
ga15 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_585_626_150_0.5.csv')
ga16 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_627_668_150_0.5.csv')
ga17 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_669_710_150_0.5.csv')
ga18 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_711_752_150_0.5.csv')
ga19 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_753_794_150_0.5.csv')
ga20 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_795_836_150_0.5.csv')
ga21 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_837_878_150_0.5.csv')
ga22 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_879_920_150_0.5.csv')
ga23 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_921_960_150_0.5.csv')
ga24 = read_csv('sim_study_data/1_changepoints/n_150/ga/results_ga_0_v01_i_961_1000_150_0.5.csv')
ga = rbind(ga1,ga2,ga3,ga4,ga5,ga6,ga7,ga8,ga9,ga10,ga11,ga12,ga13,ga14,ga15,ga16,ga17,ga18,ga19,ga20,ga21,ga22,ga23,ga24)

# Proportion where m==m_true
ga_correct = prop_correct(ga,m_true)
ggplot(ga)+geom_histogram(aes(m))
# Visualize changepoints
ga_cp = get_cp(ga)
ggplot(ga_cp) + geom_histogram(aes(cp))
# Average value of m
mean(ga$m)

num_0 = ga %>% filter(m==0) %>%nrow() 
num_1 = ga %>% filter(m==1) %>%nrow()  
num_2 = ga %>% filter(m==2) %>%nrow()  


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


