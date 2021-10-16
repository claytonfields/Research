library(gridExtra)
library(tidyverse)

ds0 = read_csv('sim_study_data/examples/linear_0cpts.csv')
ds1 = read_csv('sim_study_data/examples/linear_1cpts.csv')
ds2 = read_csv('sim_study_data/examples/linear_2cpts.csv')
ds3 = read_csv('sim_study_data/examples/linear_3cpts.csv')
ds4 = read_csv('sim_study_data/examples/linear_4cpts.csv')
ds5 = read_csv('sim_study_data/examples/linear_5cpts.csv')

ggp0 = ggplot(ds0) + geom_point(aes(X,y), color='gray') + geom_line(aes(X,y_true)) + ggtitle('0 Changepoints')
ggp1 = ggplot(ds1) + geom_point(aes(X,y), color='gray') + geom_line(aes(X,y_true)) + ggtitle('1 Changepoint')
grid.arrange(ggp0, ggp1, ncol = 2)   
ggsave('sim_study_data/examples/linear_0-1cpts.png')

ggp2 = ggplot(ds2) + geom_point(aes(X,y), color='gray') + geom_line(aes(X,y_true)) + ggtitle('2 Changepoints')
ggp3 = ggplot(ds3) + geom_point(aes(X,y), color='gray') + geom_line(aes(X,y_true)) + ggtitle('3 Changepoint')
grid.arrange(ggp2, ggp3, ncol = 2)   
ggsave('sim_study_data/examples/linear_2-3cpts.png')

ggp4 = ggplot(ds4) + geom_point(aes(X,y), color='gray') + geom_line(aes(X,y_true)) + ggtitle('4 Changepoints')
ggp5 = ggplot(ds5) + geom_point(aes(X,y), color='gray') + geom_line(aes(X,y_true)) + ggtitle('5 Changepoint')
grid.arrange(ggp4, ggp5, ncol = 2)   
ggsave('sim_study_data/examples/linear_4-5cpts.png')

f = function(X){
  return(2*X)
}
vf = Vectorize(f)
vf(1:3)






