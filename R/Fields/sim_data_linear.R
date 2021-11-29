##### Simulated Data For testing Spline Methods
library(tidyverse)
library(splines)
library(modelr)


#### Dataset 1
### Linear, no changepoints
##  Generate X
n = 1000
m_true = 0

X = seq(from=0, to=100, length.out = n)

y_true = 1.245*X
eps = rnorm(n,0,3)

y = y_true + eps


df2 = tibble(X,y,y_true)
ggplot(df2) + geom_point(aes(X,y), color='gray')  + 
  geom_line(aes(X,y_true), color = 'red')



# write_csv(path = 'sim_data_01.csv', x = df2)
# read_csv('sim_data_01.csv')

#### Dataset 2
### Linear, 1 changepoints
##  Generate X
xi_1 = 32
xi = c(1,xi_1)

n = 1000
X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X >= xi_1]

y1 = rep(26.3, length(X1))
y2 = .82*X2

y_true = c(y1,y2)
eps = rnorm(n,0,3)
y = y_true + eps


df2 = tibble(X,y,y_true)
ggplot(df2) + geom_point(aes(X,y), color='gray') 

#### Dataset 3
### Linear, 2 changepoints
##  Generate X
xi_1 = 26
xi_2 = 70

xi = c(2,xi_1, xi_2)

n = 1000
X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X >= xi_1 & X < xi_2]
X3 = X[X >= xi_2]

y1 = -.6*X1 + 67.4
y2 = 2*X2
y3 = -.4*X3+168


y_true = c(y1,y2,y3)
eps = rnorm(n,0,3)

y = y_true + eps


df2 = tibble(X,y,y_true)
ggplot(df2) + geom_point(aes(X,y), color='gray') 



#### Dataset 4
### Linear, 3 changepoints
##  Generate X
xi_1 = 21
xi_2 = 70
xi_3 = 85

xi = c(3,xi_1, xi_2, xi_3)

n = 1000
X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X >= xi_1 & X < xi_2]
X3 = X[X >= xi_2 & X < xi_3]
X4 = X[X >= xi_3]

y1 = 2*X1
y2 = 69 - 1.3*X2
y3 = rep(-22, length(X3))
y4 = X4 - 107


y_true = c(y1,y2,y3,y4)


eps = rnorm(n,0,3)
y = y_true + eps


df2 = tibble(X,y,y_true)
ggplot(df2) + geom_point(aes(X,y), color='gray') 







#### Dataset 5
###  Linear, 4 changepoints
##  Generate X
xi_1 = 8
xi_2 = 21
xi_3 = 44
xi_4 = 60
xi = c(xi_1, xi_2, xi_3, xi_4)

n = 1000
X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X >= xi_1 & X < xi_2]
X3 = X[X >= xi_2 & X < xi_3]
X4 = X[X >= xi_3 & X < xi_4]
X5 = X[X >= xi_4]

y1 = rep(16, length(X1))
y2 = 2*X2
y3 = rep(42, length(X3))
y4 = -X4 + 86
y5 = rep(26, length(X5))

y_true = c(y1,y2,y3,y4,y5)
eps = rnorm(n,0,3)

y = y_true + eps


df2 = tibble(X,y,y_true)
ggplot(df2) + geom_point(aes(X,y), color='gray') 

## Fit Natural Spline: Automatic Knot selection
mod = lm(y ~ ns(X, df=2), data = df2)
df2 %>% add_predictions( mod)  %>% 
  ggplot() + geom_point(aes(X,y), color='gray') + 
  geom_line(aes(X,pred), color='blue') + 
  geom_line(aes(X,y_true), color='red')

## Fit Natural Spline: Knots provided
mod = lm(y ~ ns(X, df=5), data = df2)
df2 %>% add_predictions( mod)  %>% 
  ggplot() + geom_point(aes(X,y), color='gray') + 
  geom_line(aes(X,pred), color='blue') + 
  geom_line(aes(X,y_true), color='red')

## Fit B-spline: Automatic Knot selection
# find a way to get knots from 
mod = lm(y ~ bs(X, degree=1,df=5), data = df2)
df2 %>% add_predictions(mod) %>%
  ggplot + geom_point(aes(X,y), color='gray') + 
  geom_line(aes(X,pred), color='blue') + 
  geom_line(aes(X,y_true), color='red')


## Fit B-spline: Correct Knots provided
est = as.numeric(dfB[1,4:7])
mod = lm(y ~ bs(X, degree=1, knots=est),data = df2)
df2 %>% add_predictions(mod) %>%
  ggplot + geom_point(aes(X,y), color='gray') + 
  geom_line(aes(X,pred), color='blue') + 
  geom_line(aes(X,y_true), color='red')





#### Dataset 6
###  Linear, 5 changepoints
n = 1000
seed = 2244
sigma = 3
m_true = 5
##  Generate X
xi_1 = 11
xi_2 = 21
xi_3 = 44
xi_4 = 60
xi_5 = 86
xi = c(m_true,xi_1, xi_2, xi_3, xi_4,xi_5)
# Generate X
X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X >= xi_1 & X < xi_2]
X3 = X[X >= xi_2 & X < xi_3]
X4 = X[X >= xi_3 & X < xi_4]
X5 = X[X >= xi_4 & X < xi_5]
X6 = X[X >= xi_5]
# Generate y
y1 = -0.4*X1 + 25
y2 = 1.2*X2 + 7.5
y3 = 2.7*X3 - 24
y4 = -X4 + 139
y5 = rep(26, length(X5)) + 53
y6 = 143.55 - .75*X6 
y_true = c(y1,y2,y3,y4,y5,y6)
eps = rnorm(n,0,sigma)
y = y_true + eps
# Plot 
df0 = tibble(X,y,y_true)
ggplot(df0) + geom_point(aes(X,y), color='gray') +
  geom_line(aes(X,y_true)) + 
  ggtitle('Linear Model: 5 Changepoints')
ggsave('sim_study_data/examples/linear_5cpts.png')
write_csv(df0,'sim_study_data/examples/linear_5cpts.csv')









