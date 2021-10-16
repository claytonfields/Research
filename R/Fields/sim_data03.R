library(tidyverse)



## Dataset 4
# Linear, 4 changepoints
# Parameters
n = 250
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
X = rnorm(n,50,16)
X=sort(X)
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