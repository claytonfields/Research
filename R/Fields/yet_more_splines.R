library(splines)
library(tidyverse)

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
ggplot(df2) + geom_point(aes(X,y), color='gray') + 
  geom_line(aes(X,y_true), color='red')


### Splines and the like
library(changepoint.np)
out <- cpt.np(y_true, penalty = "AIC",method="PELT",minseglen = 50,nquantiles =4*log(length(y_true)))
cpts(out)/10
plot(out)















