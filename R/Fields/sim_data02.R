##### Simulated Data For testing Spline Methods
library(tidyverse)
library(splines)
library(modelr)

#### Dataset 1
### Generate Piecewise X
xi_1 = -1
xi_2 = 0
xi_3 = 1.2
xi = c(xi_1, xi_2, xi_3)
X = seq(from=-3,to=3, length.out = 1000)
X1 = X[X < xi_1]
X2 = X[(X >= xi_1 & X < xi_2)]
X3 = X[ X >= xi_2 & X < xi_3]
X4 = X[X>=xi_3]

### Generate Piecewise y
y = c()
y1 = rep(-0.02,length(X1)) 
y2 = -.02*X2^2

y3 = .002*X3^3
y4 = -.001*X4^3 + .0053
y_true = c(y1,y2,y3,y4)

eps = rnorm(n=1000,sd=.0017)
y = y_true + eps
df1 = tibble(X,y,y_true)
ggplot(df1) + geom_point(aes(X,y), color='gray') + 
  geom_line(aes(X,y_true), color='red') 

### Fit Spline

## B Spline
mod = lm(y ~ bs(X), data = df1)
df1 = add_predictions(df1,mod)
# Automatic Knot Selection
ggplot(df1) + geom_point(aes(X,y),color='gray') + 
  geom_line(aes(X,pred),color='blue') + 
  geom_line(aes(X,y_true), color='red') +
  ggtitle('B-spline fit to simulated Data', subtitle = 'Automatic knot selection')
# Knots Provided
df1 = tibble(X,y, y_true)
mod = lm(y ~ bs(X, knots = xi),  data = df1) 
df1 = add_predictions(df1,mod)

ggplot(df1) + geom_point(aes(X,y),color='gray') + 
  geom_line(aes(X,pred),color='blue') + 
  geom_line(aes(X,y_true), color='red') +
  ggtitle('B-spline fit to simulated Data', subtitle = 'True knots provided')

## Natural Spline
df1 = tibble(X,y, y_true)
mod = lm(y ~ ns(X, df = 5), data = df1)
df1 = add_predictions(df1,mod)

ggplot(df1) + geom_point(aes(X,y),color='gray') + 
  geom_line(aes(X,pred),color='blue') + 
  geom_line(aes(X,y_true), color='red') +
  ggtitle('B-spline fit to simulated Data', subtitle = 'True knots provided')
