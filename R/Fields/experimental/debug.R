library(tidyverse)
### Load the proposed GA and fitted model likelihood funtion packages
WD.lib = c('Fields/experimental/')
WD.inp = c('data/')
source(file=paste(WD.lib,"ga_cont_02.R",sep=""))
source(file=paste(WD.lib,"mle_lin_02.R",sep=""))

### Data
##  Linear, 4 changepoints
# Parameters
n = 1000
sigma = 3
m_true = 4

# xi
xi_1 = 8
xi_2 = 21
xi_3 = 44
xi_4 = 60
xi = c(xi_1, xi_2, xi_3, xi_4)
cp = c(m_true, xi)

#  Generate X
X = seq(from=0, to=100, length.out = n)
X1 = X[X < xi_1]
X2 = X[X >= xi_1 & X < xi_2]
X3 = X[X >= xi_2 & X < xi_3]
X4 = X[X >= xi_3 & X < xi_4]
X5 = X[X >= xi_4]

#Generate y
y1 = rep(16, length(X1))
y2 = 2*X2
y3 = rep(42, length(X3))
y4 = -X4 + 86
y5 = rep(26, length(X5))
y_true = c(y1,y2,y3,y4,y5)
eps = rnorm(n,0,sigma)
y = y_true + eps


df2 = tibble(X,y,y_true)
ggplot(df2) + geom_point(aes(X,y), color='gray') +
  geom_line(aes(X,y_true)) + 
  ggtitle('Linear Model: 4 Changepoints')


### GA
# Parameters for GA
p.mut = .01
max.itr = 150


x.inc = 200
x.min = min(X)
x.max = max(X)

set.seed(2244)
# GA
ga.out = ga.cpt_Norm(y=y, x=X,fitness=pnllik.MDL.M0Z, p.mut=p.mut, x.inc=x.inc,
                     max.itr=max.itr,seed=2244, is.print = TRUE, is.graphic = FALSE)
chrom.sol = ga.out$solution
m = chrom.sol[1]

print(m)
print(chrom.sol)










