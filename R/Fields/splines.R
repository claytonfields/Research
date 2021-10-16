library(ISLR)
library(splines)
library(tidyverse)
library(modelr)
library(earth)   




### Simulated Data
## True data

f = function(X){
     .0035*X^8  + .001*X^6+ .02*X^5 -.4*X^4 + .168*X^3 + .3*X^2 + X + 1 - .75*cos(4*X) + .9*sin(2*X)
}
set.seed(2244)
n = 200
X = rnorm(n, sd=1)
eps = rnorm(n,sd=.8)
y = f(X) + eps
y_true = f(X)
ggplot() + geom_point(aes(X,y)) + geom_line(aes(X,y_true), color = 'blue')

df = data.frame(X,y)
X_lims = range(df$X)
X_grid = seq(X_lims[1], X_lims[2], length.out = 50)

mod = lm(y ~ ns(X, df=5), data = df)
pred = predict(mod, newdata = list(X = X_grid) )

ggplot() + geom_point(aes(X,y), color='gray') + geom_line(aes(X_grid,pred), color = 'blue') +
  geom_line(aes(X,y_true), color='red') 

df = df %>% add_predictions(mod)

ggplot(aes(X),data = df) + geom_point(aes(y=y), show.legend = T) + geom_smooth(aes(y=pred), show.legend = T)

## County Data: Linear
vitals = read_csv('data/MEASURESOFBIRTHANDDEATH.csv')
risk = read_csv('data/RISKFACTORSANDACCESSTOCARE.csv')

x = vitals %>% filter(Col_Cancer >=0) %>%
  select(`State_FIPS_Code`, `County_FIPS_Code`,
         `CHSI_County_Name`, `Col_Cancer`)
y = filter(risk, risk$Obesity>=0) %>%
  select(`State_FIPS_Code`, County_FIPS_Code,
         CHSI_County_Name, Obesity)

vital_risk = inner_join(x,y, by=c("State_FIPS_Code","County_FIPS_Code"))
plot(vital_risk$Obesity, vital_risk$Col_Cancer)

xlims=range(vital_risk$Obesity)
x.grid=seq(from=xlims[1],to=xlims[2])

ctymod = lm(Col_Cancer ~ ns(Obesity,df=3), data=vital_risk)
pred=predict(ctymod,newdata=list(Obesity=x.grid),se=T)

plot(vital_risk$Obesity, vital_risk$Col_Cancer, col='gray')
lines(x.grid,pred$fit,lwd=2)
lines(x.grid,pred$fit+2*pred$se,lty="dashed")
lines(x.grid,pred$fit-2*pred$se,lty="dashed")


## Mcycle
library(MASS)
data(mcycle)
plot(mcycle$times, mcycle$accel)

# X grids
timelims=range(mcycle$times)
time.grid=seq(from=timelims[1],to=timelims[2])

# Fit B Spline
mod = lm(accel ~ bs(times, knots=c(20,33)), data = mcycle)
mcycle = mcycle %>% add_predictions(mod)

ggplot(mcycle, aes(times)) + geom_point(aes(y=accel), color='gray') + 
  geom_line(aes(y=pred), color='blue')+
  ggtitle('Motorcycle Data: Times vs. Acceleration: B Spline')
  
  
  
ggplot() + geom_point(mcycle, mapping=aes(x=times, y = accel), color='gray') + 
  geom_line(mapping = aes(x=time.grid,y=pred$fit))+ 
  geom_line(aes(x=time.grid,y=pred$fit+2*pred$se), linetype='dashed') +
  geom_line(aes(x=time.grid,y=pred$fit-2*pred$se), linetype='dashed') +
  ggtitle('Motorcycle Data: Times vs. Acceleration: B Spline')




# Fit Natural Spline
mcmod = lm(accel ~ ns(times,df=5), data = mcycle)
pred=predict(mcmod,newdata=list(times=time.grid),se=T)


mcycle = mcycle %>% add_predictions(mcmod)

ggplot(mcycle, aes(times)) + geom_point(aes(y=accel), color='gray') + 
  geom_line(aes(y=pred), color='blue')

ggplot() + geom_point(mcycle, mapping=aes(x=times, y = accel), color='gray') + 
  geom_line(mapping = aes(x=time.grid,y=pred$fit))+ 
  geom_line(aes(x=time.grid,y=pred$fit+2*pred$se), linetype='dashed') +
  geom_line(aes(x=time.grid,y=pred$fit-2*pred$se), linetype='dashed') +
  ggtitle('Motorcycle Data: Times vs. Acceleration: Natural Spline')

# Fit MARS model
mcmars = earth(accel ~ times, data= mcycle)
pred = predict(mcmars, newdata = list(times=time.grid))
ggplot() + geom_point(data=triceps, mapping=aes(x=age,y=triceps, color='gray')) +
  geom_line(aes(x=time.grid,y=pred))



## Triceps Data
# Library for Triceps data
library(MultiKink)
data("triceps")

# Plot data
ggplot(triceps) + geom_point(aes(age, triceps))

# Fit b-spline
mod = lm(triceps~bs(age,knots=c(4,35)),data=triceps)
triceps = triceps %>% add_predictions(mod)

ggplot(triceps) + geom_point(aes(age, triceps), color='gray') + 
  geom_line(aes(age,pred))





# Fit natural spline
mod = lm(triceps ~ ns(age,df=3), data = triceps)
pred=predict(mod,newdata=list(age=age_grid),se=T)

ggplot() + geom_point(data=triceps, mapping=aes(x=age,y=triceps, color='gray')) +
  geom_line(aes(x=age_grid,y=pred$fit)) + 
  geom_line(aes(x=age_grid,y=pred$fit+2*pred$se), linetype='dashed') +
  geom_line(aes(x=age_grid,y=pred$fit-2*pred$se), linetype='dashed') 

triceps = triceps %>% add_predictions(mod)

ggplot(triceps,aes(x=age)) + geom_point(aes(y=triceps), color='gray') +
  geom_line(aes(y=pred))


# Fit MARS spline
mars = earth(triceps ~ age,  data = triceps)
pred=predict(mars,newdata=list(age=age_grid),se=T)

ggplot() + geom_point(data=triceps, mapping=aes(x=age,y=triceps, color='gray')) +
  geom_line(aes(x=age_grid,y=pred))



## Bone Density Dataset
data(bone, package = "loon.data")
f_bone = bone %>% filter(.$sex =='female')
m_bone = bone %>% filter(.$sex =='male')
# plot age vs bd for both sexes
ggplot(bone) + geom_point(aes(x=age,y=rspnbmd), color = 'gray') + 
  ggtitle('Bone Density Data: Age vs Bone Density')
# with color
ggplot(bone) + geom_point(aes(x=age,y=rspnbmd, color=sex)) + 
  ggtitle('Bone Density Data: Age vs Bone Density')
# plot age vs bd for females
ggplot(f_bone) + geom_point(aes(x=age,y=rspnbmd)) + 
  ggtitle('Bone Density Data: Females: Age vs Bone Density')
# plot age vs bd for males
ggplot(m_bone) + geom_point(aes(x=age,y=rspnbmd)) + 
  ggtitle('Bone Density Data: Males: Age vs Bone Density')

## Fit Natural Spline
mod = lm(rspnbmd ~ ns(age, df=4), data = bone)
bone = bone %>% add_predictions(mod)
ggplot(data=bone, aes(x=age)) + geom_point(aes(y=rspnbmd), color='gray') + 
  geom_smooth(aes(y=pred),se = T)
# Females
f_mod = lm(rspnbmd ~ ns(age, df=4), data = f_bone)
f_bone = f_bone %>% add_predictions(f_mod)
ggplot(data=f_bone, aes(x=age)) + geom_point(aes(y=rspnbmd), color='gray') + 
  geom_smooth(aes(y=pred),se = T)
# Males
m_mod = lm(rspnbmd ~ ns(age, df=4), data = m_bone)
m_bone = m_bone %>% add_predictions(m_mod)
ggplot(data=m_bone, aes(x=age)) + geom_point(aes(y=rspnbmd), color='gray') + 
  geom_smooth(aes(y=pred),se = T)

ggplot() + geom_point(aes(x=age,y=rspnbmd), data=bone, color='gray') + 
#  geom_smooth(aes(age,pred), data= bone, color='black') +
  geom_smooth(aes(age,pred), data=f_bone, color='red') +
  geom_smooth(aes(age,pred), data=m_bone, color='blue')






### Diabetes Datset

# http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Diabetes.html
diabetes = read_csv('data/Diabetes2.csv')
# diabetes = filter(diabetes, diabetes$glyhb<=8)

# waist size vs glucose
ggplot()+ geom_point(aes(diabetes$waist, diabetes$stab.glu))

# wiast size vs glycolated hb
diabetes = diabetes %>% mutate(diabetic = glyhb>=7) %>%
  filter(!is.na(.$glyhb), !is.na(.$chol))

ggplot(diabetes)+geom_point(aes(glyhb, waist/hip, color=diabetic)) + 
  # coord_flip() + 
  ggtitle('Diabetes Data', subtitle = 'Waist/Hip ratio vs glyc. heomglobin')

## Fit Natural Spline waist ~ glyhb
# x gird
x_lims = range(diabetes$glyhb)
x_grid = seq(x_lims[1], x_lims[2])

mod = lm(waist ~ ns(glyhb,df=3), data=diabetes)
pred = predict(mod, newdata = list(glyhb = x_grid), se=T)
ggplot() + geom_point(aes(diabetes$glyhb, diabetes$waist), color='gray') + 
  geom_smooth(aes(x_grid,pred$fit))

## Fit B Spline Spline waist ~ glyhb
mod = lm(waist ~ bs(glyhb, knots = c(7)), data=diabetes)
pred = predict(mod, newdata = list(glyhb = x_grid), se=T)
ggplot() + geom_point(aes(diabetes$glyhb, diabetes$waist), color='gray') + 
  geom_smooth(aes(x_grid,pred$fit), se=T)

## Fit MARS spline Spline waist ~ glyhb
mod = earth(waist ~ glyhb, data=diabetes)
pred = predict(mod, newdata = list(glyhb = x_grid))
ggplot() + geom_point(aes(diabetes$glyhb, diabetes$waist), color='gray') + 
  geom_smooth(aes(x_grid,pred))

## Fit Natural Spline chol ~ glyhb
# x gird
x_lims = range(diabetes$glyhb)
x_grid = seq(x_lims[1], x_lims[2])

mod = lm(chol ~ ns(glyhb,df=3), data=diabetes)
pred = predict(mod, newdata = list(glyhb = x_grid), se=T)
ggplot() + geom_point(aes(diabetes$glyhb, diabetes$chol), color='gray') + 
  geom_smooth(aes(x_grid,pred$fit))

## Fit B Spline Spline chol ~ glyhb
mod = lm(chol ~ bs(glyhb,knots=c(7)), data=diabetes)
pred = predict(mod, newdata = list(glyhb = x_grid), se=T)
ggplot() + geom_point(aes(diabetes$glyhb, diabetes$chol), color='gray') + 
  geom_smooth(aes(x_grid,pred$fit))

## Fit MARS spline Spline chol ~ glyhb
mod = earth(chol ~ glyhb, data=diabetes)
pred = predict(mod, newdata = list(glyhb = x_grid))
ggplot() + geom_point(aes(diabetes$glyhb, diabetes$chol), color='gray') + 
  geom_smooth(aes(x_grid,pred))













