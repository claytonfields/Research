library(tidyverse)
library(tidycensus)
library(readxl)
library(AER)
library(loon)
library(loon.data)
library(MultiKink)


# ## Diabetes Data Set
# # http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Diabetes.html
diabetes = read_csv('data/Diabetes2.csv')
# diabetes = filter(diabetes, diabetes$glyhb<=8)
# 
# # waist size vs glucose
# ggplot()+ geom_point(aes(diabetes$waist, diabetes$stab.glu))
# 
# wiast size vs glycolated hb
diabetes = diabetes %>% mutate(diabetic = glyhb>=7)

ggplot(diabetes) + geom_point(aes(gly))

ggplot(diabetes)+geom_point(aes(waist/hip, glyhb, color=diabetic)) + 
  coord_flip() +
  ggtitle('Diabetes Data', subtitle = 'Waist/Hip ratio vs glyc. heomglobin')
 
# waist size vs glycolated hb
ggplot(diabetes)+geom_point(aes(glyhb, waist,color=diabetic)) +
  ggtitle('Diabetes Data', subtitle = 'Waist size vs glyc. heomglobin')

# glycolated hb vs cholesterol
ggplot(diabetes) + geom_point(aes(glyhb, chol, color=diabetic))

# glycolated hb vs weight
ggplot(diabetes)+geom_point(aes(glyhb, weight, color=diabetic))
 
# # wais size vs ratio
# ggplot()+geom_point(aes(diabetes$ratio, diabetes$glyhb))



## Bone Density Dataset
data(bone, package = "loon.data")
f_bone = bone %>% filter(.$sex =='female')
m_bone = bone %>% filter(.$sex =='male')
# plot age vs bd for both sexes
ggplot(bone)+geom_point(aes(age, rspnbmd)) + 
  ggtitle('Bone Density Data: Age vs Bone Density')
# group by sex
ggplot(bone)+geom_point(aes(age, rspnbmd, color=sex)) + 
  ggtitle('Bone Density Data: Age vs Bone Density')
# plot age vs bd for men
ggplot(bone)+geom_point(aes(age, rspnbmd)) + 
  ggtitle('Bone Density Data: Males: Age vs Bone Density')
## plot age vs bd for women
ggplot()+geom_point(aes(f_bone$age, f_bone$rspnbmd)) + 
  ggtitle('Bone Density Data: Females: Age vs Bone Density')



## Tricep Measurement dataset
data("triceps")
ggplot(triceps)+geom_point(aes(age, triceps)) + 
    ggtitle('Tricep Measurement Data', subtitle = 'Age vs. Triecp Measurement')


## Pima Diabetes
# pima = read_csv('data/diabetes 4.csv')
# plot(pima$BMI, pima$Glucose)
# plot(pima$SkinThickness, pima$Glucose)
# plot(pima$Age, pima$Glucose)


## Farmingham heart data
farmingham = read_csv('data/datasets_4123_6408_framingham.csv')
ggplot(farmingham) + geom_point(aes(glucose, BMI, color=TenYearCHD))



##Body fat
bodyfat = read_csv('data/bodyfat.csv')
ggplot(bodyfat)+ geom_point(aes(Abdomen/Hip, BodyFat))



## Boston Housing Prices
boston = read_csv('data/boston.csv')

# age vs median home value
ggplot(boston) + geom_point(aes(AGE, MEDV, color=LSTAT))

# dis vs nox conc.
boston %>% mutate(high = LSTAT>=8) %>%
  # filter(high==TRUE) %>%
  ggplot() + 
  geom_point(aes(DIS, NOX, color=high))

#dis vs median homd value
boston %>% mutate(high = LSTAT >=8) %>%
  # filter(.$high == TRUE) %>%
  ggplot() + 
  geom_point(aes(DIS,MEDV, color=high))

mod = with(boston, smooth.spline(DIS, MEDV, df=6))

boston_sub = boston %>% mutate(high = LSTAT >=8) %>%
  filter(.$high == TRUE) 
  
  ggplot() + 
  geom_point(aes(DIS,MEDV), data= boston_sub, color='gray') + 
  geom_line(aes(mod$x,mod$y), color='blue')

# Rooms per building vs median homve value
ggplot(boston) + geom_point(aes(RM, MEDV, color=PTRATIO))






## California Housing Prices
cali = read_csv('data/California_Houses.csv')
ggplot(cali) + geom_point(aes(Median_Income, Median_House_Value, color=Distance_to_coast))




## crime and housing price
crime = read_csv('data/merged_dataset.csv')
crime = crime %>% drop_na() %>% filter(Year > 2010)
ggplot(crime) + geom_point(aes(`Violent Crimes`, index_nsa))


## Wage data from ISLR
library(ISLR)
attach(Wage)
dataset('Wage')
wage = Wage
plot(wage$age, wage$wage)

## Dataset
# dataset = read_csv('data/dataset.csv')


## car insurance
# cars = read_csv('data/Car_Insurance_Claim.csv')


## Yet more data
vitals = read_csv('data/MEASURESOFBIRTHANDDEATH.csv')
risk = read_csv('data/RISKFACTORSANDACCESSTOCARE.csv')

x = filter(vitals, vitals$Col_Cancer >=0) %>%
          select( State_FIPS_Code, County_FIPS_Code,
                 CHSI_County_Name, Col_Cancer)
y = filter(risk, risk$Obesity>=0) %>%
          select(State_FIPS_Code, County_FIPS_Code,
                CHSI_County_Name, Obesity)

vital_risk = inner_join(x,y, by=c("State_FIPS_Code","County_FIPS_Code"))
ggplot(vital_risk) + geom_point(aes(Obesity, Col_Cancer))

## With census
census_api_key("67872e56f82b89b58ee22e59be2906219b221e0c")

income = get_acs(geography = "county", variables = c(medincome = "B19013_001"), year = 2014)

income = income %>% mutate(`State_FIPS_Code`= str_sub(.$GEOID,1,2),
                           `County_FIPS_Code` = str_sub(.$GEOID,3,5))

income_vital = inner_join(income,vitals, by = c("State_FIPS_Code","County_FIPS_Code"))
ggplot(income_vital) + geom_point(aes(estimate, Col_Cancer))



## Motercycle
library(MASS)
data(mcycle)
ggplot(mcycle) + geom_point(aes(times, accel))


# library(ISLR)
# dataset('boston')



## Blood pressure
hsd = read_csv('data/Health Screening Data.csv')
ggplot(hsd) + geom_point(aes(AgeinYr, BMI))

## Heart
heart = read_csv('data/heart.csv')
plot(heart$trtbps, heart$chol)

## Mastdocs
# mast = read_csv('data/masctodos.csv')


## Pima Diabetes set
pima = read_csv('data/diabetes.csv')

# . vs glucose
ggplot(pima) + geom_point(mapping = aes(x=SkinThickness, y=Glucose))
# glucose vs . , grouped by outcome
ggplot(pima) + geom_point(aes(Glucose, BloodPressure, color=Outcome))



### FEV
fev = read_csv('data/FEV.csv')

ggplot(fev) + geom_point(aes(height, fev, color=smoke))

filter(fev, sex=='female') %>%
  ggplot() + geom_point(aes(height,fev))


###
sa = read_csv('data/sa.csv')

sa %>% filter(tobacco>3) %>%
ggplot() + geom_point(aes(tobacco, obesity, color=chd))





### Concrete
concrete = read_excel('data/Concrete_Data.xls')

ggplot(concrete) + geom_point(aes(concrete$`Superplasticizer (component 5)(kg in a m^3 mixture)`, concrete$`Concrete compressive strength(MPa, megapascals)`))



### Communities and Crime
crime = read_csv('data/communities.data')


### Energy Efficiency Data
energy = read_excel('data/ENB2012_data.xlsx')

ggplot(energy) + geom_point(aes(x=X7,y=Y1))


### ImmunoTherapy Data
immuno = read_excel('data/Immunotherapy.xlsx')


### Real estate valuation
realestate = read_excel('data/Real estate valuation data set.xlsx')

ggplot(realestate) + geom_point(aes(realestate$`X2 house age`,realestate$`Y house price of unit area`))  
  
ggplot(realestate) + geom_point(aes(realestate$`X3 distance to the nearest MRT station`,realestate$`Y house price of unit area`))

ggplot(realestate) + geom_point(aes(realestate$`X4 number of convenience stores`, realestate$`Y house price of unit area`))


### Heart failure data
heartfail = read_csv('data/heart_failure_clinical_records_dataset.csv')

ggplot(heartfail) + geom_point(aes(heartfail$serum_sodium, ejection_fraction))

### Obesity
## https://archive.ics.uci.edu/ml/datasets/Estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition+
obesity = read_csv('data/ObesityDataSet_raw_and_data_sinthetic.csv')


### Abalone
## https://archive.ics.uci.edu/ml/datasets/Abalone
abalone = read_csv('data/abalone.data',col_names = FALSE)

ggplot(abalone) + geom_point(aes(X5, X9))


### Automobile
## https://archive.ics.uci.edu/ml/datasets/Automobile
autos = read_csv('data/imports-85.data', col_names = FALSE)
# col_names = read_csv('data/imports-85.names', col_names = FALSE)

ggplot(autos) + geom_point(aes(X22,X26))


### Thyroid
thyroid = read_csv('data/hypothyroid.data', col_names = FALSE)


### Forest Fires 
##https://archive.ics.uci.edu/ml/datasets/Forest+Fires
fire = read_csv('data/forestfires.csv')
ggplot(fire) + geom_point(aes(temp,area))

### Wine
## https://archive.ics.uci.edu/ml/datasets/Wine+Quality
wine = read_delim('data/winequality-red.csv',delim=';')
ggplot(wine) + geom_point(aes(`residual sugar`,quality))

### Yacht
## https://archive.ics.uci.edu/ml/datasets/Yacht+Hydrodynamics
yacht = read_delim('data/yacht_hydrodynamics.data', delim = ' ', col_names = FALSE)
ggplot(yacht) + geom_point(aes(X6,X7))












