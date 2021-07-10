library(tidyverse)
library(tidycensus)
library(readxl)
library(AER)
library(loon)
library(loon.data)
library(MultiKink)


## Diabetes Data Set
# http://staff.pubhealth.ku.dk/~tag/Teaching/share/data/Diabetes.html
diabetes = read_csv('data/Diabetes2.csv')
diabetes = filter(diabetes, diabetes$glyhb<=8)

# waist size vs glucose
plot(diabetes$waist, diabetes$stab.glu)

# wiast size vs glycolated hb
plot(diabetes$waist/diabetes$hip, diabetes$glyhb)

# waist size vs glycolated hb
plot(diabetes$waist/diabetes$hip, diabetes$glyhb)

plot(diabetes$weight, diabetes$waist)

plot(diabetes$ratio, diabetes$glyhb)


## Bone Density Dataset
data(bone, package = "loon.data")
summary(bone)
summary(bone)

x <- bone$age
y <- bone$rspnbmd

plot(x,y)



## Tricep Measurement dataset
data("triceps")









