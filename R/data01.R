library(tidyverse)
library(tidycensus)
library(readxl)
library(AER)
library(loon)
library(loon.data)

## COVID DATA
data = read_csv('covid-6-01.csv')
density = read_excel('ozone-county-population.xlsx')

density$FIPS = paste(density$`STATE FIPS`, density$`COUNTY FIPS`,sep = '')
density$FIPS = str_remove(density$FIPS, "^0+")
density$density = density$`2015 POPULATION`/density$`LAND AREA (Sqare Miles)`
density = density[c(8,9)]

combined = merge(data, density, by='FIPS')
combined = combined[c(1:3,8,9,13,15)]

combined = combined[combined$Incidence_Rate != 0,]
combined = combined[combined$FIPS !=36061,]

plot(combined$density, combined$Incidence_Rate)
plot(combined$density, combined$Confirmed)
plot(combined$density, combined$Deaths)
plot(combined$Incidence_Rate, combined$Deaths)

plot(combined$Confirmed, combined$Deaths)
hist(combined$Confirmed)
hist(combined$Deaths)

## School Data
data('CASchools')

CASchools = mutate(CASchools, average = (math+read)/2)

plot(CASchools$lunch, CASchools$math)
plot(CASchools$lunch, CASchools$read)
plot(CASchools$lunch, CASchools$average)
hist(CASchools$average)
hist(CASchools$lunch)

plot(CASchools$calworks, CASchools$math)
plot(CASchools$calworks, CASchools$read)
plot(CASchools$calworks, CASchools$average)
hist(CASchools$average)
hist(CASchools$calworks)


##  Mortality Data
variables = load_variables(2014, "acs5", cache = TRUE)
income = get_acs(geography = "county", variables = c(medincome = "B19013_001"), year = 2014)


mort = read_csv('mort.csv')
cv = mort[mort$Category == "Chronic respiratory diseases",]
cv2014 = cv[,c(1,2,25)]
data(fips_codes)

income$FIPS = str_remove(income$GEOID, "^0+")
income_mort  = merge(income, cv2014, by='FIPS')
plot(income_mort$estimate,income_mort$`Mortality Rate, 2014*`)

# cats = unique(mort$Category)  

## Texas 
texas_mp25 = read_excel('2021 County Health Rankings Texas Data - v1.xlsx', sheet= 'Ranked Measure Data')
texas_mp25 = texas_mp25[,c(1,3,62)]
col_names = texas_mp25[1,]
texas_mp25 = texas_mp25[-1,]
rd = mort[mort$Category == "Chronic respiratory diseases",]

names(texas_mp25) = c('FIPS', 'County','PM2.5')
pm_vs_mort = merge(rd, texas_mp25, by='FIPS')
pm_vs_mort = pm_vs_mort[,c(1,2,25,32)]

plot(pm_vs_mort$PM2.5, pm_vs_mort$`Mortality Rate, 2014*`)




## Covid  Vaccine data
vac_data = read_csv('/Users/claytonfields/Downloads/COVID-19_Vaccinations_in_the_United_States_County.csv')
vac_data = filter(vac_data, Date=='07/04/2021')

com_data  = read_csv('covid_counties.csv')
com_data = filter(com_data, date=='2021-07-04')
com_data$FIPS = str_replace_all(com_data$geoid,"[^0-9]",'')

vaccom_data = merge(vac_data, com_data, by = 'FIPS')
# pct vaccinated vs average deaths in last 7 days
plot(vaccom_data$Series_Complete_Pop_Pct, vaccom_data$deaths_avg_per_100k)

# pct vaccinated vs average cases in last 7 days
plot(vaccom_data$Series_Complete_Pop_Pct, vaccom_data$cases_avg_per_100k)



## Particulate matter pollution vs asthma
ca_county_info = read_excel('ctyfactbook2020.xlsx')
names(ca_county_info) = unlist(ca_county_info[2,])
ca_county_info = ca_county_info[c(-1,-2),]
ca_county_info = ca_county_info[ca_county_info$State=='California',]
ca_county_info = ca_county_info[, c(1,2,3,9,10,11)]
ca_county_info$County = str_replace(ca_county_info$County,' County', '')

preventable = read_csv('rates-of-preventable-hospitalizations-for-selected-medical-conditions-by-county-lghc-indicator-6.csv')
prev_asthma = filter(preventable, Year==2014, PQIDescription=='COPD or Asthma in Older Adults (Age 40+)')


# PM2.5 vs preventable hospitalizations: California
combined = merge(ca_county_info,prev_asthma,by='County')
combined = filter(combined,combined$`PM2.5     Wtd AM (µg/m3)`!='ND',combined$`PM2.5     Wtd AM (µg/m3)`!='IN')
plot(combined$`PM2.5     Wtd AM (µg/m3)`,combined$RiskAdjRate_ICD9)


# asthma_deaths = read_csv('asthma-deaths-by-county.csv')
# asthma_deaths = filter(asthma_deaths, asthma_deaths$`AGE GROUP`=='All ages')
# asthma_deaths = drop_na(asthma_deaths)

prevalence = read_excel('asthma-prevalence-3.xlsx')
prevalence = filter(prevalence, prevalence$STRATA=='Total population')
prevalence = prevalence[,c(1,5)]
names(prevalence)[1] = 'County'

combined1 = merge(ca_county_info,prevalence,by='County')
combined1 = filter(combined1,combined1$`PM2.5     Wtd AM (µg/m3)`!='ND',combined1$`PM2.5     Wtd AM (µg/m3)`!='IN')

# PM2.5 vs prevalence
plot(combined1$`PM2.5     Wtd AM (µg/m3)`,combined1$`CURRENT PREVALENCE`)

# PM10 vs prevalence
plot(combined1$`PM10        24-hr (µg/m3)`,combined1$`CURRENT PREVALENCE`)

# O3  vs precalence
plot(combined2$`O3            8-hr (ppm)`,combined2$`CURRENT PREVALENCE`)


## More pullution and asthma
city_asthma = read.csv('cbsa-asthma.csv', sep = '|',header = 0)
names(city_asthma)[1] = "Core Based Statistical Area (CBSA)"
city_pollution = read_excel('cbsafactbook2020.xlsx', skip = 2)

city_data = merge(city_asthma,city_pollution,by="Core Based Statistical Area (CBSA)")
city_data = filter(city_data, city_data$`PM2.5         Wtd AM (µg/m3)`!='ND', city_data$`PM2.5         Wtd AM (µg/m3)`!='IN')
plot(city_data$`PM2.5         Wtd AM (µg/m3)`,city_data$V3)

city_data = merge(city_asthma,city_pollution,by="Core Based Statistical Area (CBSA)")
city_data = filter(city_data, city_data$`PM10         24-hr (µg/m3)`!='ND', city_data$`PM10         24-hr (µg/m3)`!='IN')
plot(city_data$`PM10         24-hr (µg/m3)`,city_data$V3)

plot(city_data$`SO2         1-hr (ppb)`, city_data$V3)


## Diabetes
diabetes = read_csv('diabetes.csv')
plpl
# BMI vs Glucose
plot(diabetes$BMI, diabetes$Glucose)

# BMI vs Blood pressure
plot(diabetes$BMI, diabetes$BloodPressure)



## Heart Disease
heart = read_csv('heart.csv')
plot(heart$chol, heart$trtbps)


## 
biostat = read_csv('biostats.csv')
plot(biostat$Age, biostat$`Height (in)`)

## 
insurance = read_csv('insurance.csv')
# age vs charges
plot(insurance$age, insurance$charges)

# bmi vs charges
plot(insurance$bmi, insurance$charges)


diabetes2 = read_csv('Diabetes2.csv')
diabetes2 = filter(diabetes2, diabetes2$glyhb<=8)

# waist size vs glucose
plot(diabetes2$waist, diabetes2$stab.glu)

# wiast size vs glycolated hb
plot(diabetes2$waist/diabetes2$hip, diabetes2$glyhb)

# waist size vs glycolated hb
plot(diabetes2$waist/diabetes2$hip, diabetes2$glyhb)

plot(diabetes2$weight, diabetes2$waist)





data(bone, package = "loon.data")
summary(bone)
summary(bone)

x <- bone$age
y <- bone$rspnbmd

plot(x,y)




