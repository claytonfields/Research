library(tidyverse)
library(tidycensus)
library(readxl)
library(AER)
library(loon)
library(loon.data)

## COVID DATA
data = read_csv('data/covid-6-01.csv')
density = read_excel('data/ozone-county-population.xlsx')

# density = density %>%
#   mutate(FIPS = paste(density$`STATE FIPS`, density$`COUNTY FIPS`,sep = '')) %>%
#   str_remove(.$FIPS, "^0+")
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


## Yet more data
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





# Census
census_api_key("67872e56f82b89b58ee22e59be2906219b221e0c")

income = get_acs(geography = "county", variables = c(medincome = "B19013_001"), year = 2014)

income = income %>% mutate(`State_FIPS_Code`= str_sub(.$GEOID,1,2),
                           `County_FIPS_Code` = str_sub(.$GEOID,3,5))

income_vital = inner_join(income,vitals, by = c("State_FIPS_Code","County_FIPS_Code"))
plot(income_vital$estimate, income_vital$Col_Cancer)



















