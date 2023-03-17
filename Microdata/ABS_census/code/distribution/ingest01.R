# Download ABS and save

# library --------------------------------------------------------------------------
library(dplyr)
library(sf)
library(httr)
library(sp)
library(data.table)
library(stringr)
library(tidyr)
library(readr)
library(tidyverse)
library(tidycensus)
library(tigris)
library(rjson)
library(jsonlite)
library(dplyr)
census_api_key(Sys.getenv('census_api_key'))
key <- Sys.getenv('census_api_key')


# load the annual business survey data
#data2020 <- jsonlite::fromJSON(paste0('https://api.census.gov/data/2020/abscbo?get=NAME,GEO_ID,NAICS2017_LABEL,OWNER_SEX,OWNER_ETH,OWNER_RACE,OWNER_VET,OWNPDEMP&for=metropolitan%20statistical%20area/micropolitan%20statistical%20area:10780&OWNCHAR=CG&NAICS2017=*&key=',key))
#data2019 <- jsonlite::fromJSON(paste0('https://api.census.gov/data/2020/abscbo?get=NAME,GEO_ID,NAICS2017_LABEL,OWNER_SEX,OWNER_ETH,OWNER_RACE,OWNER_VET,OWNPDEMP&for=metropolitan%20statistical%20area/micropolitan%20statistical%20area:10780&OWNCHAR=CG&NAICS2017=*&key=',key))
#data2018 <- jsonlite::fromJSON(paste0('https://api.census.gov/data/2020/abscbo?get=NAME,GEO_ID,NAICS2017_LABEL,OWNER_SEX,OWNER_ETH,OWNER_RACE,OWNER_VET,OWNPDEMP&for=metropolitan%20statistical%20area/micropolitan%20statistical%20area:10780&OWNCHAR=CG&NAICS2017=*&key=',key))
#data2017 <- jsonlite::fromJSON(paste0('https://api.census.gov/data/2020/abscbo?get=NAME,GEO_ID,NAICS2017_LABEL,OWNER_SEX,OWNER_ETH,OWNER_RACE,OWNER_VET,OWNPDEMP&for=metropolitan%20statistical%20area/micropolitan%20statistical%20area:10780&OWNCHAR=CG&NAICS2017=*&key=',key))


test <- jsonlite::fromJSON(paste0('https://api.census.gov/data/2017/abscb?get=NAME,GEO_ID,NAICS2017_LABEL,SEX,ETH_GROUP,RACE_GROUP,VET_GROUP,FIRMPDEMP&for=place:07000&in=state:01&NAICS2017=00&BUSCHAR=A1&QDESC=B01&key=',key))


test <- read_csv(paste0('https://api.census.gov/data/2017/abscb?get=NAME,GEO_ID,NAICS2017_LABEL,SEX,ETH_GROUP,RACE_GROUP,VET_GROUP,FIRMPDEMP&for=place:*&in=state:01&NAICS2017=00&BUSCHAR=A1&QDESC=B01&key=',key))

data <- as.data.frame(data)









#mi_bg <- read_csv('Business_characteristics/Total/data/distribution/va059_bg_mi_20102020_number_business.csv.xz')
#mi_industry_bg <- read_csv('Business_characteristics/Industry/data/distribution/va059_bg_mi_20102020_number_business_by_industry.csv.xz')
#temp01 <- mi_industry_bg %>% filter(year %in% periods) %>% group_by(year,measure) %>% summarize(value=sum(value, na.rm=T))
#temp02 <- mi_bg %>% filter(year %in% periods) %>% group_by(year,measure) %>% summarize(value=sum(value, na.rm=T))



# 1. upload mergent intellect ----------------------------------------------------------------------------
mi_minority_bg <- read_csv('Business_characteristics/Minority_owned/data/distribution/va059_ct_mi_20102020_minority_industry_profile.csv.xz')

# set the frame for benchmark
periods <- c(2017,2018,2019,2020)
metrics1 <- c('total_business','new_business','exit_business','entry_rate','exit_rate')
group <- c('minority_owned','non_minority_owned')

# aggregate value at the county level
# 1. number of business
temp <- mi_minority_bg %>% filter(year %in% periods) %>% group_by(year,measure) %>% summarize(value=sum(value, na.rm=T))

temp_business <- temp %>%
  mutate(agg=str_remove_all(measure, paste(metrics1, collapse = "|")),
         industry=str_remove_all(agg, paste(group, collapse = "|")),
         status=str_remove_all(agg, paste(industry, collapse = "|")),
         metrics=str_remove_all(measure, paste(agg, collapse = "|")),
         industry=gsub("_", "", industry)
          ) %>%
  filter(metrics=='total_business') %>%
  select(year,industry,status,value)




# 2. analyze ABS data ----------------------------
names <- c('geo_name',
           'naics_code',
           'naics_name',
           'sex',
           'ethnicity',
           'race',
           'veteran_status',
           'year',
           'number_firms',
           'sales',
           'employment',
           'payroll',
           'sd_number_firms',
           'sd_sales',
           'sd_employment',
           'sd_payroll')
abs_census <- read_csv('Microdata/ABS_census/data/working/abs_business.csv')
colnames(abs_census) <- names

# total number of firms by industry and minority owned

industry <- abs_census %>%
  group_by(naics_code,naics_name) %>%
  mutate(employment=sum(employment),
         nb_business=sum(number_firms))

minority <- abs_census %>%
  filter(race %in% c("Minority","Nonminority","Total")) %>%
  mutate(employment=as.numeric(gsub(",","",employment)),
         number_firms=as.numeric(gsub(",","",number_firms))) %>%
  select(geo_name,year,race,number_firms,employment) %>%
  group_by(geo_name,year,race) %>%
  summarize(nb_employment=sum(employment, na.rm=T),
         nb_business=sum(number_firms, na.rm=T)) %>%
  pivot_wider(names_from=race, values_from=c('nb_business','nb_employment')) 


minority_by_industry <- abs_census %>%
  filter(race %in% c("Minority","Nonminority","Total")) %>%
  mutate(employment=as.numeric(gsub(",","",employment)),
         number_firms=as.numeric(gsub(",","",number_firms))) %>%
  select(geo_name,year,naics_code,naics_name,race,number_firms,employment) %>%
  group_by(geo_name,year,naics_code,naics_name,race) %>%
  summarize(nb_employment=sum(employment, na.rm=T),
            nb_business=sum(number_firms, na.rm=T)) %>%
  pivot_wider(names_from=race, values_from=c('nb_business','nb_employment')) 


