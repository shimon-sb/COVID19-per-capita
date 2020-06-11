###Coronavirus Time Series Data Wrangling
#The purpose of this file is load the COVID-19 time series data from
#Johns Hopkins and wrangle the data into a more manageable form. This will allow
#the calculation of simple stats like daily increases, etc., by country or U.S.
#state and the creation of plots of the curves for cases, deaths etc.

##Loading required libraries
library(dplyr)
library(stringr)

###Loading the data - assumes the data have been cloned from the Johns Hopkins
###website's github (https://github.com/CSSEGISandData/COVID-19.git) and that
###the working directory has been set to the master branch of the cloned git
setwd("csse_covid_19_data/csse_covid_19_time_series")
us_cases_ts <- read.csv("time_series_covid19_confirmed_US.csv")
us_deaths_ts <- read.csv("time_series_covid19_deaths_us.csv")
global_cases_ts <- read.csv("time_series_covid19_confirmed_global.csv")
global_deaths_ts <- read.csv("time_series_covid19_deaths_global.csv")

###Collapsing US data by state or province
#First removing unnecessary columns
drop <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Country_Region",
          "Lat", "Long_", "Combined_Key")
us_cases_ts <- us_cases_ts[, !(names(us_cases_ts) %in% drop)]
us_deaths_ts <- us_deaths_ts[, !(names(us_deaths_ts) %in% drop)]

#Then aggregating by state or province for all other columns
us_cases_ts <- us_cases_ts %>% group_by(., Province_State) %>%
  summarize_all(., sum) %>%
  ungroup(.)
us_deaths_ts <- us_deaths_ts %>% group_by(., Province_State) %>%
  summarize_all(., sum) %>%
  ungroup(.)
