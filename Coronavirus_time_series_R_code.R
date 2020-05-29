###Coronavirus Time Series Data Wrangling
#The purpose of this file is load the COVID-19 time series data from
#Johns Hopkins and wrangle the data into a more manageable form. This will allow
#the calculation of simple stats like daily increases, etc., by country or U.S.
#state and the creation of plots of the curves for cases, deaths etc.

###Loading the data - assumes the data have been cloned from the Johns Hopkins
###website's github (https://github.com/CSSEGISandData/COVID-19.git) and that
###the working directory has been set to the master branch of the cloned git
setwd("csse_covid_19_data/csse_covid_19_time_series")
us_cases_ts <- read.csv("time_series_covid19_confirmed_US.csv")
us_deaths_ts <- read.csv("time_series_covid19_deaths_us.csv")
global_cases_ts <- read.csv("time_series_covid19_confirmed_global.csv")
global_deaths_ts <- read.csv("time_series_covid19_deaths_global.csv")