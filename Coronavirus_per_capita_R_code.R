##Coronavirus rolling per-capita updates file (an experiment)
#Read in wikipedia file on country populations
#install.packages("rvest")
library(rvest)
#library(xml2)
library(dplyr)
#library(stringr)

##Web-scraping of Wikipedia table performed following the instructions found at
##https://www.engineeringbigdata.com/web-scraping-wikipedia-world-population-rvest-r/,
##using the packages "rvest" and "xml2" and the magrittr pipe (here accessed
##using dplyr)
wiki_pop <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
wiki_pop <- read_html(wiki_pop)
global_pop <- wiki_pop %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table()
global_pop <- global_pop[[1]]

#Cleaning the global_pop table
names(global_pop) <- c("Rank", "Country", "Population", "Percent", "Date", "Source")
#Removing bracketed notes using the gsub function
global_pop$Country <- gsub('\\[[^]]*\\]', '', global_pop$Country)

##Collapsing provinces and territories into single figures per country
#Finds territories denoted by parenthese (e.g. "Puerto Rico (US)")
global_pop$parens <- gregexpr('\\(\\w+\\)', global_pop$Country)
global_pop$actual_texts <- regmatches(global_pop$Country, global_pop$parens)
global_pop$actual_texts <- sub('character\\(0\\)', '', global_pop$actual_texts)
#Generates a column with a country ID for combining rows
global_pop$country_pooled <- ifelse(global_pop$actual_texts == '',
                                    global_pop$Country,
                                    global_pop$actual_texts)
#Cleans the country_pooled variable
global_pop$country_pooled <- sub('\\(', '', global_pop$country_pooled)
global_pop$country_pooled <- sub('\\)', '', global_pop$country_pooled)
global_pop$country_pooled <- sub('US', 'United States', global_pop$country_pooled)
global_pop$country_pooled <- sub('UK', 'United Kingdom', global_pop$country_pooled)
global_pop$country_pooled <- sub('NZ', 'New Zealand', global_pop$country_pooled)
#Removes commas from Population and converts it to numeric
global_pop$Population <- gsub(',', '', global_pop$Population)
global_pop$Population <- as.numeric(global_pop$Population)
#Collapses population of territories into the total for the sovereign nation
pop_by_country <- global_pop %>% group_by(., country_pooled) %>%
  summarize(., Population = sum(Population)) %>%
  ungroup(.)
names(pop_by_country) <- c("Country", "Population")
#Renames some pesky countries to conform with the COVID-19 dataset
pop_by_country$Country <- sub("^Congo$", "Congo (Brazzaville)", pop_by_country$Country)
pop_by_country$Country <- sub("DR Congo", "Congo (Kinshasa)", pop_by_country$Country)


##Extracting the coronavirus cases by country from the Johns Hopkins site
#Johns Hopkins data can be accessed at https://github.com/CSSEGISandData/COVID-19
#For now, the data are stored in Data_science/COVID-19
setwd("C:/Users/shim/Desktop/Data_Science/COVID-19")
#Loading the data from 3/29/20
covid_19_data <- read.csv("csse_covid_19_data/csse_covid_19_daily_reports/03-29-2020.csv")
covid_19_by_country <- covid_19_data %>% group_by(., Country_Region) %>%
  summarize(., Cases = sum(Confirmed), Deaths = sum(Deaths)) %>%
  ungroup(.)
names(covid_19_by_country) = c("Country", "Cases", "Deaths")
#Fixing a few country names in the COVID-19 dataset
covid_19_by_country$Country <- sub('US', 'United States', covid_19_by_country$Country)
covid_19_by_country$Country <- sub("\\*", "", covid_19_by_country$Country)
covid_19_by_country$Country <- sub("Holy See", "Vatican City", covid_19_by_country$Country)
covid_19_by_country$Country <- sub("Korea, South", "South Korea", covid_19_by_country$Country)
covid_19_by_country$Country <- sub("Timor-Leste", "East Timor", covid_19_by_country$Country)
covid_19_by_country$Country <- sub("Czechia", "Czech Republic", covid_19_by_country$Country)
covid_19_by_country$Country <- sub("Cote d'Ivoire", "Ivory Coast", covid_19_by_country$Country)
covid_19_by_country$Country <- sub("Cabo Verde", "Cape Verde", covid_19_by_country$Country)

#Merging COVID-19 data with population data
global_merged <- merge(pop_by_country, covid_19_by_country, by = "Country", all = T)
global_merged <- na.omit(global_merged)
#Computing the per capita rates of incidence and death (and their inverses)
global_merged$Cases_pc <- global_merged$Cases / global_merged$Population
global_merged$Deaths_pc <- global_merged$Deaths / global_merged$Population
global_merged$Cases_1_per <- 1 / global_merged$Cases_pc
global_merged$Deaths_1_per <- 1 / global_merged$Deaths_pc

#Calculating rate of deaths per number of confirmed cases
global_merged$death_rate = global_merged$Deaths / global_merged$Cases

##Extracting population data for states in the US
wiki_pop_us <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population"
wiki_pop_us <- read_html(wiki_pop_us)
us_pop <- wiki_pop_us %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = T)
us_pop <- us_pop[[1]]
us_pop <- us_pop[, 3:4]
names(us_pop) <- c("State", "Population")
us_pop <- us_pop[2:57,]
us_pop$State <- sub("U.S. Virgin Islands", "Virgin Islands", us_pop$State)

#Prepping the U.S. states subset and merging with population by state
covid_19_us <- subset(covid_19_data, Country_Region == "US")
us_by_state <- covid_19_us %>% group_by(., Province_State) %>%
  summarize(., Cases = sum(Confirmed), Deaths = sum(Deaths)) %>%
  ungroup(.)
names(us_by_state) <- c("State", "Cases", "Deaths")
us_merged <- merge(us_pop, us_by_state, by = "State", all = T)
us_merged <- na.omit(us_merged)

us_merged$Population <- gsub(',', '', us_merged$Population)
us_merged$Population <- as.numeric(us_merged$Population)
us_merged$Cases <- as.numeric(us_merged$Cases)
us_merged$Cases_pc <- us_merged$Cases / us_merged$Population
us_merged$Cases_1_per <- 1 / us_merged$Cases_pc
us_merged$Deaths_pc <- us_merged$Deaths / us_merged$Population
us_merged$Deaths_1_per <- 1 / us_merged$Deaths_pc
us_merged$Death_rate <- us_merged$Deaths / us_merged$Cases