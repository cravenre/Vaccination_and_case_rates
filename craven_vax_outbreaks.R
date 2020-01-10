library(tidyverse)
library(ggplot2)
library(readxl)
library(rio)
library(plotly)
library(countrycode)

#Importing state vaccination data for analysis
state_vax_mmr <- read_csv("data/state_vax_data.csv", skip=2)
state_vax_mmr <- state_vax_mmr[c(1:52), c(1,2,8,14,20,26,32,38,44,50,56)]

#Rename state_vax_mmr columns for easy use
colnames(state_vax_mmr) <- c("state", "2009", "2010", "2011", "2012", "2013", "2014",
                             "2015", "2016", "2017", "2018")

#Pivot state_vax_mmr so each year/state is a row
state_vax_mmr <- state_vax_mmr %>%
  gather(key=year, value=vax_rate, -state) %>% 
  na.omit()

# state_mmr data prep for shiny app
saveRDS(state_vax_mmr, file="data/state_vax_mmr.rds")

# Graph of vax rates for app testing
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = FALSE,
  lakecolor = toRGB('white')
)
maptest <- state_vax_mmr %>% 
  filter(year == 2009)

# Importing state and map to overlay onto
names(state.abb) <- state.name
maptest$abbrev <- state.abb[maptest$state]

plot_geo(maptest, locationmode='USA-states') %>%
  add_trace(z=~vax_rate, locations = ~abbrev, color= ~vax_rate, colors='Greens') %>%
  layout(
    title = "MMR Vaccination Rates",
    geo =g)

# Importing files for state measles case data
#Importing 2018 measles cases
state_measles <- read_csv("data/2018state_measles.csv", skip=13)

state_measles <- state_measles[-c(1,2,9,14,20,28,38,43,48,57,63:179),c(1,7)]

colnames(state_measles) <- c("state", "2018")

#Importing 2017 measles cases
state_measles_2017 <- read_csv("data/2017state_measles.csv", skip=13)

state_measles_2017 <- state_measles_2017[-c(1,2,9,14,20,28,38,43,48,57,63:124),c(1,7)]

colnames(state_measles_2017) <- c("state", "2017")

#Importing 2016 measles cases
state_measles_2016 <- read_csv("data/2016state_measles.csv", skip=13)

state_measles_2016 <- state_measles_2016[-c(1,2,9,14,20,28,38,43,48,57,63:130),c(1,7)]

colnames(state_measles_2016) <- c("state", "2016")

#Merging the state measles case data
state_measles <- state_measles %>% 
  inner_join(state_measles_2017) %>% 
  inner_join(state_measles_2016)

#Converting all the dashes to 0 for cases
state_measles <- as.data.frame(lapply(state_measles, function(y) gsub("—", "0", y)))

#Combining the new york state and city cases into one row
new_york <- data.frame("New York", "187", "4", "1")
names(new_york) <- c("state", "X2018", "X2017", "X2016")

state_measles <- rbind(state_measles, new_york)[-c(8,9),]

colnames(state_measles) <- c("state", "2018", "2017", "2016")

#Pivot state_measles so each year/state is a row
state_measles <- state_measles %>%
  gather(key=year, value=measles_cases, -state) %>% 
  na.omit()

# Importing the US health expenditure data and cleaning for merge
state_expenses <- import_list("data/US_health_expenditures.xlsx", setclass="tbl",
                              which="Table 11 Personal Health Care", rbind=TRUE)

state_expenses <- state_expenses[,-26]

names(state_expenses) <- unlist((state_expenses[1,]))

state_expenses <- state_expenses[-c(1:3,10,17,23,31,44,49,55,62),]

# Pivot the state expenses data for merge
state_expenses <- state_expenses %>% 
  pivot_longer(cols=-c(1),names_to="year", values_to="expenses_percapita", values_drop_na=TRUE)

colnames(state_expenses)[1] <- "state"

#Now merge the state measles cases and state vax dataframes
state_merged <- state_vax_mmr %>% 
  full_join(state_measles, by=c("state", "year"))

state_merged$measles_cases <- as.numeric(state_merged$measles_cases)

state_merged <- state_merged %>%
  full_join(state_expenses, by=c("state", "year"))

# Importing state and map to overlay onto
names(state.abb) <- state.name

state_merged$abbrev <- state.abb[state_merged$state]

# state_merged data prep for shiny app
saveRDS(state_merged, file="data/state_merged.rds")

# Now moving to the global view
# Importing the global vax rate data
global_vax <- import_list("data/2018vax_who.xlsx", setclass="tbl", rbind=TRUE)
global_vax <- global_vax %>% 
  drop_na(country)

# Selecting down to vax rates for 1 measles dose only
global_measlesvax1 <- global_vax %>% 
  filter(vaccine == "MCV1")

global_measlesvax1 <- global_measlesvax1[, -c(1,4,24:47)]

# Selecting down to vax rates for 2 measles doses only
global_measlesvax2 <- global_vax %>% 
  filter(vaccine == "MCV2")

global_measlesvax2 <- global_measlesvax2[,-c(1,4,24:47)]

# Importing the global measles case data
global_measlescases <- import_list("data/measlescasesbycountrybymonth.xls",
                                   setclass="tbl", rbind=TRUE)
global_measlescases <- global_measlescases[-c(1:5),-c(1:3)]

# Adding a case total column for each year and dropping each month
global_measlescases[,4:15] <- sapply(global_measlescases[,4:15], as.numeric)
global_measlescases[3] <- sapply(global_measlescases[3], as.character)

global_measlescases <- global_measlescases %>% 
  mutate(case_total = rowSums(.[4:15]))

global_measlescases <- global_measlescases[,-c(4:15)]

colnames(global_measlescases) <- c("iso3", "country", "year", "case_total")

# Pivot the global vax data so that it can be merged easily with case data
global_measlesvax2 <- global_measlesvax2 %>%
  pivot_longer(cols=-c(1:2),names_to="year", values_to="MCV2_rate", values_drop_na=TRUE)

global_measelesvax1[,3:21] <- sapply(global_measelesvax1[,3:21], as.numeric)

global_measlesvax1 <- global_measlesvax1 %>% 
  pivot_longer(cols=-c(1:2),names_to="year", values_to="MCV1_rate", values_drop_na=TRUE)

#Importing the global health expenditure data, brought in as per capita in $US
global_expenses <- import_list("data/WHO_health_expenditures.xlsx", setclass="tbl", rbind=TRUE)
global_expenses <- global_expenses[-1, -c(2,3)]

#Converting the expense columns to numeric
global_expenses[,2:19] <- sapply(global_expenses[,2:19], as.numeric)

#Find all global_expense country names not in the merged dataframe, then rename them
rename_countries <- global_expenses$Countries[!global_expenses$Countries %in% global_list]

global_expenses <- global_expenses %>% 
  mutate(Countries=str_replace(Countries, "United States of America", "United States")) %>% 
  mutate(Countries=str_replace(Countries, "Brunei Darussalam", "Brunei")) %>% 
  mutate(Countries=str_replace(Countries, "Republic of Korea", "South Korea")) %>% 
  mutate(Countries=str_replace(Countries, "Saint Kitts and Nevis", "St. Kitts & Nevis")) %>% 
  mutate(Countries=str_replace(Countries, "Antigua and Barbuda", "Antigua & Barbuda")) %>% 
  mutate(Countries=str_replace(Countries, "Venezuela \\(Bolivarian Republic of\\)", "Venezuela")) %>% 
  mutate(Countries=str_replace(Countries, "Czech Republic", "Czechia")) %>% 
  mutate(Countries=str_replace(Countries, "Saint Lucia", "St. Lucia")) %>% 
  mutate(Countries=str_replace(Countries, "Trinidad and Tobago", "Trinidad & Tobago")) %>% 
  mutate(Countries=str_replace(Countries, "The Republic of North Macedonia", "Macedonia")) %>% 
  mutate(Countries=str_replace(Countries, "Saint Vincent and the Grenadines", "St. Vincent & Grenadines")) %>% 
  mutate(Countries=str_replace(Countries, "Bosnia and Herzegovina", "Bosnia & Herzegovina")) %>% 
  mutate(Countries=str_replace(Countries, "Russian Federation", "Russia")) %>% 
  mutate(Countries=str_replace(Countries, "Eswatini", "Swaziland")) %>% 
  mutate(Countries=str_replace(Countries, "Cabo Verde Republic of", "Cape Verde")) %>% 
  mutate(Countries=str_replace(Countries, "Sao Tome and Principe", "São Tomé & Príncipe")) %>% 
  mutate(Countries=str_replace(Countries, "Bolivia Plurinational States of", "Bolivia")) %>% 
  mutate(Countries=str_replace(Countries, "Côte d'Ivoire", "Côte d’Ivoire")) %>% 
  mutate(Countries=str_replace(Countries, "Republic of Moldova", "Moldova")) %>% 
  mutate(Countries=str_replace(Countries, "Democratic Republic of the Congo", "Congo - Kinshasa")) %>% 
  mutate(Countries=str_replace(Countries, "Viet Nam", "Vietnam")) %>% 
  mutate(Countries=str_replace(Countries, "Congo$",  "Congo - Brazzaville")) %>% 
  mutate(Countries=str_replace(Countries, "Lao People's Democratic Republic", "Laos")) %>% 
  mutate(Countries=str_replace(Countries, "United Republic of Tanzania", "Tanzania")) %>% 
  mutate(Countries=str_replace(Countries, "Myanmar", "Myanmar (Burma)"))

# Pivot the global health expenses longer for merging with global data
global_expenses <- global_expenses %>% 
  pivot_longer(cols=-c(1),names_to="year", values_to="expenses_percapita", values_drop_na=TRUE)

# Join the global vax, case data, and global expenses
global_merged <- global_measlescases %>% 
  full_join(global_measlesvax2, by=c("iso3", "country", "year")) %>% 
  full_join(global_measlesvax1, by=c("iso3", "country", "year"))

# Converting all country names to the same standard
global_merged$country_name <- countrycode(global_merged$iso3, "iso3c", "country.name")

global_merged <- global_merged[, -2]

global_merged <- global_merged %>% 
  full_join(global_expenses, by=c("country_name" = "Countries", "year"))

# Exporting as an RDS file for shiny app
saveRDS(global_merged, file="data/global_merged.rds")

# Graph of global vax rates for app testing
g2 <- list(
  projection = list(type = 'Mercator')
  )

maptest_global <- global_merged %>% 
  filter(year == 2019)

plot_geo(maptest_global) %>%
  add_trace(z=~case_total, locations = ~iso3, color = ~case_total, colors='Greens') %>%
  layout(
    title = "Global Case Rate",
    geo =g2)

cor(x=state_merged$vax_rate, y=state_merged$expenses_percapita, use="pairwise.complete.obs")
