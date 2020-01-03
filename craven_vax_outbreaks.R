library(tidyverse)
library(ggplot2)
library(readxl)
library(rio)
library(plotly)

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

#Now merge the state measles cases and state vax dataframes
state_merged <- state_vax_mmr %>% 
  full_join(state_measles, by=c("state", "year"))

state_merged$measles_cases <- as.numeric(state_merged$measles_cases)

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

# Selecting down to vax rates for measles only
global_measlesvax <- global_vax %>% 
  filter(vaccine == "MCV2")

global_measlesvax <- global_measlesvax[,-c(1,24:47)]

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
global_measlesvax <- global_measlesvax %>%
  pivot_longer(cols=-c(1:3),names_to="year", values_to="vax_rate", values_drop_na=TRUE)

# Join the global vax and case data
global_merged <- global_measlescases %>% 
  full_join(global_measlesvax, by=c("iso3", "country", "year"))

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
