library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(ggplot2)
library(shinythemes)

state_merged <- readRDS("data/state_merged.rds")

state_year <- sort(unique(state_merged$year))
state_list <- sort(unique(state_merged$state))
state_vars <- colnames(state_merged)[2:5]
names(state_vars) <- c("Year", "Vaccination Rate", "Measles Case Count", "Health Expenditures per capita ($)")
state_map_vars <- state_vars[2:4]

global_merged <- readRDS("data/global_merged.rds")

global_year <- sort(unique(global_merged$year))
global_list <- sort(unique(global_merged$country_name))
global_vars <- colnames(global_merged)[c(2:5,7)]
names(global_vars) <- c("Year", "Measles Case Count", "Vaccination Rate (2nd dose)",
                       "Vaccination Rate (1st dose)", "Health Expenditures per capita ($)")
global_map_vars <- global_vars[2:5]