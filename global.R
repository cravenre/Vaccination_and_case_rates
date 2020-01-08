library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(ggplot2)
library(shinythemes)

state_merged <- readRDS("data/state_merged.rds")

colnames(state_merged)

state_year <- unique(state_merged$year)
state_list <- sort(unique(state_merged$state))

global_merged <- readRDS("data/global_merged.rds")

global_year <- sort(unique(global_merged$year))
global_list <- sort(unique(global_merged$country_name))
