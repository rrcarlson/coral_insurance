##### Coral insurance WTP analysis
##### 29 June 2022
##### Author: Rachel Carlson, rrcarl@stanford.edu

# Creates summary statistics on coastal businesses in Oahu and Hawaii Island
# Cleans WTP dataset for use in analysis
# Analyzes drivers of WTP for coral insurance among coastal businesses

##### Load libraries
library(raster)
library(sf)
library(tidyverse)
library(ISLR2)
library(DCchoice)
library(Ecdat)
library(lmtest)

##### Read in data
wtp <- read.csv("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/business_wtp.csv")

##### Clean data
# Remove geospatial markers (not needed) and any other uneeded columns
wtp <- wtp %>% select(-c(address, lat_land, lon_land, lat_shore, lon_shore, loc_sea, lat_sea, lon_sea)) # Remove geospatial
wtp <- wtp %>% select(-email)

##### Summary statistics

##### WTP formulae
sum(!is.na(wtp$ans1)) # 15 businesses left WTP blank, so we have a sample size of n = 181 for this formula








