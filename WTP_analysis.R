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
library(psych)
library(lme4)

##### Read in data
wtp <- read.csv("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/business_wtp.csv")

##### Clean data
# Remove geospatial markers (not needed) and any other uneeded columns
wtp <- wtp %>% select(-c(address, lat_land, lon_land, lat_shore, lon_shore, loc_sea, lat_sea, lon_sea)) # Remove geospatial
wtp <- wtp %>% select(-email)

# Some irregularities in "island"
unique(wtp$island)
wtp$island <- ifelse(wtp$island == "Hawaii\tNorth Kona","Hawaii", wtp$island) # Fix mistake
wtp$island <- ifelse(wtp$island == "", NA, wtp$island) # Translate blank to NA

##### Summary statistics

##### WTP
sum(!is.na(wtp$ans1)) # 15 businesses left WTP blank, so we have a sample size of n = 181 for this formula
wtp <- wtp %>% filter(!is.na(BID1.25))

# Trim dataset to core variables in model
wtp_corr <- wtp
wtp_corr$island_num <- ifelse(wtp_corr$island == "Hawaii", 1, 0)
wtp_corr <- wtp_corr[,c("CC", "size_rev", "influence", "econ_1", "econ_2","pro_nat1","pro_soc","age","island_num")]
wtp_corr <- wtp_corr %>% filter(CC < 100) # "Unsure" for CC is 555, so filter this out

# Test for correlation
pairs(wtp_corr, pch = 19)

# Add correlation coefficients to plot
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(wtp_corr, lower.panel = panel.cor)

# Check out correlation ellipses
pairs.panels(wtp_corr, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# No problematic correlations detected

# Try a preliminary model
wtp_m1 <- wtp %>% filter(!is.na(size_rev))
glm.fits <- glm(BID1.25 ~ econ_1 + econ_2 + pro_soc + pro_nat1 + tenure + size_rev + identity + island, 
                family = binomial,
                data = wtp_m1)



