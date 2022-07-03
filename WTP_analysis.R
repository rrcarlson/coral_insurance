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
library(ResourceSelection)
library(bestglm)

##### Read in data
wtp <- read.csv("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/business_wtp.csv")

##### Clean data
# Remove geospatial markers (not needed) and any other uneeded columns
wtp <- wtp %>% select(-c(address, lat_land, lon_land, lat_shore, lon_shore, loc_sea, lat_sea, lon_sea)) # Remove geospatial
wtp <- wtp %>% select(-email)

# Some irregularities in "island" and other variables
unique(wtp$island)
wtp$island <- ifelse(wtp$island == "Hawaii\tNorth Kona","Hawaii", wtp$island) # Fix mistake
wtp$island <- ifelse(wtp$island == "", NA, wtp$island) # Translate blank to NA
wtp$prox <- ifelse(wtp$prox == "Close to (<1 mile)", "Close", wtp$prox)
wtp$dist <- ifelse(wtp$dist == "in person " | wtp$dist == "in", "in person", wtp$dist)
wtp$gender <- ifelse(wtp$gender == "", "Other", wtp$gender)
wtp$industry1 <- ifelse(wtp$industry1 == "Recreational surface", "Recreation surface", wtp$industry1)
wtp$CC <- ifelse(wtp$CC == 555, 3, wtp$CC) # Don't want to exclude "unsure" respondents from regression but this high value may skew effect size


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

# Convert response variables to factors
wtp_m1 <- wtp %>% filter(!is.na(BID1.25))
wtp_m1 <- wtp_m1 %>% filter(!is.na(size_rev))

wtp_m1$BID5 <- as.factor(wtp_m1$BID5)
wtp_m1$BID2.5 <- as.factor(wtp_m1$BID2.5)
wtp_m1$BID1.25 <- as.factor(wtp_m1$BID1.25)

##### Redefine dataset to include only the variables I am testing
# Redefine proximity as close or far
wtp$Close <- ifelse(wtp$prox == "Close" | wtp$prox == "Beachfront", 1, 0)
# Redefine industry as recreation "surface", recreation "subsurface" or land-based
wtp$industry2 <- ifelse((wtp$industry1 == "Retail" | wtp$industry1 == "Restaurant" | wtp$industry1 == "Lodging"), "Land", 
                           ifelse(wtp$industry1 == "Recreation surface", "Recreation surface",
                                  ifelse(wtp$industry1 == "Recreation subsurface", "Recreation subsurface", 0)))
# If I want to define "multi" island as inclusive of both Hawaii and Oahu, not a third category
wtp$islandOahu <- ifelse(wtp$island == "Oahu" | wtp$island == "Multi", 1, 0)

##### Dataset for model
wtp_ms <- wtp[,c("dist", "res", "identity", "age", "gender", "island", "Close", "CC", "industry2", "size_rev", "tenure", "econ_1", "econ_2.1","pro_nat1","pro_soc", "BID1.25", "BID2.5", "BID5")]
# wtp_ms$islandHawaii <- ifelse(wtp_ms$island == "Hawaii" | wtp_ms$island == "Multi", 1, 0)

wtp_ms <- na.omit(wtp_ms)
wtp_ms[sapply(wtp_ms, is.character)] <- lapply(wtp_ms[sapply(wtp_ms, is.character)], 
                                               as.factor)
# Rename outcome as "y" (required by "bestglm()" function)
wtp_ms_1.25 <- wtp_ms %>% select(-c(BID2.5, BID5)) %>% rename(y = BID1.25)
wtp_ms_2.5 <- wtp_ms %>% select(-c(BID1.25, BID5)) %>% rename(y = BID2.5)
wtp_ms_5 <- wtp_ms %>% select(-c(BID1.25, BID2.5)) %>% rename(y = BID5)


##### Try a preliminary model
glm.fits <- glm(BID5 ~ dist + res + gender + identity + age + island + CC + industry2 + Close + size_rev + pro_nat1 + econ_1 + econ_2.1 + pro_soc + tenure, 
                family = binomial,
                data = wtp_ms)

# Predict outcome based on model coefficients
glm.probs <- predict(glm.fits_5, type = "response")
glm.probs[1:10]
glm.pred <- rep(0, 174)
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, wtp_m1$BID2.5)

##### Best subset selection for WTP = 1/8 of 1%
best.logit <- bestglm(wtp_ms_1.25,
                      IC = "AIC",                 # Information criteria for
                      family=binomial,
                      method = "exhaustive")
summary(best.logit$BestModel) # AIC

# Best model is as follows:
glm.fits_1.25 <- glm(BID1.25 ~ identity + size_rev + tenure + pro_nat1, 
                family = binomial,
                data = wtp_ms)
summary(glm.fits_1.25)
hl_1.25 <- hoslem.test(glm.fits_1.25$y, fitted(glm.fits_1.25), g=10) # Hosmer and Lemeshow goodness of fit (GOF) test
hl_1.25 # p-value = 0.839, so there is no evidence of poor fit

##### Best subset selection for WTP = 1/4 of 1%
best.logit2 <- bestglm(wtp_ms_2.5,
                      IC = "AIC",                 # Information criteria for
                      family=binomial,
                      method = "exhaustive")
summary(best.logit2$BestModel)

# Best model is as follows:
glm.fits_2.5 <- glm(BID2.5 ~ identity + island + tenure + pro_nat1, 
                     family = binomial,
                     data = wtp_ms)
summary(glm.fits_2.5)
hl_2.5 <- hoslem.test(glm.fits_2.5$y, fitted(glm.fits_2.5), g=10) # Hosmer and Lemeshow goodness of fit (GOF) test
hl_2.5 # p-value = 0.5666, so there is no evidence of poor fit

##### Best subset selection for WTP = 1/2 of 1%
best.logit3 <- bestglm(wtp_ms_5,
                      IC = "AIC",                 # Information criteria for
                      family=binomial,
                      method = "exhaustive")
summary(best.logit3$BestModel)

# Best model is as follows:
glm.fits_5 <- glm(BID5 ~ identity + island + CC + tenure + econ_2.1 + pro_nat1 + pro_soc, 
                    family = binomial,
                    data = wtp_ms)
summary(glm.fits_5)
hl_5 <- hoslem.test(glm.fits_5$y, fitted(glm.fits_5), g=10) # Hosmer and Lemeshow goodness of fit (GOF) test
hl_5 # p-value = 0.1687, so there is no evidence of poor fit

##### Model for revealed preference behavior
wtp_rp <- wtp[,c("dist", "res", "identity", "age", "gender", "island", "Close", "CC", "industry2", "size_rev", "tenure", "econ_1", "econ_2.1","pro_nat1","pro_soc", "Revealed")]
wtp_rp <- na.omit(wtp_rp)
wtp_rp[sapply(wtp_rp, is.character)] <- lapply(wtp_rp[sapply(wtp_rp, is.character)], 
                                               as.factor)
glm.fits_rp <- glm(Revealed ~ dist + res + gender + identity + age + island + CC + industry2 + Close + size_rev + pro_nat1 + econ_1 + econ_2.1 + pro_soc + tenure, 
                family = binomial,
                data = wtp_rp)

# Best subset selection for revealed preference behavior
wtp_ms_rp <- wtp_rp %>% rename(y = Revealed)
best.logit_rp <- bestglm(wtp_rp,
                       IC = "AIC",                 # Information criteria for
                       family=binomial,
                       method = "exhaustive")
summary(best.logit_rp$BestModel)

# Best model is as follows:
glm.fits_rp <- glm(Revealed ~ age + island + pro_soc, 
                  family = binomial,
                  data = wtp_rp)
summary(glm.fits_rp)
hl_rp <- hoslem.test(glm.fits_rp$y, fitted(glm.fits_rp), g=10) # Hosmer and Lemeshow goodness of fit (GOF) test
hl_rp # p-value = 0.9951, so there is no evidence of poor fit

##### Descriptive statistics
wtp_raw <- read.csv("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/business_wtp.csv")

wtp_raw$island <- ifelse(wtp_raw$island == "Hawaii\tNorth Kona","Hawaii", wtp_raw$island) # Fix mistake
wtp_raw$island <- ifelse(wtp_raw$island == "", NA, wtp_raw$island) # Translate blank to NA
wtp_raw$prox <- ifelse(wtp_raw$prox == "Close to (<1 mile)", "Close", wtp_raw$prox)
wtp_raw$dist <- ifelse(wtp_raw$dist == "in person " | wtp_raw$dist == "in", "in person", wtp_raw$dist)
wtp_raw$gender <- ifelse(wtp_raw$gender == "", "Other", wtp_raw$gender)
wtp_raw$industry1 <- ifelse(wtp_raw$industry1 == "Recreational surface", "Recreation surface", wtp_raw$industry1)
wtp_raw$industry2 <- ifelse((wtp_raw$industry1 == "Retail" | wtp_raw$industry1 == "Restaurant" | wtp_raw$industry1 == "Lodging"), "Land", 
                        ifelse(wtp_raw$industry1 == "Recreation surface", "Recreation surface",
                               ifelse(wtp_raw$industry1 == "Recreation subsurface", "Recreation subsurface", 0)))

sum(wtp_raw$yy, na.rm = TRUE)

# Backwards selection
glm.fits <- glm(BID2.5 ~ dist + res + gender + identity + age + island + CC + industry2 + Close + size_rev + pro_nat1 + econ_1 + econ_2.1 + pro_soc + tenure, 
                family = binomial,
                data = wtp_ms)
deviance(glm.fits) # 192.1054

glm.fits2 <- glm(BID2.5 ~ dist + res + gender + identity + age + island + CC + industry2 + Close + size_rev + pro_nat1 + econ_1 + econ_2.1 + tenure, 
                family = binomial,
                data = wtp_ms) # Eliminated "pro_soc"
deviance(glm.fits2) # 192.21

glm.fits3 <- glm(BID2.5 ~ dist + res + gender + identity + island + CC + industry2 + Close + size_rev + pro_nat1 + econ_1 + econ_2.1 + tenure, 
                 family = binomial,
                 data = wtp_ms) # Eliminated "age"
deviance(glm.fits3) #192.293

glm.fits4 <- glm(BID2.5 ~ dist + res + gender + identity + island + CC + industry2 + Close + size_rev + pro_nat1 + econ_1 + tenure, 
                 family = binomial,
                 data = wtp_ms) # Eliminated "econ_2.1"
deviance(glm.fits4) #192.307

glm.fits4 <- glm(BID2.5 ~ dist + res + gender + identity + island + CC + industry2 + Close + size_rev + pro_nat1 + econ_1 + tenure, 
                 family = binomial,
                 data = wtp_ms)
deviance(glm.fits4)

