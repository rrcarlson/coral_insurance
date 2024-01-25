##### Coral insurance WTP analysis
##### Start date of analysis: 29 June 2022
##### Author: Rachel Carlson, rrcarlson@ucdavis.edu

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
library(ggplot2)
library(ggthemes)
library(wesanderson)

##### Read in data
wtp <- read.csv("/Users/rachelcarlson/Documents/Research/PhD-2018-2022/Coral_Insurance/Data/business_wtp.csv")

##### Clean data
# Remove geospatial markers (not needed) and any other uneeded columns
wtp <- wtp %>% select(-c(address, lat_land, lon_land, lat_shore, lon_shore, loc_sea, lat_sea, lon_sea)) # Remove geospatial
wtp <- wtp %>% select(-email)

# Some typos in "island" and other variables
unique(wtp$island)
wtp$island <- ifelse(wtp$island == "Hawaii\tNorth Kona","Hawaii", wtp$island) # Fix mistake
wtp$island <- ifelse(wtp$island == "", NA, wtp$island) # Translate blank to NA
unique(wtp$prox)
wtp$prox <- ifelse(wtp$prox == "Close to (<1 mile)", "Close", wtp$prox)
unique(wtp$dist)
wtp$dist <- ifelse(wtp$dist == "in person " | wtp$dist == "in", "in person", wtp$dist)
unique(wtp$gender)
wtp$gender <- ifelse(wtp$gender == "", "Other", wtp$gender)
unique(wtp$industry1)
wtp$industry1 <- ifelse(wtp$industry1 == "Recreational surface", "Recreation surface", wtp$industry1)
wtp$CC <- ifelse(wtp$CC == 555, 3, wtp$CC) # Don't want to exclude "unsure" respondents from regression but this high value may skew effect size. Therefore, set "unsure" as 3 on 1-5 scale.
# There is a mistaken repeated visit to "Guava Shop". Delete response from "associate" and retain response from "general manager".
wtp <- wtp %>% filter(Name != "Guava Shop Haleiwa")
wtp <- wtp %>% filter(Name != "Kama'aina Kids Kayak and Snorkel Eco-ventures")

##### Summary statistics

##### WTP
sum(!is.na(wtp$ans1)) # Filter out businesses with NA data for WTP response
wtp <- wtp %>% filter(!is.na(BID1.25)) # BID1.25 is a column signifying y/n to 1/8 of 1% of annual revenue. Those with NA in 1/8 of 1% level have NA for WTP in general.

# Test Pearson's correlation between continuous covariates
wtp_corr <- wtp
wtp_corr$island_num <- ifelse(wtp_corr$island == "Hawaii", 1, 0)
wtp_corr <- wtp_corr[,c("CC", "size_rev", "influence", "econ_1", "econ_2","pro_nat1","pro_soc","age","island_num", "res")]

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

# No problematic correlations detected, all correlations r<0.7

# Define some variables for use in WTP model: 1) proximity redefined as binary "close" and "far" (close = on reef OR beachfront, far = inland); 2) industry redefined as recreation surface, recreation subsurface, and land (rather than subdividing land-based industries into retail, restaurants, etc.)
wtp$Close <- ifelse(wtp$prox == "Close" | wtp$prox == "Beachfront", 1, 0) # Redefine proximity as close or far
wtp$industry2 <- ifelse((wtp$industry1 == "Retail" | wtp$industry1 == "Restaurant" | wtp$industry1 == "Lodging"), "Land", 
                        ifelse(wtp$industry1 == "Recreation surface", "Recreation surface",
                               ifelse(wtp$industry1 == "Recreation subsurface", "Recreation subsurface", 0))) # Redefine industry as recreation "surface", recreation "subsurface" or land-based



# Check for correlations between categorical variables
wtp_corr2 <- wtp
wtp_corr2$island_num <- ifelse(wtp_corr2$island == "Hawaii", 1, 0)
wtp_corr2 <- wtp_corr2[,c("dist","identity","gender","island","Close","industry2","CC", "size_rev", "influence", "econ_1", "econ_2","pro_nat1","pro_soc","age","island_num", "res", "tenure")]
wtp_corr2 <- wtp_corr2 %>% filter(CC < 100) # "Unsure" for CC is 555, so filter this out

# Chi-2 between categorical variables
wtp_corr2_cat <- wtp_corr2 %>% select(c(dist, identity, gender, island, Close, industry2))
wtp_corr2_cat <- na.omit(wtp_corr2_cat)
wtp_corr2_cat[] <- lapply(wtp_corr2_cat, as.factor)
# Do the following for all variables
chisq.test(wtp_corr2_cat$dist, wtp_corr2_cat$industry2)
chisq.test(wtp_corr2_cat$identity, wtp_corr2_cat$industry2)
chisq.test(wtp_corr2_cat$gender, wtp_corr2_cat$industry2)
chisq.test(wtp_corr2_cat$island, wtp_corr2_cat$industry2)
chisq.test(wtp_corr2_cat$Close, wtp_corr2_cat$industry2)

# Convert categorical binary variables to 0, 1 to prepare for Point Biserial Correlation test
wtp_corr2$hawaiian <- ifelse(wtp_corr2$identity == "Native", 1, 0)
wtp_corr2$big <- ifelse(wtp_corr2$island == "Hawaii", 1, 0)
cor.test(wtp_corr2$pro_nat1, wtp_corr2$hawaiian) # Hawaiian identity of business owner is not correlated with pro_nat1 (p>0.05).
cor.test(wtp_corr2$res, wtp_corr2$hawaiian) # Hawaiian identity correlation with years on island r=0.29 (acceptable).
cor.test(wtp_corr2$tenure, wtp_corr2$hawaiian) # Hawaiian identity is not correlated with years with company (p>0.05).
cor.test(wtp_corr2$influence, wtp_corr2$big)  # Big island correlation with age of respondent r=0.21 (acceptable).



############### WTP Analysis

##### Dataset for model
wtp_ms <- wtp[,c("dist", "res", "identity", "age", "gender", "island", "Close", "CC", "industry2", "size_rev", "tenure", "econ_1", "econ_2.1","pro_nat1","pro_soc", "BID1.25", "BID2.5", "BID5")]

wtp_ms <- na.omit(wtp_ms) # n=171 businesses had no NA values in any covariate above (line 124)
wtp_ms[sapply(wtp_ms, is.character)] <- lapply(wtp_ms[sapply(wtp_ms, is.character)], 
                                               as.factor)
# Rename outcome as "y" (required by "bestglm()" function)
wtp_ms_1.25 <- wtp_ms %>% select(-c(BID2.5, BID5)) %>% rename(y = BID1.25) # select response variable as 1.25 (1/8 of 1% revenue) payment level
wtp_ms_2.5 <- wtp_ms %>% select(-c(BID1.25, BID5)) %>% rename(y = BID2.5) # select response variable as 2.5 (1/4 of 1% revenue) payment level
wtp_ms_5 <- wtp_ms %>% select(-c(BID1.25, BID2.5)) %>% rename(y = BID5) # select response variable as 5 (1/2 of 1% revenue) payment level


##### Preliminary model (all covariates included)
glm.fits <- glm(BID5 ~ dist + res + gender + identity + age + island + CC + industry2 + Close + size_rev + pro_nat1 + econ_1 + econ_2.1 + pro_soc + tenure, 
                family = binomial,
                data = wtp_ms)
# Reran the above with BID5 replaced with BID2.5, BID5 for 3 payment level models

# Predict outcome based on model coefficients
glm.probs <- predict(glm.fits_5, type = "response")
glm.probs[1:10]
glm.pred <- rep(0, 174)
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, wtp_m1$BID2.5)

##### Best subset selection for WTP = 1/8 of 1%
# First convert characters to factors since bestglm doesn't take characters.
col_names <- c("identity","island","industry2","gender","Close","dist")
wtp_ms_1.25[col_names] <- lapply(wtp_ms_1.25[col_names] , factor)

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
hl_1.25 # p-value = 0.913, so there is no evidence of poor fit

##### Best subset selection for WTP = 1/4 of 1%
# Again convert characters to factors since bestglm doesn't take characters.
wtp_ms_2.5[col_names] <- lapply(wtp_ms_2.5[col_names] , factor)

best.logit2 <- bestglm(wtp_ms_2.5,
                      IC = "AIC",                
                      family=binomial,
                      method = "exhaustive")
summary(best.logit2$BestModel)

# Best model is as follows:
glm.fits_2.5 <- glm(BID2.5 ~ identity + island + tenure + pro_nat1, 
                     family = binomial,
                     data = wtp_ms)
summary(glm.fits_2.5)
hl_2.5 <- hoslem.test(glm.fits_2.5$y, fitted(glm.fits_2.5), g=10) # Hosmer and Lemeshow goodness of fit (GOF) test
hl_2.5 # p-value = 0.752, so there is no evidence of poor fit

##### Best subset selection for WTP = 1/2 of 1%
# Again convert characters to factors since bestglm doesn't take characters.
wtp_ms_5[col_names] <- lapply(wtp_ms_5[col_names] , factor)

best.logit3 <- bestglm(wtp_ms_5,
                      IC = "AIC",                 
                      family=binomial,
                      method = "exhaustive")
summary(best.logit3$BestModel)

# Best model is as follows:
glm.fits_5 <- glm(BID5 ~ identity + island + CC + tenure + econ_2.1 + pro_nat1 + pro_soc, 
                    family = binomial,
                    data = wtp_ms)
summary(glm.fits_5)
hl_5 <- hoslem.test(glm.fits_5$y, fitted(glm.fits_5), g=10) # Hosmer and Lemeshow goodness of fit (GOF) test
hl_5 # p-value = 0.4926 so there is no evidence of poor fit

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
                       IC = "AIC",                
                       family=binomial,
                       method = "exhaustive")
summary(best.logit_rp$BestModel)

# Best model is as follows:
glm.fits_rp <- glm(Revealed ~ age + island + pro_soc, 
                  family = binomial,
                  data = wtp_rp)
summary(glm.fits_rp)
hl_rp <- hoslem.test(glm.fits_rp$y, fitted(glm.fits_rp), g=10) # Hosmer and Lemeshow goodness of fit (GOF) test
hl_rp # p-value = 0.7985, so there is no evidence of poor fit

