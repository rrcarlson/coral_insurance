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

# Try a preliminary model

glm.fits <- glm(BID5 ~ econ_1 + econ_2 + pro_soc + pro_nat1 + tenure + size_rev + identity + island, 
                family = binomial,
                data = wtp_m1)

# Predict outcome based on model coefficients
glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
glm.pred <- rep(0, 174)
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, wtp_m1$BID5)

##### Perform best subset selection
# Redefine dataset to include only the variables I am testing
wtp_ms <- wtp[,c("dist", "res", "identity", "age", "gender", "island", "CC", "industry1", "prox", "size_rev", "tenure", "econ_1", "econ_2.1","pro_nat1","pro_soc", "BID1.25", "BID2.5", "BID5")]
wtp_ms <- na.omit(wtp_ms)
wtp_ms[sapply(wtp_ms, is.character)] <- lapply(wtp_ms[sapply(wtp_ms, is.character)], 
                                       as.factor)
# Rename outcome as "y" (required by "bestglm()" function)
wtp_ms_1.25 <- wtp_ms %>% select(-c(BID2.5, BID5)) %>% rename(y = BID1.25)
wtp_ms_2.5 <- wtp_ms %>% select(-c(BID1.25, BID5)) %>% rename(y = BID2.5)
wtp_ms_5 <- wtp_ms %>% select(-c(BID1.25, BID2.5)) %>% rename(y = BID5)

best.logit <- bestglm(wtp_ms_1.25,
                      IC = "AIC",                 # Information criteria for
                      family=binomial,
                      method = "exhaustive")
summary(best.logit$BestModel) # AIC
summary(best.logit.1$BestModel) # BIC

best.logit2 <- bestglm(wtp_ms_2.5,
                      IC = "AIC",                 # Information criteria for
                      family=binomial,
                      method = "exhaustive")
summary(best.logit2$BestModel)
summary(best.logit2.1$BestModel)

best.logit3 <- bestglm(wtp_ms_5,
                      IC = "AIC",                 # Information criteria for
                      family=binomial,
                      method = "exhaustive")
summary(best.logit3$BestModel)
summary(best.logit3.1$BestModel)


# Use multinomial logistic regression instead, for fun (not used in final analysis)






