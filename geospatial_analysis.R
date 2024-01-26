##### Coral insurance geospatial analysis
##### Start of analysis: 31 May 2022
##### Author: Rachel Carlson, rrcarlson@ucdavis.edu

# Rescales coral health and value layers for the Big Island and Oahu
# Attributes coral health and value data to n=202 business locations in Hawaii

##### Load libraries
library(raster)
library(sf)
library(tidyverse)
library(ISLR2)
library(ggplot2)

##### Coral health: Load GAO coral cover data for Oahu and Hawaii Island

GAO_Hawaii <- raster("/Users/rachelcarlson/Documents/Research/RS_data/islands/Hawaii/Hawaii_merged_CC.tif")
GAO_Oahu <- raster("/Users/rachelcarlson/Documents/Research/RS_data/islands/Oahu/ASU_GAO_Oahu_Percent_Live_Cover_v3.tif")
GAO_Kauai <- raster("/Users/rachelcarlson/Documents/Research/RS_data/islands/Kauai/ASU_GAO_Kauai_Percent_Live_Cover_v3.tif")
GAO_Maui <- raster("/Users/rachelcarlson/Documents/Research/RS_data/islands/Maui/ASU_GAO_Maui_Percent_Live_Cover_v3 (1).tif")

NAvalue(GAO_Hawaii) <- 255
NAvalue(GAO_Oahu) <- 255
NAvalue(GAO_Kauai) <- 255
NAvalue(GAO_Maui) <- 255

##### Backup coral health: PacIOOS summed for all species. (There are a few small gaps in GAO in Oahu that we need to impute from PacIOOS.)

# List files belonging to individual species
Pac_ls <- list.files("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/PacIOOS_CC/all_spp/",
                     pattern = ".asc", full.names = TRUE)
Pac <- raster::stack(Pac_ls) # Create raster stack where each layer = CC of one species
crs(Pac) = "+proj=utm +zone=5 +datum=WGS84 +units=m +no_defs"
Pac_sum <- calc(Pac, sum) # Sum layers in the raster stack for one raster representing all spp
# Clip `Pac_sum` to Oahu and Big Island separately
Big_clip <- st_read("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Clips/Clip_Big.shp") # Load clip cookie cutter/spatial extent polygon
Oahu_clip <- st_read("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Clips/Clip_Oahu.shp")
Kauai_clip <- st_read("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Clips/Clip_Kauai.shp")
Maui_clip <- st_read("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Clips/Clip_Maui.shp")
embayment_clip <- st_read("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Clips/embayment.shp")

Pac_Oahu <- crop(Pac_sum, Oahu_clip) # Crop Pac to clip polygon boundary
Pac_Big <- crop(Pac_sum, Big_clip)
Pac_Kauai <- crop(Pac_sum, Kauai_clip)
Pac_Maui <- crop(Pac_sum, Maui_clip)

##### Coral value (rec): Load Spalding et al., 2017 data

rec_all <- st_read("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Spalding/MOW_Global_Coral_Tourism_Jan2017_Total_Dollar_Value_Poly_Equal_Area_clip.shp")
# Clip `rec_all` to Oahu and Big Island separately
rec_all <- rec_all %>% st_transform(crs = crs(Big_clip)) # Ensure clip bbox and shapefile have same projection (use crs of Big Island as template)
rec_Oahu <- st_crop(rec_all, st_bbox(Oahu_clip)) # Crop layer to clip box
rec_Big <- st_crop(rec_all, st_bbox(Big_clip))
rec_Kauai <- st_crop(rec_all, st_bbox(Kauai_clip))
rec_Maui <- st_crop(rec_all, st_bbox(Maui_clip))

##### Rescale all layers based on 5 quantiles

# If GAO rasters are too large to calculate quantiles locally, users can opt to resample GAO raster.
# GAO_HI_40 <- raster("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Spatial/Basemaps/GAO_Hawaii_merged_40m.tif")
# GAO_Oahu_40 <- raster("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Spatial/Basemaps/GAO_Oahu_40m.tif")
# GAO_Kauai_40 <- raster("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Spatial/Basemaps/GAO_Kauai_40m.tif")
# GAO_Maui_40 <- raster("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Spatial/Basemaps/GAO_Maui_40m.tif")

# Determine quantile breaking points
q_gao_hi <- raster::quantile(GAO_HI, probs = c(0.2, 0.4, 0.6, 0.8), names = FALSE, na.rm = TRUE)
q_gao_oahu <- raster::quantile(GAO_Oahu, probs = c(0.2, 0.4, 0.6, 0.8), names = FALSE, na.rm = TRUE)
q_gao_kauai <- raster::quantile(GAO_Kauai, probs = c(0.2, 0.4, 0.6, 0.8), names = FALSE, na.rm = TRUE)
q_gao_maui <- raster::quantile(GAO_Maui, probs = c(0.2, 0.4, 0.6, 0.8), names = FALSE, na.rm = TRUE)

q_rec_hi <- quantile(rec_Big$val_per_ha, probs = c(0.2, 0.4, 0.6, 0.8), names = FALSE, na.rm = TRUE)
q_rec_oahu <- quantile(rec_Oahu$val_per_ha, probs = c(0.2, 0.4, 0.6, 0.8), names = FALSE, na.rm = TRUE)
q_rec_all <- quantile(rec_all$val_per_ha, probs = c(0.2, 0.4, 0.6, 0.8), names = FALSE, na.rm = TRUE)

##### Load business data. Business data is private due to identifying information.
bus <- read.csv("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/business_data.csv") %>%
  dplyr::filter(!is.na(lat_sea)) %>% 
  st_as_sf(coords = c("lon_sea","lat_sea"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%  # Location information comes from location where they operate in the ocean. If they do not operate in the ocean, lon_sea and lat_sea reflects closest shoreline location. For evaluating flood data, we change geographic attributes to actual business location where most physical infrastructure is located. 
  mutate(ID = row_number())
# Correct error in Island
bus$island <- ifelse(bus$island == "Hawaii\tNorth Kona","Hawaii", bus$island)

##### Coral health data

# Shoreline commercial
bus_buff_1k <- bus %>% dplyr::filter(is.na(loc_sea)) %>% # Shoreline locations = those locations with NA at loc_sea
  st_buffer(1000) #%>% st_transform(crs = 4326) # Buffered within 1 km radius

# Locations in the water, i.e., dive, snorkel, and other marine operators
bus_buff_200m <- bus %>% dplyr::filter(!is.na(loc_sea)) %>% # Ocean locations = locations with value (no NA) at loc_sea
  st_buffer(200) #%>% st_transform(crs = 4326)

# Hawaii

# First derive mean coral cover from GAO data for shoreline locations
bus_1k_HI <- bus_buff_1k %>% filter(island == "Hawaii")
bus_1k_HI <- bus_1k_HI %>% mutate(
    ACC_mean = raster::extract(GAO_HI, bus_1k_HI, fun = mean, na.rm = TRUE),
    ACC_max = raster::extract(GAO_HI, bus_1k_HI, fun = max, na.rm = TRUE))

# Then for ocean locations
bus_200m_HI <- bus_buff_200m %>% filter(island == "Hawaii")
bus_200m_HI <- bus_200m_HI %>% mutate(
  ACC_mean = raster::extract(GAO_HI, bus_200m_HI, fun = mean, na.rm = TRUE),
  ACC_max = raster::extract(GAO_HI, bus_200m_HI, fun = max, na.rm = TRUE))

# Oahu

# First derive coral cover from GAO data. There will be gaps in GAO in Oahu that we need to impute from PacIOOS.
bus_1k_Oahu <- bus_buff_1k %>% filter(island == "Oahu") 
bus_1k_Oahu <- bus_1k_Oahu %>% dplyr::mutate(
  ACC_mean = raster::extract(GAO_Oahu, bus_1k_Oahu, fun = mean, na.rm = TRUE),
  ACC_max = raster::extract(GAO_Oahu, bus_1k_Oahu, fun = max, na.rm = TRUE))

bus_200m_Oahu <- bus_buff_200m %>% filter(island == "Oahu") 
bus_200m_Oahu <- bus_200m_Oahu %>% mutate(
  ACC_mean = raster::extract(GAO_Oahu, bus_200m_Oahu, fun = mean, na.rm = TRUE),
  ACC_max = raster::extract(GAO_Oahu, bus_200m_Oahu, fun = max, na.rm = TRUE))

# To fill in gaps in coral cover, need to impute coral cover data from PacIOOS where GAO data does not exist (there are a few small gaps). If this doesn't work in R due to sf version, can conduct this process in QGIS.
for (i in 1:nrow(bus_1k_Oahu)) {
   if (is.na(bus_1k_Oahu$ACC_mean[i]) == TRUE) {
     bus_1k_Oahu$ACC_mean[i] = raster::extract(Pac_Oahu, bus_1k_Oahu, fun = mean, na.rm = TRUE)
     bus_1k_Oahu$ACC_max[i] = raster::extract(Pac_Oahu, bus_1k_Oahu, fun = max, na.rm = TRUE)
   }
 }
 
 for (i in 1:nrow(bus_200m_Oahu)) {
   if (is.na(bus_200m_Oahu$ACC_mean[i]) == TRUE) {
     bus_200m_Oahu$ACC_mean[i] = raster::extract(Pac_Oahu, bus_200m_Oahu[i], fun = mean, na.rm = TRUE)
     bus_200m_Oahu$ACC_max[i] = raster::extract(Pac_Oahu, bus_200m_Oahu[i], fun = max, na.rm = TRUE)
   }
 }

# Bind all datasets (each island, type of buffer) together (note: "ACC" = "Actual Cover Cover").
all <- do.call("rbind", list(bus_1k_HI[ , c("ID", "ACC_mean", "ACC_max")], 
                             bus_1k_Oahu[ , c("ID", "ACC_mean", "ACC_max")],  
                             bus_200m_HI[ , c("ID", "ACC_mean", "ACC_max")], 
                             bus_200m_Oahu[ , c("ID", "ACC_mean", "ACC_max")])) %>% 
  as.data.frame() %>% select(-geometry)

# Bind two coral cover variables back to original business dataset
bus <- left_join(x = bus, y = all, by = "ID")

##### Value (rec) data
rec_all_filtered <- rec_all %>% select(Total_Val, val_per_ha) # select only those recreational value attributes I wish to join to business dataset
bus <- st_join(bus, rec_all_filtered) # spatial join

bus <- bus %>% as.data.frame() %>% select(-geometry)
write.csv(bus, "/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/business_sea.csv")

##### Value (flood) data
flood <- st_read("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Reguero/Floodmasks/HawaiianIslands_floodpoints.shp")

# Switch geometry to actual location where most infrastructure is located (not reef they visit with boats in the ocean, etc.).
bus_land <- read.csv("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/business_sea.csv") %>%
  dplyr::filter(!is.na(lat_land)) %>% 
  st_as_sf(coords = c("lon_land","lat_land"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
bus_land <- bus_land %>% st_transform(crs(flood))

# Dissociate floodgauge names from island
flood$Scenario <- str_remove(flood$Scenario,"Hawaii_")
flood$Scenario <- str_remove(flood$Scenario,"Oahu_")
flood$Scenario <- str_remove(flood$Scenario,"Kauai_")
flood$Scenario <- str_remove(flood$Scenario,"Maui_")

foo <- st_nearest_feature(bus_land,flood) # Find flood gauge points nearest to land locations of businesses
foo2 <- flood[foo,] # Create new dataframe of ONLY flood points nearest business locations
foo3 <- st_intersects(foo2, flood) # See if those nearest points intersect any others (they will)

# Create new columns in business dataframe for flood depth under different scenarios
# Used to see if flood depth is higher for coastal businesses without coral reefs
for (i in 1:nrow(bus_land)) {
  df <- flood[foo3[[i]],c("Scenario","F_Depth")] %>% as.data.frame() %>% select(-geometry) %>% spread(Scenario, F_Depth)
  bus_land$worf_10[i] <- ifelse("rp10_worf_floodpoints" %in% colnames(df), df[1,"rp10_worf_floodpoints"], NA)
  bus_land$wrf_10[i] <- ifelse("rp10_wrf_floodpoints" %in% colnames(df), df[1,"rp10_wrf_floodpoints"], NA)
  bus_land$worf_50[i] <- ifelse("rp50_worf_floodpoints" %in% colnames(df), df[1,"rp50_worf_floodpoints"], NA)
  bus_land$wrf_50[i] <- ifelse("rp50_wrf_floodpoints" %in% colnames(df), df[1,"rp50_wrf_floodpoints"], NA)
  bus_land$wrf_100[i] <- ifelse("rp100_wrf_floodpoints" %in% colnames(df), df[1,"rp100_wrf_floodpoints"], NA)
  bus_land$worf_100[i] <- ifelse("rp100_worf_floodpoints" %in% colnames(df), df[1,"rp100_worf_floodpoints"], NA)
  bus_land$wrf_500[i] <- ifelse("rp500_wrf_floodpoints" %in% colnames(df), df[1,"rp500_wrf_floodpoints"], NA)
  bus_land$worf_500[i] <- ifelse("rp500_worf_floodpoints" %in% colnames(df), df[1,"rp500_worf_floodpoints"], NA)
  }

# The above is based on flood gauge points, where we tagged the nearest point to businesses
# However, businesses may be NEAR a flood point without being inside the floodplain
# We want to determine those businesses inside v. outside the floodplain

# Load floodplain shapefile
flood_shp <- st_read("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Reguero/Floodmasks/HawaiianIslands_floodmasks.shp")

# See which floodplains intersect which businesses
foo4 <- st_intersects(bus_land, flood_shp) # See if those nearest points intersect any others (they will)

# Dissociate floodplain names from island
flood_shp$MaskName <- str_remove(flood_shp$MaskName,"Hawaii_")
flood_shp$MaskName <- str_remove(flood_shp$MaskName,"Oahu_")
flood_shp$MaskName <- str_remove(flood_shp$MaskName,"Kauai_")
flood_shp$MaskName <- str_remove(flood_shp$MaskName,"Maui_")

# Create columns that say 1 if inside given floodplain, 0 if not
for (i in 1:nrow(bus_land)) {
  df <- flood_shp[foo4[[i]],c("MaskName")] %>% as.data.frame() %>% select(-geometry)
  bus_land$worf_10_fp[i] <- ifelse("rp10_worf_floodmask" %in% df$MaskName, 1, 0)
  bus_land$wrf_10_fp[i] <- ifelse("rp10_wrf_floodmask" %in% df$MaskName, 1, 0)
  bus_land$worf_50_fp[i] <- ifelse("rp50_worf_floodmask" %in% df$MaskName, 1, 0)
  bus_land$wrf_50_fp[i] <- ifelse("rp50_wrf_floodmask" %in% df$MaskName, 1, 0)
  bus_land$wrf_100_fp[i] <- ifelse("rp100_wrf_floodmask" %in% df$MaskName, 1, 0)
  bus_land$worf_100_fp[i] <- ifelse("rp100_worf_floodmask" %in% df$MaskName, 1, 0)
  bus_land$wrf_500_fp[i] <- ifelse("rp500_wrf_floodmask" %in% df$MaskName, 1, 0)
  bus_land$worf_500_fp[i] <- ifelse("rp500_worf_floodmask" %in% df$MaskName, 1, 0)
}

# Calculate val (flood) metric

# Find which businesses are: 1) in fp without reefs, but not with, 2) in fp with/out reefs but deeper flood wo, 3) not in any fp
bus_land$fp_10_diff <- bus_land$worf_10_fp + bus_land$wrf_10_fp
bus_land$fp_50_diff <- bus_land$worf_50_fp + bus_land$wrf_50_fp
bus_land$fp_100_diff <- bus_land$worf_100_fp + bus_land$wrf_100_fp
bus_land$fp_500_diff <- bus_land$worf_500_fp + bus_land$wrf_500_fp

bus_land <- bus_land %>% select(-c(worf_10_fp, worf_50_fp, worf_100_fp, worf_500_fp, wrf_10_fp, wrf_50_fp, wrf_100_fp, wrf_500_fp))

bus_land$fl_val_10 <- ifelse(bus_land$fp_10_diff > 0, 5, 1)
bus_land$fl_val_50 <- ifelse(bus_land$fp_50_diff > 0, 5, 1)
bus_land$fl_val_100 <- ifelse(bus_land$fp_100_diff > 0, 5, 1)
bus_land$fl_val_500 <- ifelse(bus_land$fp_500_diff > 0, 5, 1)

# Summary statistics on amount flood levels increased at businesses
# 10-year storm
mean(ifelse(bus_land$fp_10_diff > 0, 1, 0) * (bus_land$worf_10 - bus_land$wrf_10), na.rm = TRUE) # 0.29
range(ifelse(bus_land$fp_10_diff > 0, 1, 0) * (bus_land$worf_10 - bus_land$wrf_10), na.rm = TRUE) # 0 - 1.53

# 50-year storm
mean(ifelse(bus_land$fp_50_diff > 0, 1, 0) * (bus_land$worf_50 - bus_land$wrf_50), na.rm = TRUE) # 0.32
range(ifelse(bus_land$fp_50_diff > 0, 1, 0) * (bus_land$worf_50 - bus_land$wrf_50), na.rm = TRUE) # 0 - 1.17

# 100-year storm
mean(ifelse(bus_land$fp_100_diff > 0, 1, 0) * (bus_land$worf_100 - bus_land$wrf_100), na.rm = TRUE) # 0.32
range(ifelse(bus_land$fp_100_diff > 0, 1, 0) * (bus_land$worf_100 - bus_land$wrf_100), na.rm = TRUE) # 0 - 1.37

# 500-year storm
mean(ifelse(bus_land$fp_100_diff > 0, 1, 0) * (bus_land$worf_500 - bus_land$wrf_500), na.rm = TRUE) # 0.18
range(ifelse(bus_land$fp_100_diff > 0, 1, 0) * (bus_land$worf_500 - bus_land$wrf_500), na.rm = TRUE) # 0 - 1.43

# Number of businesses with increased risk
sum(bus_land$fl_val_10 > 1) # 114 under 10y storm
sum(bus_land$fl_val_50 > 1) # 119 under 50y storm
sum(bus_land$fl_val_100 > 1) # 120 under 100y storm
sum(bus_land$fl_val_500 > 1) # 128 under 500y storm

write.csv(bus_land, "/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/business_land.csv")

##### Generate over- under-estimate index for all variables

##### Value (flood)
# Find index for over- and under-estimates. Negative scores = underestimates, positive = overestimates
bus_land$fl_score_10 <- bus_land$Q16_4 - bus_land$fl_val_10
bus_land$fl_score_50 <- bus_land$Q16_4 - bus_land$fl_val_50
bus_land$fl_score_100 <- bus_land$Q16_4 - bus_land$fl_val_100
bus_land$fl_score_500 <- bus_land$Q16_4 - bus_land$fl_val_500

# Mean under/over- estimate per storm return period
mean(bus_land$fl_score_10, na.rm = TRUE)
mean(bus_land$fl_score_50, na.rm = TRUE)
mean(bus_land$fl_score_100, na.rm = TRUE)
mean(bus_land$fl_score_500, na.rm = TRUE)

##### Value (recreation)
# Find index for over- and under-estimates. Negative scores = underestimates, positive = overestimates. Cutoffs of rec value taken from quantile breaking points determined above.
bus$rec_scaled[bus$island == "Hawaii" & bus$val_per_ha <= 68936] <- 1
bus$rec_scaled[bus$island == "Hawaii" & bus$val_per_ha > 68936 & bus$val_per_ha <= 73160] <- 2
bus$rec_scaled[bus$island == "Hawaii" & bus$val_per_ha > 73160 & bus$val_per_ha <= 117608] <- 3
bus$rec_scaled[bus$island == "Hawaii" & bus$val_per_ha > 117608 & bus$val_per_ha <= 123656] <- 4
bus$rec_scaled[bus$island == "Hawaii" & bus$val_per_ha > 123656] <- 5

bus$rec_scaled[bus$island == "Oahu" & bus$val_per_ha <= 5360] <- 1
bus$rec_scaled[bus$island == "Oahu" & bus$val_per_ha > 5360 & bus$val_per_ha <= 6656] <- 2
bus$rec_scaled[bus$island == "Oahu" & bus$val_per_ha > 6656 & bus$val_per_ha <= 11384] <- 3
bus$rec_scaled[bus$island == "Oahu" & bus$val_per_ha > 11384 & bus$val_per_ha <= 22784] <- 4
bus$rec_scaled[bus$island == "Oahu" & bus$val_per_ha > 22784] <- 5

mean(bus$rec_scaled, na.rm = TRUE)
bus$rec_score <- (bus$Q16_2 + bus$Q16_5)/2 - bus$rec_scaled
bus$rec_perc <- (bus$Q16_2 + bus$Q16_5)/2
mean(bus$rec_score, na.rm = TRUE)


##### Reef health. Cutoffs of rescaling index (1-5) taken from quantile breaking points determined above (q_gao_hi and q_gao_oahu)
bus$ACC_mean <- as.numeric(bus$ACC_mean)
bus$ACC_mean_scaled[bus$island == "Hawaii" & bus$ACC_mean <= q_gao_hi[1]] <- 1
bus$ACC_mean_scaled[bus$island == "Hawaii" & bus$ACC_mean > q_gao_hi[1] & bus$ACC_mean <= q_gao_hi[2]] <- 2
bus$ACC_mean_scaled[bus$island == "Hawaii" & bus$ACC_mean > q_gao_hi[2] & bus$ACC_mean <= q_gao_hi[3]] <- 3
bus$ACC_mean_scaled[bus$island == "Hawaii" & bus$ACC_mean > q_gao_hi[3] & bus$ACC_mean <= q_gao_hi[4]] <- 4
bus$ACC_mean_scaled[bus$island == "Hawaii" & bus$ACC_mean > q_gao_hi[4]] <- 5

bus$ACC_mean_scaled[bus$island == "Oahu" & bus$ACC_mean <= q_gao_oahu[1]] <- 1
bus$ACC_mean_scaled[bus$island == "Oahu" & bus$ACC_mean > q_gao_oahu[1] & bus$ACC_mean <= q_gao_oahu[2]] <- 2
bus$ACC_mean_scaled[bus$island == "Oahu" & bus$ACC_mean > q_gao_oahu[2] & bus$ACC_mean <= q_gao_oahu[3]] <- 3
bus$ACC_mean_scaled[bus$island == "Oahu" & bus$ACC_mean > q_gao_oahu[3] & bus$ACC_mean <= q_gao_oahu[4]] <- 4
bus$ACC_mean_scaled[bus$island == "Oahu" & bus$ACC_mean > q_gao_oahu[4]] <- 5


# Rescale reef health instead based on literature of baseline "healthy" coral cover.
# Historical average for the Indo-Pacific is 36% (Bruno and Selig, 2007: https://doi.org/10.1371/journal.pone.0000711)
# >10% change is considered a "major" shift in absolute coral cover
# Average coral cover in HI at 152 reef stations was 20.3% in Friedlander et al., 2008: "The state of coral reef ecosystems of the main Hawaiian Islands"
# Based on these studies, 20 is considered the midway point ("average" condition) of scores 1-5. Since 10% change is a "major" shift, <5% cover represents our threshold for score = 1 and >35% for score = 5.
q_lit_all <- c(5, 15, 25, 35)

# Mean rescaled coral cover within buffer zone of businesses
bus$ACC_mean_scaled_2[bus$ACC_mean <= q_lit_all[1]] <- 1
bus$ACC_mean_scaled_2[bus$ACC_mean > q_lit_all[1] & bus$ACC_mean <= q_lit_all[2]] <- 2
bus$ACC_mean_scaled_2[bus$ACC_mean > q_lit_all[2] & bus$ACC_mean <= q_lit_all[3]] <- 3
bus$ACC_mean_scaled_2[bus$ACC_mean > q_lit_all[3] & bus$ACC_mean <= q_lit_all[4]] <- 4
bus$ACC_mean_scaled_2[bus$ACC_mean > q_lit_all[4]] <- 5

# Max rescaled coral cover within buffer zone of businesses
bus$ACC_max_scaled_2[bus$ACC_max <= q_lit_all[1]] <- 1
bus$ACC_max_scaled_2[bus$ACC_max > q_lit_all[1] & bus$ACC_max <= q_lit_all[2]] <- 2
bus$ACC_max_scaled_2[bus$ACC_max > q_lit_all[2] & bus$ACC_max <= q_lit_all[3]] <- 3
bus$ACC_max_scaled_2[bus$ACC_max > q_lit_all[3] & bus$ACC_max <= q_lit_all[4]] <- 4
bus$ACC_max_scaled_2[bus$ACC_max > q_lit_all[4]] <- 5

# Summary statistics
mean(bus$ACC_mean_scaled, na.rm = TRUE) # based on intra-island quantiles
mean(bus$ACC_mean_scaled_2, na.rm = TRUE) # based on historical baseline

# Code perception of coral cover on Likert Scale
bus$CC <- ifelse(bus$CC == "Uhealthy","Unhealthy", bus$CC) # Correct typos
bus$CC <- ifelse(bus$CC == "Unhealthy (though some spots healthy--reflection on amount of fish, not coral)","Unhealthy", bus$CC)
bus$CC <- ifelse(bus$CC == "Unsure", NA, bus$CC)

bus$CC[bus$CC == "Very unhealthy"] <- 1 # Rescale
bus$CC[bus$CC == "Unhealthy"] <- 2
bus$CC[bus$CC == "Fair"] <- 3
bus$CC[bus$CC == "Healthy"] <- 4
bus$CC[bus$CC == "Very healthy"] <- 5

bus$CC <- as.numeric(bus$CC)
mean(bus$CC, na.rm = TRUE)

st_write(bus, "/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Spatial/bus_sea_vis2.shp")
st_write(bus_land, "/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Spatial/bus_land_vis.shp")

##### Bootstrapping to compare means
## Note: variable/attribute names are modified slightly depending on how csvs automatically retitle variables (argh). You may have to revise these.

##### Value (flood)

# Create a lean dataset with only the variables of interest, taking the average flood score across all locations
bus_land <- st_read("/Users/rachelcarlson/Documents/Research/Coral_Insurance/Data/Spatial/bus_land_vis.shp")
flood_bts <- bus_land %>% as.data.frame() %>% select(-geometry) %>% 
  dplyr::group_by(Name,identty,tenure,indstr1,indstr_, outside, influnc, age, island, prox, revenue) %>% # This groups values associated with multiple locations of same business into same business "Name" and appends other identifying categories/variables of interest.
  summarize(fl_perc = mean(Q16_4),
            fl_v_10 = max(fl_v_10),
            fl_s_10 = mean(fl_s_10),
            fl_s_50 = mean(fl_s_50),
            fl_s_100 = mean(fl_s_100),
            fl_s_500 = mean(fl_s_500))

flood_bts$indstr1 <- ifelse(flood_bts$indstr1 == "Recreational surface","Recreation surface",flood_bts$indstr1) # Correct typo ("Recreational surface" and "Recreation surface" are the same category)

# Bootstrap based on grouping variables of interest (model below for "Recreation surface" - can use for any category.
bt_sub <- flood_bts %>% filter((indstr1 == "Recreation surface") & !is.na(fl_s_10))
n = length(bt_sub$fl_s_10)
B = 10000
result = rep(NA, B)
for (i in 1:B) {
  boot.sample = sample(n, replace = TRUE)
  result[i] = mean(bt_sub$fl_s_10[boot.sample])
}
with(bt_sub, mean(fl_s_10) + c(-1, 1) * 2 * sd(result))

mean(bt_sub$fl_s_10)

##### Value (recreation)
REC_bts <- bus %>% as.data.frame() %>% select(-geometry) %>% 
  dplyr::group_by(Name,identity,tenure,industry1,industry_sub, outside, influence, age, island, prox, revenue, res) %>% # This groups values associated with multiple locations of same business into same business "Name" and appends other identifying categories/variables of interest.
  summarize(rec_perc = mean(rec_perc, na.rm = TRUE),
            rec_score = mean(rec_score, na.rm = TRUE),
            rec_scaled = mean(rec_scaled, na.rm = TRUE),
            Total_val = mean(Total_Val, na.rm = TRUE),
            val_per_ha = max(val_per_ha, na.rm = TRUE))

REC_bts$industry1 <- ifelse(REC_bts$industry1 == "Recreational surface","Recreation surface",REC_bts$industry1)

# Bootstrap based on grouping variables of interest (model below for "Recreation surface" - can use for any category.
bt_sub <- REC_bts %>% filter((indstr1 == "Recreation surface") & (!is.na(rec_score)))
n = length(bt_sub$rec_score)
B = 10000
result = rep(NA, B)
for (i in 1:B) {
  boot.sample = sample(n, replace = TRUE)
  result[i] = mean(bt_sub$rec_score[boot.sample])
}
with(bt_sub, mean(rec_score) + c(-1, 1) * 2 * sd(result))

mean(bt_sub$rec_score)

##### Coral health
CC_bts <- bus %>% as.data.frame() %>% select(-geometry) %>% 
  dplyr::group_by(Name,identity,tenure,industry1,industry_sub, outside, influence, age, island, prox, revenue, res) %>% # This groups values associated with multiple locations of same business into same business "Name" and appends other identifying categories/variables of interest.
  summarize(rec_perc = mean(rec_perc, na.rm = TRUE),
  summarize(CC = mean(CC),
            customer = mean(Q16_1),
            ACC_mean = mean(ACC_mean),
            ACC_max = max(ACC_max),
            ACC_max_scaled_2 = mean(ACC_max_scaled_2),
            ACC_mean_scaled = mean(ACC_mean_scaled),
            ACC_mean_scaled_2 = mean(ACC_mean_scaled_2),
            CC_score = mean(CC_score),
            CC_score_2 = mean(CC_score_2))

CC_bts$industry1 <- ifelse(CC_bts$industry1 == "Recreational surface","Recreation surface",CC_bts$industry1)

# Bootstrap based on grouping variables of interest
bt_sub <- CC_bts %>% filter((island == "Oahu") & (!is.na(ACC_mean)))
n = length(bt_sub$ACC_mean)
B = 10000
result = rep(NA, B)
for (i in 1:B) {
  boot.sample = sample(n, replace = TRUE)
  result[i] = mean(bt_sub$ACC_mean[boot.sample])
}
with(bt_sub, mean(ACC_mean) + c(-1, 1) * 2 * sd(result))

mean(bt_sub$ACC_mean)

