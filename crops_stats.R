library(terra)
library(sf)
library(geodata)
library(Recocrop)
library(dplyr)

ken <- vect("gadm41_KEN_shp (1)/gadm41_KEN_2.shp")

# Worldclim historical
tavg <- worldclim_global(var="tavg", res="2.5", path=".")
prec <- worldclim_global(var="prec", res="2.5", path=".")

tavg <- crop(tavg, ken)
tavg <- mask(tavg, ken)

prec <- crop(prec, ken)
prec <- mask(prec, ken)

# Historical
suit <- function(tavg, rain, cropnames){
  
  crops <- Recocrop::ecocropPars(cropnames)
  m <- Recocrop::ecocrop(crops)
  control(m, get_max=TRUE)
  mz <- predict(m, tavg=tavg, prec=rain)
  plot(mz)
  
  writeRaster(mz, paste0("output/all_crops/rasters/historical/", cropnames, ".tif"))
}

cropnames <- c("amaranthus", "bean, common", "maize", "cowpea", " pearl millet", "onion",
               "tea", "tomato", "potato", "soyabean", "sorghum (low altitude)", 
               "mung bean", "sweet potato", "mango", "cassava")

lapply(cropnames, suit, tavg=tavg, rain=prec)

# 2030s
tn30 <- rast("wc2.1_2.5m_tmin_MIROC6_ssp585_2021-2040.tif")
tx30 <- rast("wc2.1_2.5m_tmax_MIROC6_ssp585_2021-2040.tif")
pr30 <- rast("wc2.1_2.5m_prec_MIROC6_ssp585_2021-2040.tif")

tn30 <- crop(tn30, ken)
tn30 <- mask(tn30, ken)

tx30 <- crop(tx30, ken)
tx30 <- mask(tx30, ken)

tavg30 <- (tn30 + tx30)/ 2

pr30 <- crop(pr30, ken)
pr30 <- mask(pr30, ken)

suit30 <- function(tavg, rain, cropnames){
  
  crops <- Recocrop::ecocropPars(cropnames)
  m <- Recocrop::ecocrop(crops)
  control(m, get_max=TRUE)
  mz <- predict(m, tavg=tavg, prec=rain)
  plot(mz)
  
  writeRaster(mz, paste0("output/all_crops/rasters/2030s/", cropnames,"_2030s", ".tif"))
}

lapply(cropnames, suit30, tavg=tavg30, rain=pr30)

# 2050s
tn50 <- rast("wc2.1_2.5m_tmin_MIROC6_ssp585_2041-2060.tif")
tx50 <- rast("wc2.1_2.5m_tmax_MIROC6_ssp585_2041-2060.tif")
pr50 <- rast("wc2.1_2.5m_prec_MIROC6_ssp585_2041-2060.tif")

tn50 <- crop(tn50, ken)
tn50 <- mask(tn50, ken)

tx50 <- crop(tx50, ken)
tx50 <- mask(tx50, ken)

tavg50 <- (tn50 + tx50)/ 2

pr50 <- crop(pr50, ken)
pr50 <- mask(pr50, ken)

suit50 <- function(tavg, rain, cropnames){
  
  crops <- Recocrop::ecocropPars(cropnames)
  m <- Recocrop::ecocrop(crops)
  control(m, get_max=TRUE)
  mz <- predict(m, tavg=tavg, prec=rain)
  plot(mz)
  
  writeRaster(mz, paste0("output/all_crops/rasters/2050s/", cropnames,"_2050s", ".tif"))
}

lapply(cropnames, suit50, tavg=tavg50, rain=pr50)

# Extract all stats for the crops
# Read rasters
ke <- vect("gadm41_KEN_shp (1)/gadm41_KEN_2.shp")

extractStats <- function(cropnames){
  r <- rast(paste0("output/all_crops/rasters/historical/", cropnames, ".tif"))
  r30 <- rast(paste0("output/all_crops/rasters/2030s/", cropnames, "_2030s", ".tif"))
  r50 <- rast(paste0("output/all_crops/rasters/2050s/", cropnames, "_2050s", ".tif"))
  
  df <- terra::extract(r, ke, fun=NULL, cells=TRUE)
  df30 <- terra::extract(r30, ke, fun=NULL, cells=TRUE)
  df50 <- terra::extract(r50, ke, fun=NULL, cells=TRUE)
  
  colnames(df) <- c("ID", "suit_hist", "cell")
  colnames(df30) <- c("ID", "suit_2030", "cell")
  colnames(df50) <- c("ID", "suit_2050", "cell")
  
  stats <- cbind(df, df30, df50)
  stats$crops <- paste0(cropnames)
  stats1 <- stats[-c(3,4,6,7)]
  write.csv(stats1, paste0("output/all_crops/intermediate/", cropnames, ".csv"))
  
}

lapply(cropnames, extractStats)

# Combine into one file
files <- list.files("output/all_crops/intermediate/", 
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)

fdf <- read_csv(files) %>% 
  bind_rows()

fdf <- fdf[-c(1)]

ke1 <- st_as_sf(ke)
ken1 <- ke1[,c(5,7)] %>%
  st_drop_geometry()
ken1$ID <- 1:300

# Merge with subcounty names
final_df <- merge(fdf, ken1, by.x="ID", by.y="ID", all=TRUE)

final_df1 <- final_df[c(1, 5, 7, 8, 2, 3, 4,6)]
colnames(final_df1) <- c("ID", "cell", "admin 1", "admin 2",  "suit_hist", 
                         "suit_2030", "suit_2050", "crops")
head(final_df1)

write.csv(final_df1, "output/all_crops/suit_ken1.csv")

View(final_df)

# All crops in mapspam -- 42 crops
# List of crops
