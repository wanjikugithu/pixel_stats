library(terra)
library(sf)
library(geodata)
library(Recocrop)
library(dplyr)
library(readr)
library(tidyr)

ken <- vect("gadm41_KEN_shp (1)/gadm41_KEN_2.shp")

# 2050s
tn50 <- rast("wc2.1_5m_tmin_MPI-ESM1-2-HR_ssp585_2041-2060.tif")
tx50 <- rast("wc2.1_5m_tmax_MPI-ESM1-2-HR_ssp585_2041-2060.tif")
pr50 <- rast("wc2.1_5m_prec_MPI-ESM1-2-HR_ssp585_2041-2060.tif")

tn50 <- crop(tn50, ken)
tn50 <- mask(tn50, ken)

tx50 <- crop(tx50, ken)
tx50 <- mask(tx50, ken)

tavg50 <- (tn50 + tx50)/ 2

pr50 <- crop(pr50, ken)
pr50 <- mask(pr50, ken)

cropnames <- c("wheat, common", "bean, common", "maize", "cowpea", "pearl millet", 
               "chick pea", "tea", "pigeon pea", "potato", "soyabean", 
               "sorghum (med. altitude)", "lentil", "sweet potato", "groundnut", 
               "cassava", "coconut", "sunflower", "sesame seed", "sugar beet", 
               "tobacco", "coffee arabica", "coffee robusta", "cacao", "plantain bananas")

suit50 <- function(tavg, rain, cropnames){
  
  crops <- Recocrop::ecocropPars(cropnames)
  m <- Recocrop::ecocrop(crops)
  control(m, get_max=TRUE)
  mz <- predict(m, tavg=tavg, prec=rain)
  plot(mz)
  
  writeRaster(mz, paste0("output/mapspam2010/2050s/rasters/", cropnames,"_2050s", ".tif"))
}

lapply(cropnames, suit50, tavg=tavg50, rain=pr50)

spm <- read_csv("C:/Users/Beatrice Wanjiku/Downloads/spam2010v2r0_global_phys_area_csv/spam2010V2r0_global_A_TH.csv")

ke <- spm %>%
  select(iso3, alloc_key, cell5m, x, y) %>%
  filter(iso3 == "KEN")

kef <- ke
kef$id <- 1:2367

ke <- ke[c(4:5)]

# Extract stats
extractStats <- function(cropnames){
  
  r <- rast(paste0("output/mapspam2010/2050s/rasters/", cropnames, "_2050s", ".tif"))
  df <- terra::extract(r, ke, fun=NULL) #, cells=TRUE, xy=TRUE)
  
  # colnames(df) <- c("ID", "suit_hist", "cell", "x", "y")
  colnames(df) <- c("ID", "suit_2050s")
  
  stats <- df
  stats$crops <- paste0(cropnames)
  
  write.csv(stats, paste0("output/mapspam2010/2050s/intermediate/", cropnames, "_2050s", ".csv"))
  
}

lapply(cropnames, extractStats)

# Combine into one file
files <- list.files("output/mapspam2010/2050s/intermediate/", 
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)

fdf <- read_csv(files) %>%
  bind_rows()

# Convert from long to wide format 
# xy included

fdf <- fdf[-1]

fdf1 <- fdf %>% 
  pivot_wider(id_cols=ID, names_from = crops, values_from = suit_2050s)

# merge with kef
final_df <- merge(fdf1, kef, by.x="ID", by.y="id", all=TRUE)
ff <- final_df[c(1, 26:30, 2:25)]

write.csv(ff, "output/mapspam2010/MPI-ESM1-2-HR_suit.csv")

# some pixels missed values
r <- rast("output/2050s/rasters/bean, common_2050s.tif")
kek <- ke[2035,]

dfsp1 <- st_as_sf(kek, coords = c("x", "y"), crs = st_crs(ken))

dfff <- terra::extract(r, kek, fun=NULL)

plot(r)
plot(dfsp1, add=TRUE)
