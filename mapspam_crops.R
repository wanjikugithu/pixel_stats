library(terra)
library(sf)
library(geodata)
library(Recocrop)
library(dplyr)
library(readr)
library(tidyr)

ken <- vect("gadm41_KEN_shp (1)/gadm41_KEN_2.shp")
ken1 <- st_as_sf(ken)

dfsp <- st_as_sf(ke, coords = c("x", "y"), crs = st_crs(ken))
pnts <- dfsp %>% mutate(
  intersection = as.integer(st_intersects(geometry, ken))
  , area = if_else(is.na(intersection), '', ken$NAME_2[intersection])
) 

r <- rast("output/mapspam2010/2050s/rasters/cacao_2050s.tif")
r1 <- rast("output/mapspam2010/rasters/5m/cacao.tif")
r11 <- rast("output/mapspam2010/rasters/5m/cacao.tif")
r2 <- rast("output/mapspam2010/rasters/cacao.tif")
dd <- dfsp[2199,]

plot(r11)
plot(dd, add =TRUE)
# df <- read.csv("mapspam_crops.csv")

# WorldClim historical
tavg <- worldclim_global(var="tavg", res="5", path=".")
prec <- worldclim_global(var="prec", res="5", path=".")

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
  
  writeRaster(mz, paste0("output/mapspam2010/rasters/5m/", cropnames, ".tif"))
}

# cropnames <- c("amaranthus", "bean, common", "maize", "cowpea", " pearl millet", "onion",
#                "tea", "tomato", "potato", "soyabean", "sorghum (low altitude)", 
#                "mung bean", "sweet potato", "mango", "cassava")

cropnames <- c("wheat, common", "bean, common", "maize", "cowpea", "pearl millet", "chick pea",
               "tea", "pigeon pea", "potato", "soyabean", "sorghum (med. altitude)", 
               "lentil", "sweet potato", "groundnut", "cassava", "coconut", "sunflower", 
               "sesame seed", "sugar beet", "tobacco", "coffee arabica", "coffee robusta",
               "cacao", "plantain bananas")

lapply(cropnames, suit, tavg=tavg, rain=prec)

# Extract stats for historical suitability only
# Using mapspam xy coordinates
spm <- read_csv("C:/Users/Wanjiku/Downloads/spam2010v2r0_global_phys_area_csv/spam2010V2r0_global_A_TH.csv")

ke <- spm %>%
  select(iso3, alloc_key, cell5m, x, y) %>%
  filter(iso3 == "KEN")

kef <- ke
kef$id <- 1:2367

ke <- ke[c(4:5)]
# write.csv(ke, "ken_coords.csv")
extractStats <- function(cropnames){
  
  r <- rast(paste0("output/mapspam2010/rasters/5m/", cropnames, ".tif"))
  
  # df <- terra::extract(r, ken, fun=NULL, cells=TRUE, xy=TRUE) #IDs of input spatvector y
  df <- terra::extract(r, ke, fun=NULL) #, cells=TRUE, xy=TRUE)
  
  # colnames(df) <- c("ID", "suit_hist", "cell", "x", "y")
  colnames(df) <- c("ID", "suit_hist")
  
  stats <- df
  stats$crops <- paste0(cropnames)
  
  # stats1 <- stats[-c(3,4,6,7)]
  write.csv(stats, paste0("output/mapspam2010/rasters/5m/stats/", cropnames, ".csv"))
  
}

lapply(cropnames, extractStats)

# Combine into one file
files <- list.files("output/mapspam2010/rasters/5m/stats/", 
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)

fdf <- read_csv(files) %>%
  bind_rows()

# Convert from long to wide format 
# xy included

fdf <- fdf[-1]
fdf1 <- fdf %>% 
  pivot_wider(id_cols=ID, names_from = crops, values_from = suit_hist)

# merge with kef
final_df <- merge(fdf1, kef, by.x="ID", by.y="id", all=TRUE)
ff <- final_df[c(1, 26:30, 2:25)]
write.csv(ff, "output/mapspam2010/ken_hist_5m.csv")

# How about the 5 mins resolution
fdf <- fdf[-c(1)]
ke1 <- st_as_sf(ken)
ken1 <- ke1[,c(5,7)] %>%
  st_drop_geometry()

ken1$ID <- 1:300

# Merge with subcounty names
final_df <- merge(fdf, ken1, by.x="ID", by.y="ID", all=TRUE)

final_df1 <- final_df[c(1, 3, 5, 6, 2, 4)]
colnames(final_df1) <- c("ID", "cell", "admin 1", "admin 2",  "suit_hist", 
                         "crops")
head(final_df1)

write.csv(final_df1, "output/mapspam/suit_ken.csv")


# 23 crops out of 42 mapspam
# compare if output is similar to the one I had before
# Combine into one/ different file