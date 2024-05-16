# Mapspam format -- dbf or csv
library(foreign)
library(dplyr)
library(readr)

xx <- read.csv("C:/Users/Wanjiku/Downloads/spam2017v2r1_ssa_csv/spam2017V2r1_SSA_A_TA.csv")
xy <- read.csv("C:/Users/Wanjiku/Downloads/spam2017v2r1_ssa_csv/spam2017V2r1_SSA_A_TL.csv")
xz <- read.csv("C:/Users/Wanjiku/Downloads/spam2017v2r1_ssa_csv/spam2017V2r1_SSA_P_TL.csv")
  
z <- read_csv("C:/Users/Wanjiku/Downloads/spam2010v2r0_global_phys_area_csv/spam2010V2r0_global_A_TH.csv")

zz <- z %>%
  filter(iso3 == "KEN")


y <- read_csv(file="C:/Users/Wanjiku/Downloads/spam2010v2r0_global_phys_area_csv/spam2010V2r0_global_A_TS.csv")
yy <- y %>%
  filter(iso3 == "KEN")
df <- read.dbf(file="C:/Users/Wanjiku/Downloads/spam2017v2r1_ssa_dbf/spam2017V2r1_SSA_V_TS.dbf")
df1 <- read.dbf(file="C:/Users/Wanjiku/Downloads/spam2017v2r1_fips_ssa_level1_dbf/Spam2017V2r1_FIPS_SSA_level1_H_TA.dbf")

ke <- df %>%
  select(ISO3, ALLOC_KEY, CELL5M, X, Y) %>%
  filter(ISO3 == "KEN")

ke <- xx %>%
  select(iso3, alloc_key, cell5m, x, y) %>%
  filter(iso3 == "KEN")

ke1 <- xz %>%
  select(iso3, alloc_key, cell5m, x, y) %>%
  filter(iso3 == "KEN")

ke <- ke[c(4:5)]

ke1 <- xx %>%
  filter(iso3 == "KEN")


