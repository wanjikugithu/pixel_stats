library(dplyr)

stats <- read.csv("gaez/data/gaez_stats.csv")
mapspam <- read.csv("gaez/data/mapspam_crops.csv")
met <- read.csv("gaez/data/metadata_res05.csv")
met <- met %>%
  filter(sub_theme_name == "Suitability Index") %>%
  select(name, file_description)

# Suitability divide by 10000
stats$suit_hist <- stats$suit_hist/10000

# Split to add category description
met$cats <- strsplit(met$name, "_")
met$cats <- sapply(met$cats, "[[", 1)

# Get description of the unique categories for editing
met <- met %>%
  filter(cats == grep(pattern="^su", x=cats, value=TRUE))

met1 <- met %>% 
  distinct(cats, .keep_all = TRUE)

write.csv(met1, "gaez/data/cats.csv")
 
# Add categories description  
cats <- read.csv("gaez/data/cats.csv")

stats <- merge(stats, cats, by.x="category", by.y = "cats")
stats <- stats[-c(2,13)]

# Match with mapspam name
m <- sort(unique(mapspam$fullname))
g <- sort(unique(stats$full_name))

stats$full_name <- tolower(stats$full_name)
names(stats)[10] <- "gaez_name"
stats$mname <- stats$gaez_name

# Replace to match with mapspam
stats$mname[stats$mname == "phaseolus bean"] <- "bean"
stats$mname[stats$mname == "oil palm"] <- "oilpalm"
stats$mname[stats$mname == "biomass sorghum"] <- "sorghum"
stats$mname[stats$mname == "sugar beet"] <- "sugarbeet"
stats$mname[stats$mname == "sugar cane"] <- "sugarcane"
stats$mname[stats$mname == "white potato"] <- "potato"
stats$mname[stats$mname == "yam"] <- "yams"

mapspam$fullname[mapspam$fullname == "arabica coffee"] <- "coffee"
mapspam$fullname[mapspam$fullname == "robusta coffee"] <- "coffee"
mapspam <- mapspam[-c(1,4,5)]
mapspam <- mapspam[-33,]
mapspam$partial_name[mapspam$partial_name == "acof"] <- "acof/rcof"

# Pick max if name == 2 or more varieties
# rice and coffee
# for every id & category pick the max value

stats1 <- stats %>%
  filter(gaez_name == "dryland rice" | gaez_name == "wetland rice") %>%
  group_by(category, ID) %>%
  summarise(max(suit_hist))

stats1 <- merge(stats1, kef, by.x="ID", by.y="id")
stats1$mname <- "rice"

stats1 <- merge(stats1, cats, by.x="category", by.y = "cats")
stats1 <- stats1[-10]
stats1$gaez_name <- "rice"
stats1$crops <- "rcw/rcd max value"
stats1 <- stats1[c(1,2,4:8,3,12,11,10,9)]
names(stats1)[8] <- "suit_hist"

# Remove rice from stats
stats <- stats %>%
  filter(!gaez_name == "dryland rice") %>%
  filter(!gaez_name == "wetland rice")

stats <- stats[-11]

# cbind stats and stat1
fstats <- rbind(stats, stats1)

# Remove those that are not available in mapspam
# Final merge
# coffee thing
final_stats <- merge(fstats, mapspam, by.x="mname", by.y="fullname")
final_stats <- final_stats[c(3:9,1,11,13,10,2,12)]
names(final_stats)[8] <- "mapspam_name"
names(final_stats)[10] <- "mapspam"
names(final_stats)[11] <- "gaez"
names(final_stats)[13] <- "category_description"
final_stats <- final_stats[-c(10,11)]

write.csv(final_stats, "gaez/gaez_lowinput11.csv")

df <- read.csv("gaez/gaez_low_input.csv")

fdf <- df %>%
  group_by(ID, mapspam_name) %>%
  summarise(max(suit_hist))

fdf <- fdf %>% 
  pivot_wider(id_cols=ID, names_from = mapspam_name, values_from = `max(suit_hist)`)

# Merge with kef
ff <- merge(fdf, kef, by.x="ID", by.y="id")
ff <- ff[c(1, 30:34, 2:29)]

write.csv(ff, "gaez/output/gaez_low.csv")

# Combined
cc <- rbind(df, high)

fdf <- cc %>%
  group_by(ID, mapspam_name) %>%
  summarise(max(suit_hist))

fdf <- fdf %>% 
  pivot_wider(id_cols=ID, names_from = mapspam_name, values_from = `max(suit_hist)`)

# Merge with kef
ff <- merge(fdf, kef, by.x="ID", by.y="id")
ff <- ff[c(1, 30:34, 2:29)]

write.csv(ff, "gaez/output/gaez_combined.csv")

# Check similarity
low <- read.csv("gaez/output/gaez_low.csv")
high <- read.csv("gaez/output/gaez_high.csv")
high1 <- high
combined <- read.csv("gaez/output/gaez_combined.csv")
