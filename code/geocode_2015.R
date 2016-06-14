# library(ggmap) # GOOGLELLA
# load("./data/sdmr_4_english_language_versionL.RData")
# locations <- as.character(unique(d15l$PUNKTc1))
# # Remove numeric locations
# locations <- locations[!grepl("[0-9]",locations)]
# 
# location_data <- data.frame()
# for (lc in locations){
#   loc <- as.data.frame(ggmap::geocode(location = lc, source ="google"))
#   loc$PUNKTc1 <- lc
#   location_data <- rbind(location_data,loc)
# }
# 
# # Add coordinates for locations google could not locate
# ## 1: geocode failed with status ZERO_RESULTS, location = "Липецкая область, Добровский рай" 
# ## 2: geocode failed with status ZERO_RESULTS, location = "Калининградская область, пгт Янт" 
# ## 3: geocode failed with status ZERO_RESULTS, location = "Новосибирская область, пгт Посев" 
# ## 4: geocode failed with status ZERO_RESULTS, location = "Новгородская область, пгт Пролет" 
# ## 5: geocode failed with status ZERO_RESULTS, location = "Челябинская область, пгт Первома" 
# 
# manual_locations <- read.table(textConnection('
# lon     lat     PUNKTc1
# 52.42   39.09   "Липецкая область, Добровский рай"
# 54.4201 20.2711 "Калининградская область, пгт Янт"
# 55.27   79.33   "Новосибирская область, пгт Посев"
# 58.26   32.23   "Новгородская область, пгт Пролет"
# 55.1547 61.3758 "Челябинская область, пгт Первома"
# 
# '),header=TRUE, stringsAsFactors=FALSE)
# names(manual_locations) <- c("lat","lon","PUNKTc1")
# manual_locations <- manual_locations[c(2,1,3)]
# 
# location_data2 <- rbind(location_data,manual_locations)
# location_df <- na.omit(location_data2)
# save(location_df, file="./data/location_df.RData")
load("./data/location_df.RData")


load("./data/sdmr_4_english_language_versionL.RData")
n_cases <- d15l %>% group_by(PUNKTc1) %>% 
  summarise(n = n()) %>% 
  filter(!grepl("[0-9]",PUNKTc1))

location_df <- merge(location_df,n_cases,by="PUNKTc1")
save(location_df, file="./data/location_df.RData")

