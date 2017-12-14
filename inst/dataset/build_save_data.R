require(rgdal)
require(sp)

# http://www.naturalearthdata.com/downloads/

#----------------
# countries - 50m
#----------------

# country_50m <- readOGR(dsn = "C:\\Users\\Datastorm\\Downloads\\50m_cultural",
#                        layer = "ne_50m_admin_0_countries")
#
# summary(country_50m)
#
# # keep only Europe
# country_50m <- country_50m[country_50m$continent%in% "Europe", ]
#
# # subset on some columns
# europe_countries_50m <- country_50m[, c("name", "name_long", "admin", "adm0_a3",
#                                         "adm0_a3_is","adm0_a3_us", "type",  "subunit",
#                                         "su_a3", "pop_est", "pop_year", "continent", "region_un",
#                                         "subregion",  "sovereignt")]
#
# summary(europe_countries_50m)
#
# # save as rda
# devtools::use_data(europe_countries_50m, overwrite = T)

# saveRDS(country_50m, file = "inst/dataset/maps/europe_countries_50m.RDS", overwrite = T)

#----------------
# countries - 10m
#----------------

country_10m_map <- readOGR(dsn = "C:\\Users\\Datastorm\\Downloads\\10m_cultural\\10m_cultural",
                       layer = "ne_10m_admin_0_map_units")
# 
# country_10m_ref <- readOGR(dsn = "C:\\Users\\Datastorm\\Downloads\\10m_cultural\\10m_cultural",
#                        layer = "ne_10m_admin_0_countries")

country_10m_map <- readOGR(dsn = "C:\\Users\\Datastorm\\Downloads\\50m_cultural",
                           layer = "ne_50m_admin_0_map_units")

# country_10m_ref <- readOGR(dsn = "C:\\Users\\Datastorm\\Downloads\\50m_cultural",
#                            layer = "ne_50m_admin_0_countries")
# keep only Europe + Turquie
country_10m <- country_10m_map[country_10m_map$continent%in% "Europe" | 
                                 country_10m_map$name_long %in% c("Turkey", "Cyprus"), ]
summary(country_10m)

plot(country_10m)
plot(country_10m[country_10m$name_long %in% "Cyprus",])

# chypre : fusion avec la chypre du nors
country_10m_cyprus <- country_10m_map[country_10m_map$name_long %in% c("Cyprus", "Northern Cyprus"), ]
country_10m_cyprus <- raster::aggregate(country_10m_cyprus, by = c("adm0_a3_is"))
plot(country_10m_cyprus)

slot(country_10m, "polygons")[[which(country_10m$name_long %in% "Cyprus")]] <- slot(country_10m_cyprus, "polygons")[[1]]
plot(country_10m[country_10m$name_long %in% "Cyprus",])

# remove canaries from spain
# plot(country_10m[country_10m$adm0_a3 %in% "ESP",])

pols_esp <- slot(country_10m, "polygons")[[which(country_10m$adm0_a3 %in% "ESP")]]

sum_area <- 0
# min_lat <- NA
# max_lat <- NA
# min_lon <- NA
# max_lon <- NA
keep_polygons <- sapply(country_10m[country_10m$adm0_a3 %in% "ESP", ]@polygons[[1]]@Polygons, function(x){
  # canaries : lattitude < 30
  if(x@labpt[2] > 30){
    sum_area <<- sum_area + x@area
    # min_lon <<- min(min_lon, x@coords[, 1], na.rm = T)
    # min_lat <<- min(min_lat, x@coords[, 2], na.rm = T)
    # max_lon <<- max(max_lon, x@coords[, 1], na.rm = T)
    # max_lat <<- max(max_lat, x@coords[, 2], na.rm = T)   
  }
  x@labpt[2]>30
})

# new_bbox <- matrix(c(min_lon, max_lon, min_lat, max_lat), nrow = 2, ncol = 2, byrow = T, 
#                    dimnames = list(c("x", "y"), c("min", "max")))

new_order <- country_10m[country_10m$adm0_a3 %in% "ESP", ]@polygons[[1]]@plotOrder[which(keep_polygons)] 
new_order[order(new_order)] <- 1:length(new_order)

slot(pols_esp, "area") <- sum_area
slot(pols_esp, "plotOrder") <- new_order
slot(pols_esp, "Polygons") <- country_10m[country_10m$adm0_a3 %in% "ESP", ]@polygons[[1]]@Polygons[which(keep_polygons)] 

# bug leaflet : have to reset comment...
comment(pols_esp) <- rgeos::createPolygonsComment(pols_esp)

slot(country_10m, "polygons")[[which(country_10m$adm0_a3 %in% "ESP")]] <- pols_esp 

plot(country_10m[country_10m$adm0_a3 %in% "ESP",])

# subset on columns
europe_countries_10m <- country_10m[, c("name", "admin", "adm0_a3",
                                        "adm0_a3_is","adm0_a3_us",
                                        "type",  "subunit",
                                        "continent", "region_un",
                                        "subregion",  "sovereignt")]
summary(europe_countries_10m)
plot(europe_countries_10m)

names(europe_countries_10m) <- gsub("^adm0_a3$", "code", names(europe_countries_10m))

# ref table
europe_countries_ref <- unique(data.frame(europe_countries_10m[, c("admin", "code")],
                                     stringsAsFactors = F))
colnames(europe_countries_ref) <- c("name", "code")


#----------------
# states - 10m
#----------------
#
# states_10m <- readOGR(dsn = "C:\\Users\\Datastorm\\Downloads\\10m_cultural\\10m_cultural",
#                       layer = "ne_10m_admin_1_states_provinces_shp")

states_10m <- readOGR(dsn = "C:\\Users\\Datastorm\\Downloads\\10m_cultural\\10m_cultural",
                      layer = "ne_10m_admin_1_states_provinces_lakes_shp")
# subset on Europe
states_10m_europe <- states_10m[states_10m$sr_adm0_a3%in% europe_countries_10m$code | 
                                  states_10m$admin %in% c("Cyprus", "Northern Cyprus"), ]
summary(states_10m_europe)

table(states_10m_europe$type_en)
plot(states_10m_europe)

plot(states_10m_europe[states_10m_europe$admin %in% "Cyprus", ])
plot(states_10m_europe[states_10m_europe$admin %in% c("Cyprus", "Northern Cyprus"), ])

summary(states_10m_europe[states_10m_europe$admin %in% c("Cyprus", "Northern Cyprus"), ])

# chypre : fusion avec la chypre du nors
levels(states_10m_europe$admin) <- gsub("Northern Cyprus", "Cyprus", levels(states_10m_europe$admin))
levels(states_10m_europe$sr_adm0_a3) <- gsub("^CYN$", "CYP", levels(states_10m_europe$sr_adm0_a3))

plot(states_10m_europe[states_10m_europe$admin %in% c("Cyprus"), ])

# remove islands from france
plot(states_10m_europe[states_10m_europe$sr_adm0_a3 %in% "FRA", ])
states_10m_europe <- states_10m_europe[!(states_10m_europe$sr_adm0_a3 %in% "FRA" &  !states_10m_europe$type_en %in% "Region"), ]
plot(states_10m_europe[states_10m_europe$sr_adm0_a3 %in% "FRA", ])

# N0R
plot(states_10m_europe[states_10m_europe$sr_adm0_a3 %in% "NOR", ])
states_10m_europe <- states_10m_europe[!(states_10m_europe$sr_adm0_a3 %in% "NOR" & !states_10m_europe$type_en %in% "County"), ]
plot(states_10m_europe[states_10m_europe$sr_adm0_a3 %in% "NOR", ])

# NLD
plot(states_10m_europe[states_10m_europe$sr_adm0_a3 %in% "NLD", ])
states_10m_europe <- states_10m_europe[!(states_10m_europe$sr_adm0_a3 %in% "NLD" & !states_10m_europe$type_en %in% "Province"), ]
plot(states_10m_europe[states_10m_europe$sr_adm0_a3 %in% "NLD", ])

# ESP
plot(states_10m_europe[states_10m_europe$sr_adm0_a3 %in% "ESP", ])
states_10m_europe <- states_10m_europe[!(states_10m_europe$sr_adm0_a3 %in% "ESP" & states_10m_europe$name %in% "ESP-00 (Canary Is. aggregation)"), ]
plot(states_10m_europe[states_10m_europe$sr_adm0_a3 %in% "ESP", ])

# for(co in europe_countries_ref$code){
#   print(co)
#   par(ask = T)
#   plot(states_10m_europe[!states_10m_europe$sr_adm0_a3 %in% co, ])
# }

# subset on columns
europe_states_provinces_10m <- states_10m_europe[, c("admin", "sr_adm0_a3", "sr_sov_a3", "adm1_code",
                                                     "name", "type", "type_en", "region")]
summary(europe_states_provinces_10m)

names(europe_states_provinces_10m) <- gsub("^sr_adm0_a3$", "code", names(europe_states_provinces_10m))

devtools::use_data(europe_countries_10m, europe_countries_ref, 
                   europe_states_provinces_10m, internal = TRUE, overwrite = T)


#------------------------------------------------------------------------------------------#

library(antaresViz)
library(spMaps)

##comparaison de plusieurs graphiques
pathS2<-"C:\\Users\\Datastorm\\Desktop\\antares\\test_case"
setSimulationPath(pathS2,-1)
myData1<-readAntares(areas = "all", links = "all")

ml<-mapLayout(readLayout())
plotMap(myData1, ml)

plotMapLayout(ml)

#-------------------
# Identify leaflet bug with Spain
#-------------------

# require(leaflet)
# 
# map=getSpMaps(countries = c("ESP"))
# 
# str(map[map$name %in% 'France',])
# str(map[map$name %in% 'Spain',])
# 
# s <- sp::polygons(map)
# 
# plot(s)
# leaflet(map) %>% addPolygons()
# 
# leaflet:::derivePolygons
# leaflet:::polygonData.SpatialPolygons
# leaflet:::polygonData(s)
# 
# leaflet:::sp_bbox(s)
# leaflet:::to_multipolygon_list.SpatialPolygons
# 
# pgons <- s@polygons[[1]]
# comment(pgons)
# rgeos::createPolygonsComment(pgons)
# leaflet:::to_multipolygon.Polygons
# lapply(pgons@polygons, to_multipolygon)
# 
# str(s)
# s@polygons
# pgons = leaflet:::derivePolygons(map, lng = NULL, lat = NULL, T, T, 
#                        "addPolygons")
# 