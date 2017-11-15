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

country_10m_ref <- readOGR(dsn = "C:\\Users\\Datastorm\\Downloads\\50m_cultural",
                           layer = "ne_50m_admin_0_countries")
# keep only Europe
country_10m <- country_10m_map[country_10m_map$continent%in% "Europe", ]
summary(country_10m)

plot(country_10m)

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

# ref table
country_ref <- country_10m_ref[country_10m_ref$continent%in% "Europe", ]
europe_countries_ref <- data.frame(country_ref[, c("name", "adm0_a3")],
                                   stringsAsFactors = F)

colnames(europe_countries_ref)[2] <- c("code")


#----------------
# states - 10m
#----------------
#
# states_10m <- readOGR(dsn = "C:\\Users\\Datastorm\\Downloads\\10m_cultural\\10m_cultural",
#                       layer = "ne_10m_admin_1_states_provinces_shp")

states_10m <- readOGR(dsn = "C:\\Users\\Datastorm\\Downloads\\10m_cultural\\10m_cultural",
                      layer = "ne_10m_admin_1_states_provinces_lakes_shp")
# subset on Europe
states_10m_europe <- states_10m[states_10m$sr_adm0_a3%in% europe_countries_10m$adm0_a3, ]
summary(states_10m_europe)

table(states_10m_europe$type_en)
plot(states_10m_europe)

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

devtools::use_data(europe_countries_10m, europe_countries_ref, 
                   europe_states_provinces_10m, internal = TRUE, overwrite = T)


#------------------------------------------------------------------------------------------#

library(antaresViz)
library(antaresMaps)

##comparaison de plusieurs graphiques
pathS2<-"C:\\Users\\Datastorm\\Desktop\\antares\\test_case"
setSimulationPath(pathS2,-1)
myData1<-readAntares(areas = "all", links = "all")

ml<-mapLayout(readLayout())
plotMap(myData1, ml)

#-------------------
# Identify leaflet bug with Spain
#-------------------

# require(leaflet)
# ?leaflet

# map=getAntaresMap(countries = c("ESP"))
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