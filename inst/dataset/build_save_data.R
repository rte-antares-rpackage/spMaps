require(sf)
require(sp)

#----------------
# countries - 10m
#----------------


country_10m_map <- sf::st_read(dsn = "D:\\Users\\mahoudiabd\\Downloads\\Datastorm",
                               layer = "ne_10m_ADMIN_0_map_units")
# 
# country_10m_ref <- readOGR(dsn = "C:\\Users\\Datastorm\\Downloads\\10m_cultural\\10m_cultural",
#                        layer = "ne_10m_ADMIN_0_countries")

country_10m_map <- sf::st_read(dsn = "D:\\Users\\mahoudiabd\\Downloads\\Datastorm",layer = "ne_50m_ADMIN_0_map_units")

# country_10m_ref <- st_read(dsn = "C:\\Users\\Datastorm\\Downloads\\50m_cultural",
#                            layer = "ne_50m_ADMIN_0_countries")
# keep only Europe + Turquie
country_10m <- country_10m_map[country_10m_map$CONTINENT %in% "Europe" |
                                 country_10m_map$NAME_LONG %in% c("Turkey", "Cyprus"), ]
summary(country_10m)

plot(country_10m)
plot(country_10m[country_10m$NAME_LONG %in% "Cyprus",])

# chypre : fusion avec la chypre du nors
country_10m_cyprus <- country_10m_map[country_10m_map$NAME_LONG %in% c("Cyprus", "Northern Cyprus"), ]
country_10m_cyprus <- sf::st_cast(country_10m_cyprus, "MULTIPOLYGON", group = "ADM0_A3_is")

plot(country_10m_cyprus)

slot(country_10m, "polygons")[[which(country_10m$NAME_LONG %in% "Cyprus")]] <- slot(country_10m_cyprus, "polygons")[[1]]
plot(country_10m[country_10m$NAME_LONG %in% "Cyprus",])

# remove canaries from spain
# plot(country_10m[country_10m$ADM0_A3 %in% "ESP",])

pols_esp <- slot(country_10m, "polygons")[[which(country_10m$ADM0_A3 %in% "ESP")]]

sum_area <- 0
# min_lat <- NA
# max_lat <- NA
# min_lon <- NA
# max_lon <- NA
keep_polygons <- sapply(country_10m[country_10m$ADM0_A3 %in% "ESP", ]@polygons[[1]]@Polygons, function(x){
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
#                    dimNAMEs = list(c("x", "y"), c("min", "max")))

new_order <- country_10m[country_10m$ADM0_A3 %in% "ESP", ]@polygons[[1]]@plotOrder[which(keep_polygons)] 
new_order[order(new_order)] <- 1:length(new_order)

slot(pols_esp, "area") <- sum_area
slot(pols_esp, "plotOrder") <- new_order
slot(pols_esp, "Polygons") <- country_10m[country_10m$ADM0_A3 %in% "ESP", ]@polygons[[1]]@Polygons[which(keep_polygons)] 

slot(country_10m, "polygons")[[which(country_10m$ADM0_A3 %in% "ESP")]] <- pols_esp 

plot(country_10m[country_10m$ADM0_A3 %in% "ESP",])

# subset on columns
europe_countries_10m <- country_10m[, c("NAME", "ADMIN", "ADM0_A3",
                                        "ADM0_A3_IS","ADM0_A3_US",
                                        "TYPE",  "SUBUNIT",
                                        "CONTINENT", "REGION_UN",
                                        "SUBREGION",  "SOVEREIGNT")]
summary(europe_countries_10m)
plot(europe_countries_10m)

NAMEs(europe_countries_10m) <- gsub("^ADM0_A3$", "CODE", NAMEs(europe_countries_10m))

# ref table
europe_countries_ref <- unique(data.frame(europe_countries_10m[, c("ADMIN", "CODE")],
                                     stringsAsFactors = F))
colNAMEs(europe_countries_ref) <- c("NAME", "CODE")


#----------------
# states - 10m
#----------------
#
# states_10m <- st_read(dsn = "C:\\Users\\Datastorm\\Downloads\\10m_cultural\\10m_cultural",
#                       layer = "ne_10m_ADMIN_1_states_provinces_shp")

states_10m <- st_read(dsn = "C:\\Users\\Datastorm\\Downloads\\10m_cultural\\10m_cultural",
                      layer = "ne_10m_ADMIN_1_states_provinces_lakes_shp")
# subset on Europe
states_10m_europe <- states_10m[states_10m$sr_ADM0_A3%in% europe_countries_10m$CODE | 
                                  states_10m$ADMIN %in% c("Cyprus", "Northern Cyprus"), ]
summary(states_10m_europe)

table(states_10m_europe$TYPE_en)
plot(states_10m_europe)

plot(states_10m_europe[states_10m_europe$ADMIN %in% "Cyprus", ])
plot(states_10m_europe[states_10m_europe$ADMIN %in% c("Cyprus", "Northern Cyprus"), ])

summary(states_10m_europe[states_10m_europe$ADMIN %in% c("Cyprus", "Northern Cyprus"), ])

# chypre : fusion avec la chypre du nors
levels(states_10m_europe$ADMIN) <- gsub("Northern Cyprus", "Cyprus", levels(states_10m_europe$ADMIN))
levels(states_10m_europe$sr_ADM0_A3) <- gsub("^CYN$", "CYP", levels(states_10m_europe$sr_ADM0_A3))

plot(states_10m_europe[states_10m_europe$ADMIN %in% c("Cyprus"), ])

# remove islands from france
plot(states_10m_europe[states_10m_europe$sr_ADM0_A3 %in% "FRA", ])
states_10m_europe <- states_10m_europe[!(states_10m_europe$sr_ADM0_A3 %in% "FRA" &  !states_10m_europe$TYPE_en %in% "Region"), ]
plot(states_10m_europe[states_10m_europe$sr_ADM0_A3 %in% "FRA", ])

# N0R
plot(states_10m_europe[states_10m_europe$sr_ADM0_A3 %in% "NOR", ])
states_10m_europe <- states_10m_europe[!(states_10m_europe$sr_ADM0_A3 %in% "NOR" & !states_10m_europe$TYPE_en %in% "County"), ]
plot(states_10m_europe[states_10m_europe$sr_ADM0_A3 %in% "NOR", ])

# NLD
plot(states_10m_europe[states_10m_europe$sr_ADM0_A3 %in% "NLD", ])
states_10m_europe <- states_10m_europe[!(states_10m_europe$sr_ADM0_A3 %in% "NLD" & !states_10m_europe$TYPE_en %in% "Province"), ]
plot(states_10m_europe[states_10m_europe$sr_ADM0_A3 %in% "NLD", ])

# ESP
plot(states_10m_europe[states_10m_europe$sr_ADM0_A3 %in% "ESP", ])
states_10m_europe <- states_10m_europe[!(states_10m_europe$sr_ADM0_A3 %in% "ESP" & states_10m_europe$NAME %in% "ESP-00 (Canary Is. aggregation)"), ]
plot(states_10m_europe[states_10m_europe$sr_ADM0_A3 %in% "ESP", ])

# for(co in europe_countries_ref$CODE){
#   print(co)
#   par(ask = T)
#   plot(states_10m_europe[!states_10m_europe$sr_ADM0_A3 %in% co, ])
# }

# subset on columns
europe_states_provinces_10m <- states_10m_europe[, c("ADMIN", "sr_ADM0_A3", "sr_sov_a3", "adm1_CODE",
                                                     "NAME", "TYPE", "TYPE_en", "region")]
summary(europe_states_provinces_10m)

NAMEs(europe_states_provinces_10m) <- gsub("^sr_ADM0_A3$", "CODE", NAMEs(europe_states_provinces_10m))

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