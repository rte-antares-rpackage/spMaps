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

country_10m_map <- sf::st_read(dsn = "D:\\Users\\mahoudiabd\\Downloads\\Datastorm",
                               layer = "ne_50m_ADMIN_0_map_units")

# country_10m_ref <- st_read(dsn = "C:\\Users\\Datastorm\\Downloads\\50m_cultural",
#                            layer = "ne_50m_ADMIN_0_countries")
# keep only Europe + Turquie
country_10m <- country_10m_map[country_10m_map$CONTINENT %in% "Europe" |
                                 country_10m_map$NAME_LONG %in% c("Turkey", "Cyprus"), ]
summary(country_10m)

plot(country_10m)
plot(country_10m[country_10m$NAME_LONG %in% "Cyprus",])

# chypre : fusion avec la chypre du nors
########

# Subset data
country_cyprus <- country_10m_map[country_10m_map$NAME_LONG %in% c("Cyprus", "Northern Cyprus"), ]# Perform a union of geometries
union_geometry <- st_union(country_cyprus$geometry)# Replace the geometry of the "Cyprus" feature in the original dataset
country_10m[country_10m$NAME_LONG == "Cyprus", "geometry"] <- union_geometry

# Plot the updated dataset
plot(country_10m[country_10m$NAME_LONG %in% "Cyprus", ])


###########

#slot(country_10m, "polygons")[[which(country_10m$NAME_LONG %in% "Cyprus")]] <- slot(country_10m_cyprus, "polygons")[[1]]

# remove canaries from spain
# plot(country_10m[country_10m$ADM0_A3 %in% "ESP",])

pols_esp <- country_10m[country_10m$ADM0_A3 == "ESP", ]

#########

# Assuming country_10m is your sf object
spain <- country_10m[country_10m$ADM0_A3 %in% "ESP", ]
# Extract the geometries and calculate sum area
spain_polygons <- spain$geometry
sum_area <- sum(st_area(spain_polygons[st_coordinates(st_centroid(spain_polygons))[ ,2] > 30]))
# Calculate new order
new_order <- seq_along(spain_polygons[st_coordinates(st_centroid(spain_polygons))[ ,2] > 30])
# Update the order of the geometries
spain_polygons[st_coordinates(st_centroid(spain_polygons))[ ,2] > 30] <- spain_polygons[new_order]
# Update the area attribute
spain$area <- sum_area
# Update the geometry
spain$geometry <- spain_polygons
# Plot the modified polygons
plot(spain)

#########

# subset on columns
europe_countries_10m <- country_10m[, c("NAME", "ADMIN", "ADM0_A3",
                                        "ADM0_A3_US",
                                        "TYPE",  "SUBUNIT",
                                        "CONTINENT", "REGION_UN",
                                        "SUBREGION",  "SOVEREIGNT")]
summary(europe_countries_10m)
plot(europe_countries_10m)

names(europe_countries_10m) <- gsub("^ADM0_A3$", "CODE", names(europe_countries_10m))

# ref table
europe_countries_ref <- unique(data.frame(europe_countries_10m[, c("ADMIN", "CODE")],
                                     stringsAsFactors = F))
colnames(europe_countries_ref) <- c("NAME", "CODE")


#----------------
# states - 10m
#----------------
#
# states_10m <- st_read(dsn = "C:\\Users\\Datastorm\\Downloads\\10m_cultural\\10m_cultural",
#                       layer = "ne_10m_ADMIN_1_states_provinces_shp")

states_10m <- st_read(dsn = "D:\\Users\\mahoudiabd\\Downloads\\Datastorm",
                      layer = "ne_10m_ADMIN_1_states_provinces_lakes")
# subset on Europe

states_10m_europe <- states_10m[states_10m$adm0_a3%in% europe_countries_10m$CODE | 
                                  states_10m$admin %in% c("Cyprus", "Northern Cyprus"), ]
summary(states_10m_europe)

table(states_10m_europe$type_en)
plot(states_10m_europe)

plot(states_10m_europe[states_10m_europe$admin %in% "Cyprus", ])
plot(states_10m_europe[states_10m_europe$admin %in% c("Cyprus", "Northern Cyprus"), ])

summary(states_10m_europe[states_10m_europe$admin %in% c("Cyprus", "Northern Cyprus"), ])

# chypre : fusion avec la chypre du nors
levels(states_10m_europe$admin) <- gsub("Northern Cyprus", "Cyprus", levels(states_10m_europe$admin))
levels(states_10m_europe$adm0_a3) <- gsub("^CYN$", "CYP", levels(states_10m_europe$adm0_a3))

plot(states_10m_europe[states_10m_europe$admin %in% c("Cyprus"), ])

# remove islands from france
plot(states_10m_europe[states_10m_europe$adm0_a3 %in% "FRA", ])
states_10m_europe <- states_10m_europe[!(states_10m_europe$adm0_a3 %in% "FRA" &  !states_10m_europe$type_en %in% "Region"), ]
plot(states_10m_europe[states_10m_europe$adm0_a3 %in% "FRA", ])

# N0R
plot(states_10m_europe[states_10m_europe$adm0_a3 %in% "NOR", ])
states_10m_europe <- states_10m_europe[!(states_10m_europe$adm0_a3 %in% "NOR" & !states_10m_europe$type_en %in% "County"), ]
plot(states_10m_europe[states_10m_europe$adm0_a3 %in% "NOR", ])

# NLD
plot(states_10m_europe[states_10m_europe$adm0_a3 %in% "NLD", ])
states_10m_europe <- states_10m_europe[!(states_10m_europe$adm0_a3 %in% "NLD" & !states_10m_europe$type_en %in% "Province"), ]
plot(states_10m_europe[states_10m_europe$adm0_a3 %in% "NLD", ])

# for(co in europe_countries_ref$CODE){
#   print(co)
#   par(ask = T)
#   plot(states_10m_europe[!states_10m_europe$adm0_a3 %in% co, ])
# }

# subset on columns
europe_states_provinces_10m <- states_10m_europe[, c("admin", "adm0_a3", "sov_a3", "adm1_code",
                                                     "name", "type", "type_en", "region")]
summary(europe_states_provinces_10m)

names(europe_states_provinces_10m) <- gsub("^adm0_a3$", "code", names(europe_states_provinces_10m))


## Export des tables vers le dossier inst, ne pas lancer si on ne souhaite pas écraser les données.
#devtools::use_data(europe_countries_10m, europe_countries_ref, europe_states_provinces_10m, internal = TRUE, overwrite = T)


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