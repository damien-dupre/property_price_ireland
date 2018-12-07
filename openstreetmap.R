gradian_col <- c('#3a53a4', '#5356a5', '#6a63ab', '#7d87b2','#7cac90', '#73c16a', '#68bd45',
                 '#7fc242','#9dca3c', '#d69629', '#d5972a', '#ea7125', '#ed1c24')

library(OpenStreetMap)
options(scipen=999)
map <- openmap(c(max(data_gps_sample_dublin$lat),min(data_gps_sample_dublin$lng)),
               c(min(data_gps_sample_dublin$lat),max(data_gps_sample_dublin$lng)),
               minNumTiles=10)

mapLatLon <- openproj(map)
autoplot(mapLatLon) +
  #stat_density_2d(data = data_gps_sample_dublin, geom = "raster", aes(x = lng, y = lat,fill = stat(density)), contour = FALSE) +
  #geom_point(data = data_gps_sample_dublin, mapping = aes(x = lng, y = lat)) +
  geom_density_2d(data = data_gps_sample_dublin, mapping = aes(x = lng, y = lat, colour = price)) +
  #geom_raster(data = data_gps_sample_dublin, mapping = aes(x = lng, y = lat, fill=price))+
  theme_bw()

# test <- get_openstreetmap(bbox = c(
#   left = min(data_gps_sample_dublin$lng), 
#   bottom = min(data_gps_sample_dublin$lat),
#   right = max(data_gps_sample_dublin$lng),
#   top = max(data_gps_sample_dublin$lat)
#   ))
autoplot(mapLatLon) +
  geom_point(data = data_gps_sample_dublin, aes(x = lng, y = lat, colour = price))+
  scale_colour_gradientn(colours = gradian_col, trans = "log10") +
  theme_bw()



