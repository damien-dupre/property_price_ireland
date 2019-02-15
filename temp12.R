data_dublin_2018_osm_features <-
  readr::read_rds(here::here("data/data_dublin_2018_osm_features.rds")) %>%
  dplyr::mutate(year_fact = as.factor(as.character(year)))
#
ireland_smallarea <- rgdal::readOGR(here::here("data/Small_Areas__Generalised_20m__OSi_National_Boundaries.geojson"))

polygon <- st_as_sf(ireland_smallarea)
st_crs(polygon) <- st_crs(4326) # assign crs
polygon_ireland <- st_transform(polygon, 32721)

pnts <- data_dublin_2018_osm_features %>% 
  dplyr::select(lat,lng)

data_dublin_2018_osm_features$GEOGID <- apply(pnts, 1, function(row) {  
  # transformation to palnar is required, since sf library assumes planar projection 
  
  points.df <- data.frame(
    'lat' = row[1], 
    'lng' = row[2]
  )
  points.sf <- st_as_sf(points.df, coords = c("lng","lat"))
  st_crs(points.sf) <- st_crs(4326) # assign crs
  points.sf <- st_transform(points.sf, crs = 32721)
  
  ID <- polygon_ireland[which(st_intersects(points.sf, polygon_ireland, sparse = FALSE)), ]$GEOGID
  print(ID)
  ID
})
write_rds(data_dublin_2018_osm_features,"data/data_dublin_2018_osm_features.rds")
