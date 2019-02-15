################################################################################

test_gps <- data_gps_sample_dublin_clean[1,]
point <- c(test_gps$lat,test_gps$lng)
pt1 = st_point(point)

#st_crs(pt1) <- st_crs(4326) # assign crs
################################################################################
#poly_3 <- ireland_smallarea@polygons[[3]]

polygon <- st_as_sf(ireland_smallarea)

test <- polygon$geometry[1]

intersection <- st_intersection(x = test, y = point)
################################################################################
################################################################################
points.df <- data.frame(
  'lat' = test_gps$lat,
  'lng' = test_gps$lng
)
points.sf <- st_as_sf(points.df, coords = c("lat", "lng"))

st_crs(points.sf) <- st_crs(4326) # assign crs
points.sf <- st_transform(points.sf, crs = 32721) # transform
################################################################################
polygon <- st_as_sf(ireland_smallarea)
st_crs(polygon) <- st_crs(4326) # assign crs
polygon <- st_transform(polygon, crs = 32721) # transform

#distances <- st_intersection(y = points.sf, x = polygon)
################################################################################
################################################################################
#https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package
polygon_GEOGID1 <- polygon[polygon$GEOGID == "A017001001",]
polygon_GEOGID1 <- st_transform(polygon_GEOGID1, 32721)



st_intersects(points.sf, polygon_GEOGID1, sparse = FALSE)
################################################################################
################################################################################
#true
points.df <- data.frame(
  'lat' = 52.733128, 
  'lng' = -6.915340
)
#false
points.df <- data.frame(
  'lat' = 53.350140, 
  'lng' = -6.266155
)
points.sf <- st_as_sf(points.df, coords = c("lng","lat"))
st_crs(points.sf) <- st_crs(4326) # assign crs
points.sf <- st_transform(points.sf, crs = 32721) # transform

st_intersects(points.sf, polygon_GEOGID1, sparse = FALSE)

polygon_GEOGID1[which(st_intersects(points.sf, polygon_GEOGID1, sparse = FALSE)), ]$GEOGID
################################################################################
################################################################################
#true
points.df <- data.frame(
  'lat' = 52.733128, 
  'lng' = -6.915340
)
points.sf <- st_as_sf(points.df, coords = c("lng","lat"))
st_crs(points.sf) <- st_crs(4326) # assign crs
points.sf <- st_transform(points.sf, crs = 32721) # transform

polygon <- st_as_sf(ireland_smallarea)
st_crs(polygon) <- st_crs(4326) # assign crs
polygon_ireland <- st_transform(polygon, 32721)

polygon_ireland[which(st_intersects(points.sf, polygon_ireland, sparse = FALSE)), ]$GEOGID
################################################################################
################################################################################
polygon <- st_as_sf(ireland_smallarea)
st_crs(polygon) <- st_crs(4326) # assign crs
polygon_ireland <- st_transform(polygon, 32721)

pnts <- data_gps_sample_dublin_clean %>% 
  dplyr::select(lat,lng)

pnts$GEOGID <- apply(pnts, 1, function(row) {  
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
