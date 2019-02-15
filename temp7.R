# osmdata::add_osm_feature(key = 'building', value = 'transportation') = 29%
# osmdata::add_osm_feature(key = 'landuse', value = 'commercial') = 27.2%
variable_osm <- osmdata::opq('Dublin, Ireland') %>%
  osmdata::add_osm_feature(key = 'leisure', value = 'park')
  
variable_osm %>% 
  osmdata::osmdata_sp() %>% 
  magrittr::use_series(osm_points) %>% 
  sp::plot()

variable_sf <- variable_osm %>% 
  osmdata::osmdata_sf() %>%
  magrittr::use_series(osm_points) %>% 
  magrittr::use_series(geometry) %>% 
  st_sf()

st_crs(variable_sf) <- st_crs(4326) # assign crs
variable_sf <- st_transform(variable_sf, crs = 32721) # transform
variable_sf <- st_combine(variable_sf)

plot(variable_sf)

# distance
data_dublin_geocoded_clean$variable <- as.numeric(st_distance(x = data_sf, y = variable_sf))

gam_dublin_gps <- mgcv::gam(
  price ~ s(lng, lat, bs = "so", xt = list(bnd = bound)) + s(variable), 
  data = subset(data_dublin_geocoded_clean,year == 2010), 
  method = "REML", 
  knots = knots,
  #control=list(trace = TRUE),
  control = gam.control(nthreads = 3),
  family=gaussian(link = "identity"))

summary(gam_dublin_gps)











