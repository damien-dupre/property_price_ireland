osm_feature_dist <- function(Key,Value) {
  
  variable_osm <- osmdata::opq('Dublin, Ireland') %>%
    osmdata::add_osm_feature(key = Key, value = Value)
  
  # variable_osm %>%
  #   osmdata::osmdata_sp() %>%
  #   magrittr::use_series(osm_points) %>%
  #   sp::plot()
  
  variable_sf <- variable_osm %>%
    osmdata::osmdata_sf() %>%
    magrittr::use_series(osm_lines) %>%
    magrittr::use_series(geometry) %>%
    st_sf()
  
  if (nrow(variable_sf) == 0) {
    
    return(NA)
    
  } else {
    st_crs(variable_sf) <- st_crs(4326) # assign crs
    variable_sf <- st_transform(variable_sf, crs = 32721) # transform
    variable_sf <- st_combine(variable_sf)
    
    # distance
    
    return(as.numeric(st_distance(x = data_sf, y = variable_sf)))
    
  }
  
}


for (i in 1:nrow(list_osm_features)) {
  Key <- list_osm_features[i,"Key"]
  Value <- list_osm_features[i,"Value"]
  variable_name <- paste(Key,Value,sep = "_")
  
  paste(i,variable_name) %>% print()
  # obtain gps coordinates of geographic variables (e.g., cycle lanes)

  try(
    data_dublin_2018 <- data_dublin_2018 %>% 
      dplyr::mutate(!!variable_name := osm_feature_dist(Key,Value))
  )
  
  Sys.sleep(5)
  
}
