library(plyr)
library(tidyverse)
library(jsonlite)
######################################################
data_to_be_geocoded <- readr::read_rds(here::here("data/data_to_be_geocoded.rds"))
######################################################
nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame(osm_address = NA, lng = NA, lat = NA))
  )
  if(!exists("d")) {
    return(data.frame(osm_address = address, lng = NA, lat = NA))
  } else if(length(d) == 0) {
    return(data.frame(osm_address = address, lng = NA, lat = NA))
  } else {
    return(data.frame(osm_address = d$display_name, lng = as.numeric(d$lon), lat = as.numeric(d$lat)))
  }

}
######################################################
list_address <- data_to_be_geocoded$full_address

# list_geocodes <- NULL
# 
# for (i in list_address) {
#   print(i)
#   res <- nominatim_osm(i)
#   list_geocodes <- rbind(list_geocodes, res)
# }
# write_rds(list_geocodes, "list_geocodes.rds")

list_geocodes <- plyr::ldply(list_address, function(address){
  return(nominatim_osm(address))
})