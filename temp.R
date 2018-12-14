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
list_address <- data_dublin$full_address

list_geocodes <- NULL

for (i in list_address) {
  print(i)
  res <- nominatim_osm(i)
  list_geocodes <- rbind(list_geocodes, res)
}

