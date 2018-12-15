library(tidyverse)
library(jsonlite)
######################################################
data_raw <- readr::read_csv(here::here("data/PPR-ALL.csv")) %>%
  dplyr::rename(
    date_of_sale = `Date of Sale (dd/mm/yyyy)`,
    price = `Price (<U+0080>)`
  ) %>%
  dplyr::mutate(price = readr::parse_number(price)) %>%
  dplyr::mutate(date_of_sale = as.Date(date_of_sale, format = "%d/%m/%Y")) %>%
  dplyr::mutate(full_address = paste(Address, County, "Ireland")) %>%
  tibble::rowid_to_column("sale_id") %>%
  dplyr::mutate(year = lubridate::year(date_of_sale))

data_dublin <- data_raw %>%
  dplyr::filter(County == "Dublin")
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
  print(res)
  list_geocodes <- rbind(list_geocodes, res)
}
write_rds(list_geocodes, "list_geocodes.rds")
  