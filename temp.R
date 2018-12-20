library(plyr)
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
    print("FAILED")
    return(data.frame(osm_address = address, lng = NA, lat = NA, status = "FAILED"))
  } else if(length(d) == 0) {
    print("FAILED")
    return(data.frame(osm_address = address, lng = NA, lat = NA, status = "FAILED"))
  } else {
    print("SUCCESS")
    return(data.frame(osm_address = d$display_name, lng = as.numeric(d$lon), lat = as.numeric(d$lat), status = "SUCCESS"))
  }

}
######################################################
list_address <- data_dublin$full_address

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

data_dublin_geocoded <- dplyr::bind_cols(data_dublin,list_geocodes) %>%
  dplyr::filter(!is.na(lat))
write_rds(data_dublin_geocoded,"data/data_dublin_geocoded.rds")

data_dublin_to_be_geocoded <- dplyr::anti_join(data_dublin,data_dublin_geocoded)
write_rds(data_dublin_to_be_geocoded,"data/data_dublin_to_be_geocoded.rds")

data_dublin_to_be_geocoded <- data_dublin_to_be_geocoded %>%
  tidyr::separate(Address, sep= ",", into = c("street", NA,NA), remove = TRUE) %>%
  dplyr::mutate(address_2 = paste(street, County, sep = ", "))

list_address <- data_dublin_to_be_geocoded$address_2

list_geocodes <- plyr::ldply(list_address, function(address){
  return(nominatim_osm(address))
})
########################################################
data_dublin_to_be_geocoded <- readr::read_rds(here::here("data/data_dublin_to_be_geocoded.rds"))

data_dublin_to_be_geocoded <- data_dublin_to_be_geocoded %>%
  tidyr::separate(Address, sep= ",", into = c("street", NA,NA), remove = TRUE) %>%
  dplyr::mutate(address_2 = paste(street, County, sep = ", "))

list_address <- data_dublin_to_be_geocoded$address_2

list_geocodes <- plyr::ldply(list_address, function(address){
  return(nominatim_osm(address))
})
###############################################
data_dublin <- data_raw %>%
  dplyr::filter(County == "Dublin") %>%
  tidyr::separate(Address, sep= ",", into = c("street", NA,NA), remove = TRUE) %>%
  dplyr::mutate(address_2 = paste(street, County, sep = ", "))

list_address <- data_dublin$address_2

list_geocodes <- NULL

nb_fail <- 0

repeat{
  
  for (i in list_address) {
    print(i)
    res <- nominatim_osm(i)
    Sys.sleep(1.1)
    
    if(res$status == "FAILED"){
      nb_fail <- nb_fail +1
    } else {
      nb_fail <- 0
    }
    list_geocodes <- rbind(list_geocodes, res)
    if(nb_fail > 10) {
      write_rds(list_geocodes,paste0("list_geocodes",format(Sys.time(), "%d-%m-%Y_%H-%M-%S"),".rds"))
      list_address <- tail(list_address, n=length(list_address)-nrow(list_geocodes))
      break
    }
  }
  
  if(nrow(data_dublin) == nrow(list_geocodes)){
    break()
  }
}
