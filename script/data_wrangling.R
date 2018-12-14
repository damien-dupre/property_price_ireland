
#################################################
#                     Setup                     #
#################################################
# store passwords
library(config)
# data wrangling
library(plyr)
library(tidyverse)
# API
library(googleway)
#################################################
#                  Load Data                    #
#################################################
data_raw <- read_csv("data/PPR-ALL.csv") %>%
  dplyr::rename(
    date_of_sale = `Date of Sale (dd/mm/yyyy)`,
    price = `Price (<U+0080>)`
    ) %>%
  dplyr::mutate(price = readr::parse_number(price)) %>%
  dplyr::mutate(date_of_sale = as.Date(date_of_sale, format = "%d/%m/%Y")) %>%
  dplyr::mutate(full_address = paste(Address, County, "Ireland")) %>%
  tibble::rowid_to_column("sale_id")
#################################################
#   Get gps from address from google api        #
#################################################
# google_key <- config::get("google_cloud")
# 
# list_address <- data_raw$full_address
# 
# df_addess <- NULL
# 
# for(address in list_address){
#   print(address)
#   dat <- googleway::google_geocode(address = address, key = google_key$key)
#   
#   if(dat$status == "ZERO_RESULTS") {
#     res <- data.frame(
#       original_address = address,
#       formatted_address = "ZERO_RESULTS",
#       lat = NA,
#       lng = NA
#     )
#   } else {
#     res <- data.frame(
#       original_address = address,
#       formatted_address = dat$results$formatted_address,
#       lat = dat$results$geometry$location$lat,
#       lng = dat$results$geometry$location$lng
#     )
#   }
#   df_addess <- rbind(df_addess, res)
# }
#################################################
#   Get gps from address from openstreetmap     #
#################################################
#https://datascienceplus.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/
data_dublin <- data_raw %>%
  dplyr::filter(County == "Dublin")

gps_dublin <- plyr::ldply(data_dublin$full_address, function(address){
  print(address)
  dat <- jsonlite::fromJSON( 
    gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
         'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
  )
  Sys.sleep(0.1)
  if(length(dat) == 0) {
    data.frame(osm_address = NA, lon = NA, lat = NA)
  } else {
    data.frame(osm_address = dat$display_name, lon = as.numeric(dat$lon), lat = as.numeric(dat$lat))
  }
})

data_dublin_gps <- dplyr::bind_cols(data_dublin, gps_dublin)
###################################################################
#geocode("125 The Pines, herbert park lane, Ballsbridge, Ireland", source="dsk")

data_gps <- plyr::ldply(data_raw$full_address, function(address){
  print(address)
  dat <- prettymapr::geocode(address, source="pickpoint", key = "zNsYY9uDwbCiDpAE6ivt")
  Sys.sleep(0.1)
  if(length(dat) == 0) {
    data.frame(osm_address = NA, lon = NA, lat = NA)
  } else {
    data.frame(osm_address = dat$address, lon = as.numeric(dat$lon), lat = as.numeric(dat$lat))
  }
}) %>%
  write_rds("data_gps.rds")
