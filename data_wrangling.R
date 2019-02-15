#########################################################
#                 filter disjoint gps                   #
#########################################################
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

data_dublin_geocoded <- readr::read_rds(here::here("data/data_dublin_geocoded.rds")) %>%
  dplyr::filter(status == "SUCCESS") %>%
  dplyr::filter(!grepl('apartment|Apartment|APART|APT|Apt', Address)) %>%
  dplyr::filter(lat > 50 & lat < 53.7 & lng > -7  & lng < -5.5) #gps artefact

mean_dat <- mean(data_dublin_geocoded$price)
sd_dat <- sd(data_dublin_geocoded$price)

data_dublin_geocoded_clean <- data_dublin_geocoded %>%
  dplyr::filter(price > mean_dat - sd_dat) %>%
  dplyr::filter(price < mean_dat + sd_dat)

#########################################################
#               get Dublin GPS box                      #
#########################################################
boundaries_dublin <- opq(bbox = 'Dublin, Ireland') %>%
  add_osm_feature(key = 'admin_level', value = '7') %>%
  osmdata_sf %>% unique_osmdata

bb <- st_bbox(boundaries_dublin$osm_multipolygons$geometry)
# Defining a buffer
buffer <- 0.1
p_big <- rbind(c(bb[1] - buffer, bb[2] - buffer),
               c(bb[1] - buffer, bb[4] + buffer),
               c(bb[3] + buffer, bb[4] + buffer),
               c(bb[3] + buffer, bb[2] - buffer),
               c(bb[1] - buffer, bb[2] - buffer))
# Putting the coordinates into a squared polygon object
pol_dublin <- st_polygon(list(p_big)) %>% st_geometry
# Providing the SRID (here, unprojected lon/lat)
st_crs(pol_dublin) <- 4326
#########################################################
#              get Dublin coast line                    #
#########################################################
coast_dublin <- opq(bbox = st_bbox(pol_dublin)) %>%
  add_osm_feature(key = 'natural', value = 'coastline') %>%
  osmdata_sf %>% unique_osmdata

blade_dublin <- coast_dublin$osm_lines$geometry %>% st_union %>% st_line_merge

ls <- st_linestring(blade_dublin[[1]][[1]])

multipol <- st_split(st_geometry(pol_dublin), st_geometry(ls))
#########################################################
#           define knots on Dublin ground               #
#########################################################
bound.grid.lng <- seq(from = bb[1],to = bb[3],len = 10)
bound.grid.lat<- seq(from = bb[2],to = bb[4],len = 10)

bound.grid.map <- expand.grid(lng = bound.grid.lng,lat = bound.grid.lat)

gam_boundaries <- multipol[[1]][[1]][[1]]

bound <- list(list(x = gam_boundaries[,1], y = gam_boundaries[,2], f = rep(0, nrow(gam_boundaries))))

names(bound.grid.map) <- c("x","y")

knots <- bound.grid.map[with(bound.grid.map, inSide(bound, x, y)), ]

names(knots) <- c("lng", "lat")
names(bound[[1]]) <- c("lng", "lat", "f")

knots <- knots[-38, ]

#########################################################
#                 filter disjoint gps                   #
#########################################################
dublin_frame <- multipol[[1]][[1]]

data_dublin_geocoded_gps <- data_dublin_geocoded_clean %>%
  dplyr::select(lng, lat)

data_dublin_geocoded_gps$disjoint <- apply(data_dublin_geocoded_gps, 1, function(row) {  
  as.numeric(st_disjoint(dublin_frame, st_point(c(row["lng"],row["lat"]))))
})

data_dublin_geocoded_disjoint <- data_dublin_geocoded_gps %>%
  dplyr::filter(disjoint == 1)

data_dublin_geocoded_clean <- dplyr::anti_join(data_dublin_geocoded_clean,data_dublin_geocoded_disjoint, by = c("lng", "lat"))
write_rds(data_dublin_geocoded_clean, "data_dublin_geocoded_clean.rds")