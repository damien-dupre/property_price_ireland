data_dublin_2018_osm_features <- read_rds(here::here("data/data_dublin_2018_osm_features.rds")) %>% 
  dplyr::select_if(~sum(!is.na(.)) > 0) %>% 
  dplyr::mutate(GEOG_ID = stringr::str_remove(GEOGID,"[A]"))

list_geoid <- unique(data_dublin_2018_osm_features$GEOG_ID)
################################################################################
carers <- read_csv(here::here("data/aero_data/carers.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

disabilty_age_group <- read_csv(here::here("data/aero_data/disabilty_age_group.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

general_health <- read_csv(here::here("data/aero_data/general_health.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

housing_occupancy <- read_csv(here::here("data/aero_data/housing_occupancy.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

housing_rooms <- read_csv(here::here("data/aero_data/housing_rooms.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

housing_tenure <- read_csv(here::here("data/aero_data/housing_tenure.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

housing_type <- read_csv(here::here("data/aero_data/housing_type.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

population <- read_csv(here::here("data/aero_data/population.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)

religion <- read_csv(here::here("data/aero_data/religion.csv")) %>% 
  dplyr::filter(GEOG_ID %in% list_geoid)
################################################################################
data_dublin_2018_osm_features_aero <- data_dublin_2018_osm_features %>% 
  dplyr::left_join(carers, by = "GEOG_ID") %>% 
  dplyr::left_join(disabilty_age_group, by = "GEOG_ID") %>% 
  dplyr::left_join(general_health, by = "GEOG_ID") %>% 
  dplyr::left_join(housing_occupancy, by = "GEOG_ID") %>% 
  dplyr::left_join(housing_rooms, by = "GEOG_ID") %>% 
  dplyr::left_join(housing_tenure, by = "GEOG_ID") %>% 
  dplyr::left_join(housing_type, by = "GEOG_ID") %>% 
  dplyr::left_join(population, by = "GEOG_ID") %>% 
  dplyr::left_join(religion, by = "GEOG_ID")
################################################################################
amenity <- c(
  "amenity_bar",
  "amenity_college",                             
  "amenity_school",
  "amenity_university",                          
  "amenity_bicycle_parking",
  "amenity_fuel",                               
  "amenity_parking",
  "amenity_community_centre",                    
  "amenity_bench",
  "amenity_embassy",                            
  "amenity_police",
  "amenity_prison",                             
  "amenity_recycling"
)
barrier <- c(
  "barrier_city_wall",                           
  "barrier_ditch",                              
  "barrier_fence",                               
  "barrier_guard_rail",                         
  "barrier_hedge",                              
  "barrier_kerb",                               
  "barrier_retaining_wall",                      
  "barrier_wall",                               
  "barrier_block",                               
  "barrier_bollard",                            
  "barrier_chain",                               
  "barrier_full-height_turnstile",              
  "barrier_gate",                                
  "barrier_jersey_barrier",                     
  "barrier_yes"
)
boundary <- c(
  "boundary_administrative",                    
  "boundary_historic",                           
  "boundary_political",                         
  "boundary_postal_code",                        
  "boundary_protected_area"
)
building <- c(
  "building_apartments",                         
  "building_house",                             
  "building_residential",                        
  "building_commercial",                        
  "building_industrial",                         
  "building_retail",                           
  "building_hospital",                           
  "building_university",                        
  "building_yes"
)
highway <- c(
  "highway_motorway",                           
  "highway_trunk",                              
  "highway_secondary",                          
  "highway_tertiary",                            
  "highway_unclassified",                       
  "highway_residential",                         
  "highway_service",                            
  "highway_motorway_link",                       
  "highway_trunk_link",                         
  "highway_secondary_link",                      
  "highway_tertiary_link",                      
  "highway_pedestrian",                          
  "highway_track",                              
  "highway_road",                                
  "highway_footway",                            
  "highway_steps",                               
  "highway_path",                               
  "highway_cycleway"
)
cycleway <- c(
  "cycleway_lane",                              
  "cycleway_opposite",                          
  "cycleway_opposite_lane",                     
  "cycleway_track",                              
  "cycleway_share_busway",                      
  "cycleway_shared_lane"
)
busway <- c(
  "busway_lane"
)
highway <- c(
  "highway_proposed",                            
  "highway_construction" 
)
junction <- c(
  "junction_roundabout"
)
historic <- c(
  "historic_yes"
)
landuse <- c(
  "landuse_commercial",                          
  "landuse_construction",                       
  "landuse_industrial" ,                         
  "landuse_residential",                        
  "landuse_retail",                              
  "landuse_farmland",                           
  "landuse_grass",                               
  "landuse_military",                           
  "landuse_railway",                             
  "landuse_recreation_ground",                  
  "landuse_religious"
)
leisure <- c(
  "leisure_nature_reserve",                     
  "leisure_park",                                
  "leisure_slipway",                            
  "leisure_sports_centre",                      
  "leisure_stadium",                            
  "leisure_track"
)
man_made <- c(
  "man_made_breakwater",                        
  "man_made_crane",                              
  "man_made_embankment",                        
  "man_made_groyne",                            
  "man_made_pier",                              
  "man_made_pipeline"
)
natural <- c(
  "natural_wood",                               
  "natural_tree_row",                            
  "natural_scrub",                             
  "natural_grassland",                           
  "natural_water",                              
  "natural_beach",                               
  "natural_coastline",                          
  "natural_ridge",                               
  "natural_cliff"
)

place <- c(
  "place_district",                              
  "place_county",                               
  "place_city",                                  
  "place_suburb",                               
  "place_island",                                
  "place_locality"
)
power <- c(
  "power_cable",
  "power_line",
  "power_minor_line",                           
  "power_portal" 
)
line <- c(
  "line_busbar"
)

public_transport <- c(
  "public_transport_platform",                 
  "public_transport_stop_area"                  
)
railway <- c(
  "railway_abandoned",                          
  "railway_disused",                             
  "railway_rail",                               
  "railway_tram",
  "railway_platform"
)
bridge <- c(
  "bridge_yes"
)

cutting <- c(
  "cutting_yes"                                 
)
electrified_contact <- c(
  "electrified_contact_line"                   
)
embankment <- c(
  "embankment_yes"                              
)
service <- c(
  "service_crossover",                          
  "service_siding",                              
  "service_spur",
  "service_yard"                                
)

tunnel <- c(
  "tunnel_yes"
)
usage <- c(
  "usage_main"                                  
)

route <- c(                       
  "route_bicycle",                               
  "route_bus",                                  
  "route_ferry",                                 
  "route_hiking",                               
  "route_power",                                 
  "route_road",                                 
  "route_train",                                 
  "route_tram"
)                                 
shop <- c(  
  "shop_paint",                                  
  "shop_kitchen"                              
)
sport <- c(  
"sport_badminton",                             
"sport_equestrian",                           
"sport_gaelic_games",                          
"sport_rugby_union",                          
"sport_running"
)
tourism <- c(
  "tourism_artwork",                            
  "tourism_zoo"                                 
)

waterway <- c(
  "waterway_river",                             
  "waterway_riverbank",                          
  "waterway_stream",                            
  "waterway_canal",                              
  "waterway_drain",                            
  "waterway_ditch",                              
  "waterway_weir",                              
  "waterway_lock_gate"
)

source <- c(
  "source_survey"                              
)
area <- c(
  "area_yes"
)
covered <- c(
  "covered_yes" 
)
disused <- c(
  "disused_yes"
)
tidal <- c(
  "tidal_yes" 
)
carers <- c(              
  "% Provides No Care",                          
  "% 1-19 hours unpaid PW",                     
  "% 20-49 hours unpaid PW",                     
  "% 50+ hours unpaid PW",                      
  "% Total Care Providers"                      
)
disabilty_age_group <- c(
  "% Persons with a disability aged 0-14",      
  "% Persons with a disability aged 15 - 44",    
  "% Persons with a disability aged 25 - 44",   
  "% Persons with a disability aged 45 - 64",    
  "% Persons with a disability aged 65 Plus",   
  "% Total persons with a disability" 
)

general_health <- c(
  "% Very Good",                                
  "% Good",                                     
  "% Fair",                                     
  "% Bad",                                       
  "% Very Bad"
)

housing_occupancy <- c(
  "% Occupied/HS With ususal Residents",         
  "%  Unoccupied/HS Without Ususal Residents" 
)
housing_rooms <- c(
  "% 1 Room (Households)",                      
  "% 2 Rooms (Households)",                      
  "% 3 Rooms (Households)",                     
  "% 4 Rooms (Households)",                      
  "% 5 Rooms (Households)",                     
  "% 6 Rooms (Households)",                      
  "% 7 Rooms (Households)",                     
  "% 8 or more Rooms (Households)",              
  "% Total (Households).x"
)
housing_tenure <- c(
  "% Owner Occupier with Mortgage (Households)",
  "% Owner Occupier No Mortgage (Households)",   
  "% Private Rented",                           
  "% Social Rented",                             
  "% Rented Free of Rent (Households)",        
  "% Total (Households).y"  
)                               
housing_type <- c(
  "% House/Bungalow",                           
  "% Flat/Apartment/BedSit",                     
  "% Caravan/Mobile home/Temperory" 
)               
population <- c(
  "PC_MALE",                                     
  "PC_FEMALE",                                  
  "PCAGE014T",                                   
  "PCAGE1524T",                                 
  "PCAGE2544T",                                  
  "PCAGE4564",                                  
  "PCAEG65P",                                    
  "PCAGE1564",                                  
  "PCAGE80P"
)

religion <- c(
  "PC_RO_CATH",                                 
  "PC_OTH_C",                                   
  "PC_NC_OTH",                                  
  "PC_NS",                                       
  "PC_NO_REL"
)
################################################################################
tbl_y <- data_dublin_2018_osm_features_aero %>%
  dplyr::select(price) %>% 
  as.matrix()
tbl_x <- data_dublin_2018_osm_features_aero %>%
  dplyr::select(lng,
                lat,
                amenity,
                barrier,
                boundary,
                building,
                highway,
                cycleway,
                busway,
                highway,
                junction,
                historic,
                landuse,
                leisure,
                man_made,
                natural,
                place,
                power,
                line,
                public_transport,
                railway,
                bridge,
                cutting,
                electrified_contact,
                embankment,
                service,
                tunnel,
                usage,
                route,                       
                shop,  
                tourism,
                waterway,
                source,
                area,
                covered,
                disused,
                tidal,
                carers,              
                disabilty_age_group,
                general_health,
                housing_occupancy,
                housing_rooms,
                housing_tenure,
                housing_type,
                population,
                religion) %>% 
  as.matrix()
#
xgmodel <- xgboost::xgboost(
  data = tbl_x,
  label = tbl_y,
  objective = "reg:linear",
  eval_metric = "rmse",
  #booster = "gblinear",
  verbose = 1,
  nround = 200
)
pred <- predict(xgmodel, tbl_x)

result <- data.frame(pred = pred, raw = tbl_y)

lm_result <- lm(pred ~ price,result)

summary(lm_result)

importance_matrix <- xgb.importance(model = xgmodel)












