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
colnames(data_dublin_2018_osm_features_aero)
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
"railway_tram"                                
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

"service_crossover"                          
[143] "service_siding"                              "service_spur"                               
[145] "service_yard"                                "tunnel_yes"                                 
[147] "usage_main"                                  "railway_platform"                           
[149] "route_bicycle"                               "route_bus"                                  
[151] "route_ferry"                                 "route_hiking"                               
[153] "route_power"                                 "route_road"                                 
[155] "route_train"                                 "route_tram"                                 
[157] "shop_paint"                                  "shop_kitchen"                               
[159] "sport_badminton"                             "sport_equestrian"                           
[161] "sport_gaelic_games"                          "sport_rugby_union"                          
[163] "sport_running"                               "tourism_artwork"                            
[165] "tourism_zoo"                                 "waterway_river"                             
[167] "waterway_riverbank"                          "waterway_stream"                            
[169] "waterway_canal"                              "waterway_drain"                             
[171] "waterway_ditch"                              "waterway_weir"                              
[173] "waterway_lock_gate"                          "source_survey"                              
[175] "area_yes"                                    "covered_yes"                                
[177] "disused_yes"                                 "tidal_yes"                                  
[179] "GEOGID"                                      "GEOG_ID"                                    
[181] "AIRO_AI_ID.x"                                "ED_Ward.x"                                  
[183] "ED_WARD_ID.x"                                "County_UD.x"                                
[185] "Country.x"                                   "Provides No Care"                           
[187] "1-19 hours unpaid PW"                        "20-49 hours unpaid PW"                      
[189] "50+ hours unpaid PW"                         "Total Care Providers"                       
[191] "% Provides No Care"                          "% 1-19 hours unpaid PW"                     
[193] "% 20-49 hours unpaid PW"                     "% 50+ hours unpaid PW"                      
[195] "% Total Care Providers"                      "AIRO_AI_ID.y"                               
[197] "ED_Ward.y"                                   "ED_WARD_ID.y"                               
[199] "County_UD.y"                                 "Country.y"                                  
[201] "Persons with a disability aged 0-14"         "Persons with a disability aged 15 - 44"     
[203] "Persons with a disability aged 25 - 44"      "Persons with a disability aged 45 - 64"     
[205] "Persons with a disability aged 65 Plus"      "Total persons with a disability"            
[207] "Total Pop"                                   "% Persons with a disability aged 0-14"      
[209] "% Persons with a disability aged 15 - 44"    "% Persons with a disability aged 25 - 44"   
[211] "% Persons with a disability aged 45 - 64"    "% Persons with a disability aged 65 Plus"   
[213] "% Total persons with a disability"           "AIRO_AI_ID.x.x"                             
[215] "ED_Ward.x.x"                                 "ED_WARD_ID.x.x"                             
[217] "County_UD.x.x"                               "Country.x.x"                                
[219] "Very Good"                                   "Good"                                       
[221] "Fair"                                        "Bad"                                        
[223] "Very Bad"                                    "% Very Good"                                
[225] "% Good"                                      "% Fair"                                     
[227] "% Bad"                                       "% Very Bad"                                 
[229] "AIRO_AI_ID.y.y"                              "ED_Ward.y.y"                                
[231] "ED_WARD_ID.y.y"                              "County_UD.y.y"                              
[233] "Country.y.y"                                 "Occupied/HS With ususal Residents"          
[235] "Unoccupied/HS Without Ususal Residents"      "Total_Stock"                                
[237] "% Occupied/HS With ususal Residents"         "%  Unoccupied/HS Without Ususal Residents"  
[239] "AIRO_AI_ID.x.x.x"                            "ED_Ward.x.x.x"                              
[241] "ED_WARD_ID.x.x.x"                            "County_UD.x.x.x"                            
[243] "Country.x.x.x"                               "Average"                                    
[245] "1 Room (Households)"                         "2 Rooms (Households)"                       
[247] "3 Rooms (Households)"                        "4 Rooms (Households)"                       
[249] "5 Rooms (Households)"                        "6 Rooms (Households)"                       
[251] "7 Rooms (Households)"                        "8 or more Rooms (Households)"               
[253] "Total (Households).x"                        "% 1 Room (Households)"                      
[255] "% 2 Rooms (Households)"                      "% 3 Rooms (Households)"                     
[257] "% 4 Rooms (Households)"                      "% 5 Rooms (Households)"                     
[259] "% 6 Rooms (Households)"                      "% 7 Rooms (Households)"                     
[261] "% 8 or more Rooms (Households)"              "% Total (Households).x"                     
[263] "AIRO_AI_ID.y.y.y"                            "ED_Ward.y.y.y"                              
[265] "ED_WARD_ID.y.y.y"                            "County_UD.y.y.y"                            
[267] "Country.y.y.y"                               "Owner Occupier with Mortgage (Households)"  
[269] "Owner Occupier No Mortgage (Households)"     "Private Rented"                             
[271] "Social Rented"                               "Rented Free of Rent (Households)"           
[273] "Total (Households).y"                        "% Owner Occupier with Mortgage (Households)"
[275] "% Owner Occupier No Mortgage (Households)"   "% Private Rented"                           
[277] "% Social Rented"                             "% Rented Free of Rent (Households)"         
[279] "% Total (Households).y"                      "AIRO_AI_ID.x.x.x.x"                         
[281] "ED_Ward.x.x.x.x"                             "ED_WARD_ID.x.x.x.x"                         
[283] "County_UD.x.x.x.x"                           "Country.x.x.x.x"                            
[285] "House/Bungalow"                              "Flat/Apartment/BedSit"                      
[287] "Caravan/Mobile home/Temperory"               "% House/Bungalow"                           
[289] "% Flat/Apartment/BedSit"                     "% Caravan/Mobile home/Temperory"            
[291] "AIRO_AI_ID.y.y.y.y"                          "ED_Ward.y.y.y.y"                            
[293] "ED_WARD_ID.y.y.y.y"                          "County_UD.y.y.y.y"                          
[295] "Country.y.y.y.y"                             "TOTPOP"                                     
[297] "MALE"                                        "FEMALE"                                     
[299] "POPDENKM"                                    "AGE014T"                                    
[301] "AGE1524T"                                    "AGE2544T"                                   
[303] "AGE4564"                                     "AEG65P"                                     
[305] "AGE1564"                                     "AGE80P"                                     
[307] "DEPR"                                        "YDEPR"                                      
[309] "ODEPR"                                       "SEX_R"                                      
[311] "PC_MALE"                                     "PC_FEMALE"                                  
[313] "PCAGE014T"                                   "PCAGE1524T"                                 
[315] "PCAGE2544T"                                  "PCAGE4564"                                  
[317] "PCAEG65P"                                    "PCAGE1564"                                  
[319] "PCAGE80P"                                    "FERT_R"                                     
[321] "AIRO_AI_ID"                                  "ED_Ward"                                    
[323] "ED_WARD_ID"                                  "County_UD"                                  
[325] "Country"                                     "TOT"                                        
[327] "RO_CATH"                                     "OTH_C"                                      
[329] "NC_OTH"                                      "NS"                                         
[331] "NO_REL"                                      "PC_RO_CATH"                                 
[333] "PC_OTH_C"                                    "PC_NC_OTH"                                  
[335] "PC_NS"                                       "PC_NO_REL"





















