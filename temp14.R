carers <- read_csv(here::here("data/aero_data/carers.csv"))

disabilty_age_group <- read_csv(here::here("data/aero_data/disabilty_age_group.csv"))

general_health <- read_csv(here::here("data/aero_data/general_health.csv"))

housing_occupancy <- read_csv(here::here("data/aero_data/housing_occupancy.csv"))

housing_rooms <- read_csv(here::here("data/aero_data/housing_rooms.csv"))

housing_tenure <- read_csv(here::here("data/aero_data/housing_tenure.csv"))

housing_type <- read_csv(here::here("data/aero_data/housing_type.csv"))

population <- read_csv(here::here("data/aero_data/population.csv"))

religion <- read_csv(here::here("data/aero_data/religion.csv"))

census_data <- carers %>% 
  dplyr::left_join(disabilty_age_group) %>% 
  dplyr::left_join(general_health) %>% 
  dplyr::left_join(housing_occupancy) %>% 
  dplyr::left_join(housing_rooms) %>% 
  dplyr::left_join(housing_tenure) %>% 
  dplyr::left_join(housing_type) %>% 
  dplyr::left_join(population) %>% 
  dplyr::left_join(religion)

write.csv(census_data,"data/census_data.csv",row.names = FALSE)
