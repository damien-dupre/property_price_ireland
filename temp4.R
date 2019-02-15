test <- data_dublin_geocoded_clean %>%
  dplyr::filter(year == 2011 | year == 2012) %>%
  dplyr::mutate(year = as.factor(as.character(year)))


mean_dat <- mean(test$price)
sd_dat <- sd(test$price)

test <- test %>%
  dplyr::filter(price > mean_dat - sd_dat) %>%
  dplyr::filter(price < mean_dat + sd_dat)



gam_dublin_gps <- mgcv::gam(
  price ~ s(lng, lat, bs = "so", xt = list(bnd = bound), by = year) + year, 
  data = test, 
  method = "REML", 
  knots = knots,
  control=list(trace = TRUE))
?mgcv::gam
summary(gam_dublin_gps)

grid.lat <- seq(from = bb[2],to = bb[4],by = 0.005)
grid.lng <- seq(from = bb[1],to = bb[3],by = 0.005)

grid.year <- c(2011, 2012)

grid.map <- expand.grid(lat = grid.lat, lng = grid.lng, year = grid.year) %>%
  dplyr::mutate(year = as.factor(as.character(year)))

pdata <- transform(grid.map, price = predict(gam_dublin_gps, grid.map, type = "response"))

pdata %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(m_year = mean(price, na.rm = TRUE))

gam.check(gam_dublin_gps)

plot(gam_dublin_gps, pages = 1)

summary(gam_dublin_gps)
vis.gam(gam_dublin_gps, plot.type = "contour",too.far = 0.5, pages = 1)

ggplot() +
  geom_raster(data = pdata, aes(x = lng, y = lat, fill = price)) +
  #geom_point(data = test, aes(x = lng, y = lat), size = 0.1) +
  geom_sf(data=multipol[[1]][[2]], fill = "blue") +
  scale_fill_gradientn("Predicted Price", colours = gradian_col) +
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") +
  facet_wrap(~year)+
  theme_minimal() +
  theme(text = element_text(family = "serif", size=8))
?soap
