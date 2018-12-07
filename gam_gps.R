options(scipen=999)

data_gps_sample <- read_rds("data/data_gps_sample.rds")

data_gps_sample_dublin <- data_gps_sample %>%
  dplyr::filter(County == "Dublin") %>%
  dplyr::filter(lat > 50) %>%
  dplyr::filter(lng > -7)

library(mgcv)

test <- mgcv::gam(price ~ s(lat,lng), data = data_gps_sample_dublin, method = "REML")

gratia::draw(test)
vis.gam(test)
vis.gam(test,type="response", plot.type="contour") 
vis.gam(test, view = c("lat","lng"), plot.type = "persp", se = 2)
vis.gam(test, view = c("lat","lng"), 
        plot.type = "contour", too.far = 0.05)

library(gratia)

gratia::appraise(test, type = "response")


grid.x <- seq(from = min(data_gps_sample_dublin$lat), to = max(data_gps_sample_dublin$lat), by = 0.005)
grid.y <- seq(from = min(data_gps_sample_dublin$lng),to = max(data_gps_sample_dublin$lng), by = 0.005)

pdata <- expand.grid(lat = grid.x, lng = grid.y)

##predictions
pdata <- transform(pdata, price = predict(test, pdata, type = "response"))

ggplot() +
  geom_raster(data = pdata, aes(x = lng, y = lat, fill = price))+
  geom_point(data = data_gps_sample_dublin, aes(x = lng, y = lat))+
  scale_fill_gradientn(colours = gradian_col)
