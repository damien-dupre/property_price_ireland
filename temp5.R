test %>% 
  ggplot(aes(x = price, y = year)) + 
  ggridges::geom_density_ridges_gradient(aes(fill = ..density..), size = 0.1)+
  scale_x_continuous("Price")+
  scale_y_discrete("")+
  theme_minimal() +
  theme(text = element_text(family = "serif", size=8))+ 
  scale_fill_gradientn(name = "Density",colours = colorRampPalette(c("white", blues9))(256))


gam_dublin_gps <- mgcv::gam(
  price ~ s(lng, lat, bs = "so", xt = list(bnd = bound), by = year) + year, 
  data = test, 
  method = "REML", 
  knots = knots,
  control=list(trace = TRUE),
  family=gaussian(link = "identity"))

gam.check(gam_dublin_gps)

gam_dublin_gps <- mgcv::gam(
  price ~ s(lng, lat,bs = "so", xt = list(bnd = bound), by = year) + year, 
  data = test, 
  method = "REML", 
  knots = knots,
  control=list(trace = TRUE),
  family=gaussian(link = "identity"))

