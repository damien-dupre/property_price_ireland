library(dplyr)
library(leaflet)

ireland_smallarea <- rgdal::readOGR("C:/Users/dupred/Desktop/Small_Areas__Generalised_20m__OSi_National_Boundaries.geojson")

leaflet::leaflet(ireland_smallarea) %>% 
  addTiles() %>% 
  addPolygons()
??leaflet
################################################################################
poly_1 <- ireland_smallarea@polygons[[1]]

leaflet::leaflet(poly_1) %>% 
  addTiles() %>% 
  addPolygons() 
################################################################################
poly_3 <- ireland_smallarea@polygons[[3]]

leaflet::leaflet(poly_3) %>% 
  addTiles() %>% 
  addPolygons()
################################################################################
test <- ireland_smallarea@data
