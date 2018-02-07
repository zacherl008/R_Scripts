##Testing data visualization using five R packages: Leaflet, Dygraphs, networkD3, DataTables, and threejs.

install.packages("magrittr")
library(magrittr)

##Leaflet
install.packages("leaflet")
library(leaflet)
##Creates a leaflet object named map
map <- leaflet()
##Adds map tiles
map <- addTiles(map)
##Adds map markers at specific locations
map <- addMarkers(map, lat = 42.128, lng = -80.087,popup = "The location of Gannon University")
##Print map
map

##Dygraphs
install.packages("dygraphs")
library(dygraphs)
dygraph(AirPassengers, main = "Monthly Airline Passenger Numbers 1949-1960") %>% 
  dyAxis("x", label = "Year", pixelsPerLabel = 40) %>% 
  dyAxis("y", label = "# of Passengers") %>%
  dySeries("V1", label = "Passengers", color = "red") %>% 
  dyOptions(drawPoints = TRUE, includeZero = TRUE)

##networkD3
install.packages("networkD3")
library(networkD3)
##Using built in R dataset USArrests
National <- hclust(dist(USArrests), "ave")
diagonalNetwork(as.radialNetwork(National), nodeColour = "#393", 
                height = 700, width = 800, linkColour = "2B5DE5")

##DataTables
install.packages("DT")
library(DT)
##Using built in R dataset Orange
datatable(Orange, rownames = FALSE, options = list(pageLength = 4))

##rthreejs
install.packages("threejs")
library(threejs)
##Using built in threejs dataset LeMis
graphjs(LeMis, vertex.size = .5, vertex.shape = "sphere", bg = "black")

