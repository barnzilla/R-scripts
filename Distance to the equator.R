# Install geosphere package
install.packages("geosphere")

# Load geosphere package
library("geosphere")

# Express latitude/longitude in decimal notation 
## Latitude of 30°13’45’’N
latitude <- 30 + (13/60) + (45/3600)

## Longitude of 45 °37’83’’E
longitude <- 45 + (37/60) + (83/3600)

# Calculate shortest distance between 2 points on an ellipsoid
equator <- c(longitude, 0)
point2 <- c(longitude, latitude)
round(distGeo(equator, point2) / 1000, 0)