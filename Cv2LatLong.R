#
##			a conversion from WGS84 to GPS
#

library(proj4)
GetLatLong	<- function(df)
  {
  stg <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

  pj 	  <- project(df, stg, inverse = TRUE)
  latlong <- data.frame(lat = pj$y, lon = pj$x)
  return(latlong)
  }

Est	<- 1577339
Nth	<- 5182974
xy 	<- data.frame(Est, Nth)
LatLong <- GetLatLong (xy)
unname (LatLong)
##
