#
##	A script to wrangle the Crash Data
#
#	the 	2nd line keeps only serious and fatal accidents
#		3rd removes "odd" locations 
#		others convert to std suffixes (there is a mixture in the orig. file)
#		The NoSH will remove state highway stuff (if necessary)

library(tidyverse)
library(stringr)
library(proj4)

GetLatLong	<- function(df)		# conversion from WGS84 to GPS
  {
  stg <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"

  pj 	  <- project(df, stg, inverse = TRUE)
  latlong <- data.frame(lat = pj$y, lng = pj$x)
  return(latlong)
  }

DF2012		<- DF2012[grepl("Serious|Fatal", DF2012$crashSeverity), ] 
DF2012		<- DF2012[!grepl("01S|073|074|075|076", DF2012$crashLocation1), ] 

DF2012$crashLocation1	<- str_replace(DF2012$crashLocation1, "AVENUE",  "Ave")
DF2012$crashLocation1	<- str_replace(DF2012$crashLocation1, "DRIVE",   "Drv")
DF2012$crashLocation1	<- str_replace(DF2012$crashLocation1, "ROAD",    "Rd")
DF2012$crashLocation1	<- str_replace(DF2012$crashLocation1, "STREET",  "ST")
DF2012$crashLocation1	<- str_replace(DF2012$crashLocation1, "TERRACE", "Tce")

#NoSHdf		<- DF2012[!grepl("SH ", DF2012$crashLocation1),] 
#NoSHdf		<- NoSHdf[!grepl("SH ", NoSHdf$crashLocation2),]	# df now SH free

##	Determine the location (street) freq. There are two optons: create a table or keep as a df
##	But the table approach doesn't work as intended.

stDF	<- count(data, crashLocation1, sort = TRUE)	 	# preserving as df
print (stDF)

# dfWG	  	<- data.frame(dfCrash$X, dfCrash$Y)		# undertake the coord convsn
# LatLong	<- GetLatLong (dfWG)
# dfCrash$Lat	<- round(LatLong$lat, 5)			# 5 dec places
# dfCrash$Long	<- round(LatLong$lng, 5)

# write.csv(DF2012, '/tmp/OutPut.csv', row.names = FALSE)	# for testing

#	N.B.	This WORKS
# vec	<- c(1, 3, 5, 7, 9, 2, 4, 6, 8, 11)
# df	<- as_data_frame(table(vec))		# convert a table to a df
#		FAILS
# locs	<- table(data$crashLocation1)		# converting 'locs' to an array or df isn't possible
##
