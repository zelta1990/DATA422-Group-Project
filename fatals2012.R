#
##	A script to wrangle the Crash Data
#
#	the 	2nd line keeps only serious and fatal accidents
#		3rd removes "odd" locations 
#		others convert to std suffixes (there is a mixture in the orig. file)
#		The NoSH will remove state highway stuff (if necessary)

# dfWG	  	<- data.frame(dfCrash$X, dfCrash$Y)		# undertake the coord convsn
# LatLong	<- GetLatLong (dfWG)
# dfCrash$Lat	<- round(LatLong$lat, 5)			# 5 dec places
# dfCrash$Long	<- round(LatLong$lng, 5)

##	I think I need a pointer or two here. The output of the summary does not equal
##	the row dimension of df stDF. Apparently the group_by() is not grouping by
##	all locations passed to it; only a few and only at the upper end of the alphabet
#
#	The solution is probably something simple but not obvious.

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

Nme		<- "/tmp/Test_"
Yrs		<- c("2012", "2016", "2021")
years		<- c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")
DF2012		<- read.csv("../data/CrashData2012.csv") 
DF2012		<- DF2012[, c(1, 2, 13, 16, 18, 30, 52, 71)] 		# keep thise cols for now

DF2012		<- DF2012[grepl("Serious|Fatal", DF2012$crashSeverity), ] 
DF2012		<- DF2012[!grepl("01S|073|074|075|076", DF2012$crashLocation1), ] 

DF2012$crashLocation1	<- str_replace(DF2012$crashLocation1, "AVENUE",  "Ave")
DF2012$crashLocation1	<- str_replace(DF2012$crashLocation1, "DRIVE",   "Drv")
DF2012$crashLocation1	<- str_replace(DF2012$crashLocation1, "ROAD",    "Rd")
DF2012$crashLocation1	<- str_replace(DF2012$crashLocation1, "STREET",  "ST")
DF2012$crashLocation1	<- str_replace(DF2012$crashLocation1, "TERRACE", "Tce")

for (y in Yrs)
  {
  CrashYr		<- DF2012[DF2012$crashYear %in% y, ]
  stDF			<- count(CrashYr, crashLocation1, sort = TRUE)	 # preserving as df
  Result		<- group_by(stDF, crashLocation1) %>%
  			   summarise (total = sum(n))
  QtyCrashes		<- nrow(stDF)					# to be collected in an array

  print(QtyCrashes)
  print(dim(CrashYr))
  print(dim(stDF))
  print (y)								# print the year
  print (Result)
#  stop()
  }

##
#  Fname		<- paste0(Nme, y, ".csv")
#  write.csv(CrashYr,  Fname, row.names = FALSE)			# for testing

# print (dim(Crash2012))
#write.csv(Crash2012, '../data/CrashData2012-39Col.csv', row.names = FALSE)
# print (dim(Crash2012))
# write.csv(DF2012, '/tmp/OutPut.csv', row.names = FALSE)	# for testing
# df[is.element(df$state, c("stg1", "stg2")), ]
#NoSHdf		<- DF2012[!grepl("SH ", DF2012$crashLocation1),] 
#NoSHdf		<- NoSHdf[!grepl("SH ", NoSHdf$crashLocation2),]	# df now SH free
##
