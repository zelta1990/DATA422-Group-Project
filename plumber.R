#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#



#install.packages("tidyverse")
#install.packages("skimr")
#install.packages("readr")
#install.packages("visdat")
#install.packages("plumber")
#install.packages("gapminder")
#install.packages("proj4")
library(proj4)
library(skimr)
library(readr)
library(visdat)
library(tidyverse)
# Crash data API
library(gapminder)
library(plumber)



crash_df <-read_csv("https://raw.githubusercontent.com/zelta1990/crash_data_nz/main/Crash_data_chch.csv")
crash_df <- crash_df %>% filter(crashYear > 2011)  #only return records after 2012


#    Title: NZTM-WGS84(NZGD2000) converter
#    Author: mkennedy
#    Date: Feb 16, 2012 at 2:32
#    Availability: https://gis.stackexchange.com/questions/20389/converting-nzmg-or-nztm-to-latitude-longitude-for-use-with-r-map-library/20401#20401

#install.packages("proj4")
library(proj4)
get_lat_lon <- function(x,y)
{
  proj4string <- "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  #proj4string <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"
  # Source data
  xy <- data.frame(x, y)
  
  # Transformed data
  pj <- project(xy, proj4string, inverse=TRUE)
  latlon <- data.frame(lat=pj$y, lon=pj$x)
  return(latlon)
}


#Calculate the count of accidents at one location for each year
count_df <- crash_df %>% 
  filter(crashSeverity == "Serious Crash" | crashSeverity == "Fatal Crash" ) %>%
  count(crashLocation1, crashLocation2, crashYear,sort=TRUE) 
#Sort each location by accident count for each year
all_year = list()
for(i in unique(crash_df$crashYear))
{
  year_df <- count_df %>% 
    filter(crashYear == i ) %>% 
    arrange(desc(n)) 
  all_year[[i]] <- year_df
}
all_year_df <- bind_rows(all_year)


#Calculate the accumulative count of serious or fatal accidents at one location over ten years and return locations with count > 1
count_all_year_df <- crash_df %>% 
  filter(crashSeverity == "Serious Crash" | crashSeverity == "Fatal Crash" ) %>%
  count(crashLocation1, crashLocation2,sort=TRUE) 
count_all_year_df <- count_all_year_df %>%
  filter(n > 1) #only return locations with accident count > 1


#Join count_all_year_df with original crash_df to return details for locations that had repeated accidents 
target_df <- inner_join(crash_df, count_all_year_df %>% 
                          select(-3), by = c('crashLocation1', 'crashLocation2')) %>%
  filter(crashSeverity == "Serious Crash" | crashSeverity == "Fatal Crash" ) %>% 
  mutate(lat = get_lat_lon(X,Y)$lat, lon=get_lat_lon(X,Y)$lon) 

accident_detail_df <- target_df%>%
  select(c(X,Y,lat,lon, OBJECTID, areaUnitID,crashFinancialYear,crashDirectionDescription,crashLocation1,crashLocation2,crashRoadSideRoad,crashSeverity,
            crashYear,debris,directionRoleDescription,ditch,fatalCount,fence,houseOrBuilding,intersection,meshblockId,minorInjuryCount,
            objectThrownOrDropped,otherObject,otherVehicleType,overBank,parkedVehicle,pedestrian,phoneBoxEtc,postOrPole,region,roadCharacter,
            schoolBus,seriousInjuryCount,suv,taxi,tlaId,tlaName,unknownVehicleType))



#Calculate frequency for factor levels in columns. The result was used as a reference to determine which columns should be used for layers in the map
col_freq_df <- target_df %>% select(-c(X,Y,OBJECTID, areaUnitID,crashFinancialYear,crashDirectionDescription,crashLocation1,crashLocation2,crashRoadSideRoad,crashSeverity,
                                       crashYear,debris,directionRoleDescription,ditch,fatalCount,fence,houseOrBuilding,intersection,meshblockId,minorInjuryCount,
                                       objectThrownOrDropped,otherObject,otherVehicleType,overBank,parkedVehicle,pedestrian,phoneBoxEtc,postOrPole,region,roadCharacter,
                                       schoolBus,seriousInjuryCount,suv,taxi,tlaId,tlaName,unknownVehicleType,lat,lon))
all_columns = list()
for(i in 1:ncol(col_freq_df))
{
  df <- col_freq_df[i]
  new_df <- df %>%
    group_by(df[1]) %>%
    tally() %>%
    mutate(percent=round(100*n/sum(n))) %>%
    arrange(desc(percent)) 
  colnames(new_df)[1] <- "value"  
  colnames(new_df)[2] <- "count"         
  new_df <- cbind(name = colnames(col_freq_df[i]), new_df)
  new_df$value <- as.character(new_df$value)
  all_columns[[i]] <- new_df
}
all_col_freq_df <- bind_rows(all_columns)

#Crash data used for the map
d <- as.data.frame(target_df %>%         
                     select(lat,lon,crashLocation1, crashLocation2,crashYear,trafficControl,light,advisorySpeed,bicycle,cliffBank,flatHill,guardRail,motorcycle,NumberOfLanes,
                            roadLane,speedLimit,streetLight,trafficIsland,weatherA,weatherB)) 
d[-c(1,2,5)] <- sapply(d[-c(1,2,5)],as.character) #convert all layer columns to char type 
d <- replace(d, is.na(d), "Unknown") 




#* @apiTitle Crash data Christchurch API
#* @apiDescription An API for Christchurch crash data analysis

#* Return accident count for locations ordered by count for selected year(s).
#* Use "all" for parameter to return results between 2012 and 2022
#* @param year The year used to filter result.
#* @get /accident-count-by-year
function(year) {
 
  if(year == "all"){
    all_year_df
  }else
  {
    all_year_df %>% filter(crashYear == year) 
  }
}


#* Return accumulative accident count for locations between 2012 and 2022
#* @get /accident-count
function() {
  count_all_year_df
}

#* Return details for accidents at locations where more than one accident happened between 2012 and 2022
#* @get /accident-details
function() {
  accident_detail_df
}

#* Return descriptions for locations where more than one accident happened between 2012 and 2022
#* @get /location-details
function() {
  d
}





