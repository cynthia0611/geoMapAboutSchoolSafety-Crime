library(ggmap)
library(ggrepel)

schoolCoor <- read.csv("Z:/Yueming/YSun/GeoCodeSchools/geoCodeSchools.csv")

# class(schoolCoor)
# head(schoolCoor,2)
# 


# ggmap(get_map("syracuse, ny"),zoom=12,
#       # source="osm",
#       maptype="roadmap")


crimePoint <- read.csv("Z:/Yueming/YSun/GeoCodeSchools/syracuseCrimeSep82015-May62016.csv")

# head(crimePoint)
# names(crimePoint)

crimeAddress <- paste(crimePoint$Address,"Syracuse","NY",sep=",")

crimePointLocation <- geocode(crimeAddress)

# head(crimePointLocation,2)

############ calculate cirme distance
# crime.count.1mile <- NULL
# 
# for( i in 1:nrow(schoolCoor) )
# {
#   lat.i <- schoolCoor$Latitude[i]
#   lon.i <- schoolCoor$Longitude[i]
#   
#   dist.c <- sqrt( (lat.i - crimePointLocation$lat)^2 + (lon.i - crimePointLocation$lon)^2 )
#   
#   crime.count.1mile[i] <- sum( dist.c < 0.0087 ) # 1 mile
#   
# }
# 
# schoolCoor <- cbind( schoolCoor, crime.count.1mile )


# head(schoolCoor)

# http://www.csgnetwork.com/gpsdistcalc.html

# crime.count.2mile <- NULL
# 
# for( i in 1:nrow(schoolCoor) )
# {
#   lat.i <- schoolCoor$Latitude[i]
#   lon.i <- schoolCoor$Longitude[i]
#   
#   dist.c <- sqrt( (lat.i - crimePointLocation$lat)^2 + (lon.i - crimePointLocation$lon)^2 )
#   
#   crime.count.2mile[i] <- sum( dist.c < 0.0174 ) # 2 miles
#   
# }
# 
# schoolCoor <- cbind( schoolCoor, crime.count.2mile )
# 
# 
# head(schoolCoor)


# crime.count.0.5mile <- NULL
# 
# for( i in 1:nrow(schoolCoor) )
# {
#   lat.i <- schoolCoor$Latitude[i]
#   lon.i <- schoolCoor$Longitude[i]
#   
#   dist.c <- sqrt( (lat.i - crimePointLocation$lat)^2 + (lon.i - crimePointLocation$lon)^2 )
#   
#   crime.count.0.5mile[i] <- sum( dist.c < 0.00435 ) #0.5 mile
#   
# }
# 
# schoolCoor <- cbind( schoolCoor, crime.count.0.5mile )

# head(schoolCoor)


crime.count.3mile <- NULL

for( i in 1:nrow(schoolCoor) )
{
  lat.i <- schoolCoor$Latitude[i]
  lon.i <- schoolCoor$Longitude[i]
  
  dist.c <- sqrt( (lat.i - crimePointLocation$lat)^2 + (lon.i - crimePointLocation$lon)^2 )
  
  crime.count.3mile[i] <- sum( dist.c < 0.0261 ) #3 miles
  
}

schoolCoor <- cbind( schoolCoor, crime.count.3mile )


head(schoolCoor)


# crime.count.2.5mile <- NULL
# 
# for( i in 1:nrow(schoolCoor) )
# {
#   lat.i <- schoolCoor$Latitude[i]
#   lon.i <- schoolCoor$Longitude[i]
#   
#   dist.c <- sqrt( (lat.i - crimePointLocation$lat)^2 + (lon.i - crimePointLocation$lon)^2 )
#   
#   crime.count.2.5mile[i] <- sum( dist.c < 0.02173 ) #2.5 miles
#   
# }
# 
# schoolCoor <- cbind( schoolCoor, crime.count.2.5mile )
# 
# 
# head(schoolCoor)


syracuse <-ggmap(get_map (location = "syracuse, ny", 
               # source="google",
               # maptype="roadmap", 
               zoom=13,
               color="bw"
               ),extent="device")


  
# map2 <- syracuse + stat_density2d(data=crimePointLocation, 
#                                   aes(x=lon,y=lat,
#                                       fill= ..level..), 
#                                  # bins=50, 
#                                  # n=50,
#                                  geom="polygon", 
#                                  # geom="tile",
#                                  alpha=0.2,
#                                  # col="orange"
#                                  n = 100 
#                                  # h = 100
#                                  # na.rm = FALSE, 
#                                  # show.legend = T
#                                  )  


  
map21 <- syracuse + stat_density2d(data = crimePointLocation, aes(x = lon, y = lat,  
                               fill = ..level.., alpha = ..level..),
                 size = 0.05, 
                 # bins = 16, 
                 geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.1, 0.5), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        text = element_text(size = 12))

# https://sarahleejane.github.io/learning/r/2014/09/21/plotting-data-points-on-maps-with-r.html
  
map3 <- map21 +  geom_point(data=schoolCoor,
                           aes(x=Longitude, y=Latitude,size=crime.count.3mile),
                           # size=3, 
                           col="steel blue",alpha=0.8)


# 
# names(schoolCoor)

# https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html

map4 <- map3+geom_text_repel(data=schoolCoor,aes(x=Longitude, y=Latitude, 
                                                 label = schools)
                             # ,label.size = .5
                             ) +
  theme_classic(base_size = 10)

map4


