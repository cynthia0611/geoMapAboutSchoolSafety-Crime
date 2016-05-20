
# COUNTIES IN THE SYRACUSE MSA
# 
# Syracuse, NY (Metropolitan Statistical Area) (45060)
# 
# Madison, NY [36053]
# Onondaga, NY [36067]
# Oswego, NY [36075]

library( maptools )

setwd("Z:/Yueming/YSun/GeoCodeSchools/tl_2015_36_tract")

ny <- readShapePoly(
  fn="tl_2015_36_tract",
  proj4string=CRS("+proj=longlat +datum=WGS84")
)

# head(syrSchoolDistrict)
# 
# names(syrSchoolDistrict)

# plot( ny, border="gray" )


these <- c("053", "067", "075" ) 
# Madison, NY [36053]
# Onondaga, NY [36067]
# Oswego, NY [36075]

syr <- ny[ ny$COUNTYFP %in% these, ]

shp.id <- as.character(syr$GEOID)

# load census data for poverty topic
census.dat <- read.csv( "Z:/Yueming/YSun/GeoCodeSchools/aff_download ny/ACS_14_5YR_S1701_with_ann.csv", 
                        colClasses="character" )
census.dat <- census.dat[ -1 , ] # get rid of annotations
geo.id <- census.dat$GEO.id2

poverty.count <- as.numeric( census.dat$HC02_EST_VC01 )


# match and reorder the poverty data to the shapefile order
order <- match( shp.id, geo.id )
poverty.count <- poverty.count[ order ]

pov.dots <- dotsInPolys( syr, poverty.count/10, f="random" )

pov.dots <- as.data.frame( pov.dots )


# get school data in
schoolCoor <- read.csv("Z:/Yueming/YSun/GeoCodeSchools/geoCodeSchools.csv")


library(ggmap)
syrbasicmap <- get_map("syracuse,ny", zoom = 13, maptype="roadmap", color="bw" )
syrmap <- ggmap(syrbasicmap, extent="device")


h2 <- syrmap + geom_point(data=pov.dots, aes(x=x,y=y), size = 0.5, alpha=0.5, col="goldenrod" )


h3 <- h2 + stat_density2d(data=pov.dots,aes(x=x,y=y, fill = ..level..,alpha = ..level..),
                               # bins=500, 
                              geom="polygon", 
                              size=.05
                              # alpha=0.01 
                              )+
  scale_fill_gradient(low = "steel blue", high = "red") +
  scale_alpha(range = c(0.1, 0.5), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        text = element_text(size = 12))


h4 <- h3 +  geom_point(data=schoolCoor,
                             aes(x=Longitude, y=Latitude,
                                 size=schoolCoor$referralYTDMay19Wk33),
                       # size=schoolCoor$attRateYTDMay19Wk33), # not much difference with % number
                       # size=schoolCoor$chronicAbsenteeismYTDMay19Wk33),
                       
                       # bin=100
                             # size=3, 
                             col="steelblue",alpha=0.8)+ scale_size_area(max_size =9)

# h4

# http://colorbrewer2.org/

h5 <- h4 + geom_text_repel(data=schoolCoor,
                              aes(x=Longitude, y=Latitude, 
                                  label = schools),size=4.3
                           ,color="#252525",fontface="bold"
                          # segment.size = 0.9
                              # ,label.size = .5
) + theme_classic(base_size = 10)  
  
# geom_text(data=schoolCoor,aes(label=schools),size=3)

h5

# head(schoolCoor$referralYTDMay19Wk33)





