# installing and loading packages -----------------------------------------

# code from Shane's reply to this stack overflow answer:
# https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

packages <- c("sf", "foreign", "here", "tidyverse", "lwgeom", "stringi", "tmaptools")

new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)


library(sf)
library(foreign)
library(here)
library(tidyverse)
library(lwgeom)
library(stringi)
library(tmaptools)

# info about helsinki -----------------------------------------------------

raut_geocode <- tmaptools::geocode_OSM('Rautatientori, Helsinki')

city <- "helsinki"
lat <- raut_geocode$coords['y']
long <- raut_geocode$coords['x']
rad <- 20000
crs <- 102013


# setting up road types to be used and colors THIS WAS COPIED FROM --------

plottypes <-  c('Road', 'Street', 'Avenue', 'Lane', 'Close', 'Way', 'Place', 'Embankment')
plotcolors <-  c('Road' = '#59c8e5', 'Street' = '#fed032', 'Avenue' ='#4cb580', 'Lane' = '#fe4d64', 'Close' = '#0a7abf',
                 'Way' = '#2e968c', 'Place' = '#fe9ea5', 'Embankment' = '#fe9ea5', 'Motorway' = "#ff9223", 'Other' = '#cccccc')


# plotting ----------------------------------------------------------------

filename <- "gis_osm_roads_free_1"
allroads <- read_sf(here::here('data'), filename)

## subsetting roads in a circle

pt <- data.frame(lat = lat, long = long) %>% 
  st_as_sf(coords = c('long', 'lat'), 
            crs = 4326) %>% 
  st_transform(crs)

circle <- st_buffer(pt, dist = rad) %>% 
  st_transform(st_crs(allroads))

allroads <- st_intersection(circle, allroads)

## removing unnamed footpaths

allroads_m <- allroads %>%
  filter(!(fclass == "footway" & is.na(name)),
         fclass %in% c("motorway", "secondary", "tertiary", "primary", "service", 'residential', 'path', 'footway'))

allroads_e <- allroads[!(allroads$fclass  == "footway" & is.na(allroads$name)),]


## add in length

allroads <- allroads %>% 
  mutate(len = st_length(allroads))


# determining path/road suffixes ------------------------------------------

head(allroads)

allroads %>% 
  filter(!str_detect(name, "polku"),
         !str_detect(name, "tie"),
         !str_detect(name, "kuja"),
         !str_detect(name, "reitti"),
         !str_detect(name, "porras"),
         !str_detect(name, "piha"),
         !str_detect(name, "katu"),
         !str_detect(name, "vägen"),
         !str_detect(name, "kenttä"),
         !str_detect(name, "vieri"),
         !str_detect(name, "stigen"),
         !str_detect(name, "raitti"),
         !str_detect(name, "kaari"),
         !str_detect(name, "rata"),
         !str_detect(name, "syrjä"),
         !str_detect(name, "ranta"),
         !str_detect(name, "rinne"),
         !str_detect(name, "silta"),
         !str_detect(name, "niemi"),
         !str_detect(name, "käytävä"),
         !str_detect(name, "tunneli"),
         !str_detect(name, "promenadi"),
         !str_detect(name, "taso"),
         !str_detect(name, "aukio"),
         !str_detect(name, "mäki"),
         !str_detect(name, "lenkki"),
         !str_detect(name, "tori"),
         !str_detect(name, "puisto"),
         !str_detect(name, "laituri"))

# allroads$TYPE[allroads$fclass == 'motorway' & !(allroads$TYPE %in% plottypes)] <- "Motorway"
# 
# #put other roads into their own dataframe
# allroads$TYPE[!(allroads$TYPE %in% plottypes) & allroads$TYPE != 'Motorway'] <- "Other"
# otherroads <- allroads[(allroads$TYPE  == "Other"),]
# allroads <- allroads[(allroads$TYPE  != "Other"),]

#plot it
blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())

ggplot() + blankbg + theme(panel.grid.major = element_line(colour = "transparent")) + 
  # geom_sf(data=otherroads, size = .8) + 
  geom_sf(data=allroads_m, size =1) 
