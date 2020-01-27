library(sf)
library(foreign)
library(here)
library(tidyverse)
library(lwgeom)
# library(stringi)
library(tmaptools)
library(showtext)

# info about helsinki -----------------------------------------------------
# 
# raut_geocode <- tmaptools::geocode_OSM('Rautatientori, Helsinki')
# 
# save(raut_geocode, file = here::here('data', 'cooked', '01_raut_geocode.RData'))

load(here::here('data', 'cooked', '01_raut_geocode.RData'))

city <- 'helsinki'
lat <- raut_geocode$coords['y']
long <- raut_geocode$coords['x']
rad <- 12000
crs <- 102013

allroads <- sf::read_sf(here::here('data', 'raw', 'osm'), 'gis_osm_roads_free_1')

## subsetting roads in a circle

pt <- data.frame(lat = lat, long = long) %>% 
  st_as_sf(coords = c('long', 'lat'), 
           crs = 4326) %>% 
  st_transform(crs)

circle <- st_buffer(pt, dist = rad) %>% 
  st_transform(st_crs(allroads))

allroads <- sf::st_intersection(circle, allroads)



# reading in water --------------------------------------------------------

# water <- sf::read_sf(here::here('data', 'raw', 'osm'), 'gis_osm_water_a_free_1') %>% 
#   sf::st_intersection(circle, water)
#   
# unique(water$fclass)
# 
# feat <- sf::read_sf(here::here('data', 'raw', 'osm'), 'gis_osm_pois_a_free_1') %>%
#   sf::st_intersection(circle, .)
# 
# unique(feat$fclass)
# 
# counting suffix occurences ----------------------------------------------

allroads %>%
  select(name) %>%
  filter(!is.na(name),
         !str_detect(.$name, 'tie'),
         !str_detect(.$name, 'katu'),
         !str_detect(.$name, 'kuja'),
         !str_detect(.$name, 'polku'),
         !str_detect(.$name, 'väylä'),
         !str_detect(.$name, 'silta'),
         !str_detect(.$name, 'ranta'),
         !str_detect(.$name, 'kallio'),
         !str_detect(.$name, 'kaari')) %>% nrow()

suffixes <- c('tie')

count_suffix <- function(pattern) {
  sum(str_detect(allroads$name, pattern), na.rm = T)
}


tribble(~suffix, ~count,
       'katu', count_suffix('katu'),
       'polku', count_suffix('polku'),
       'tie', count_suffix('tie'),
       'kuja', count_suffix('kuja'),
       'reitti', count_suffix('reitti'),
       'porras', count_suffix('porras'),
       'piha', count_suffix('piha'),
       'väylä', count_suffix('väylä'),
       'silta', count_suffix('silta'),
       'ranta', count_suffix('ranta'),
       'kaari', count_suffix('kaari')) %>%
  arrange(desc(count))

count_suffix('tie')
# 
# motorway <- filter(allroads, fclass %in% c('motorway', 'motorway_link'))
# 
# motorway %>%
#   select(name) %>%
#   filter(!is.na(name)) %>%
#   glimpse()
# 
# 
# 
# trunk <- filter(allroads, fclass %in% c('trunk', 'trunk_link'))
# 
# trunk %>%
#   select(name) %>%
#   filter(!is.na(name)) %>%
#   glimpse()


# ordinals <- filter(allroads, fclass %in% c('primary', 'secondary', 'tertiary', 'primary_link', 'secondary_link', 'tertiary_link')) %>%
#   st_intersection(circle, .)
#
# footway <- filter(allroads, fclass %in% c('footway')) %>%
#   st_intersection(circle, .)
#
# residential <- filter(allroads, fclass == 'residential') %>%
#   st_intersection(circle, .)
#
# living_steet <- filter(allroads, fclass == 'living_street') %>%
#   st_intersection(circle, .)
#
# cycleways <- filter(allroads, fclass == 'cycleway') %>%
#   st_intersection(circle, .)

# allroads %>%
#   filter(fclass %in% c('primary', 'secondary', 'tertiary', 'motorway', 'trunk', 'footway', 'residential', 'living_street'),
#          !is.na(name)) %>%
#   select(name) %>%
#   View()


## removing unnamed footpaths

# allroads_m <- allroads %>%
#   filter(!(fclass == "footway" & is.na(name)),
#          fclass %in% c("motorway", "secondary", "tertiary", "primary", "service", 'residential', 'path', 'footway'))

# allroads_e <- allroads[!(allroads$fclass  == "footway" & is.na(allroads$name)),]


# plot --------------------------------------------------------------------

# c(trunk,
#   tertiary,
#   residential,
#   secondary,
#   primary,
# )

# unique(allroads$fclass)
# 
# allroads_plot <- allroads %>% 
#   filter(!is.na(name)) %>% 
#   dplyr::mutate(type = case_when(
#     str_detect(name, 'tie|Tie') ~ 'tie',
#     str_detect(name, 'katu') ~ 'katu',
#     str_detect(name, 'kuja|Kuja') ~ 'kuja',
#     str_detect(name, 'polku|Polku') ~ 'polku',
#     str_detect(fclass, 'motorway|trunk|motorway_link|trunk_link'),
#     TRUE ~ 'other'
#   ))
  # filter(fclass %in% c('service'))

allroads_plot <- allroads %>% 
  filter(!is.na(name)) %>% 
          filter(fclass %in% c('cicleway')) %>%
  dplyr::mutate(type = case_when(
    str_detect(name, 'tie|Tie') ~ 'tie',
    str_detect(name, 'katu') ~ 'katu',
    str_detect(name, 'kuja|Kuja') ~ 'kuja',
    str_detect(name, 'polku|Polku') ~ 'polku',
    str_detect(fclass, 'motorway|trunk|motorway_link|trunk_link') ~ 'moottoritie',
    TRUE ~ 'other'
  )) %>% 
  mutate(type = factor(type, levels = c(
    'moottoritie',
    'katu',
    'kuja',
    'polku',
    'tie',
    'other'
  )))



# colors_1 <- c(
#   '#fb4851',
#   '#fff275',
#   '#ff8c42',
#   '#4c6663',
#   '#8ab0ab',
#   'grey80'
# )


blankbg <-
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank()
  )

# map <-
  ggplot() + 
  blankbg + 
  theme(panel.grid.major = element_line(colour = "transparent")) + 
  # geom_sf(data = water, fill = 'blue') +
  geom_sf(data = allroads_plot, size = .3, aes(color = type),
          show.legend = T) +
  scale_color_manual(values = colors)
  
  # geom_sf(data = cycleways, size = .4, color = 'red') +
  # # geom_sf(data=otherroads, size = .8) +
  # geom_sf(data=motorway, size =.8, color = "#ff9223") +
  # geom_sf(data = trunk, size = .6, color = "steelblue") +
  # geom_sf(data = ordinals, size = .4, color = 'green') +
  # geom_sf(data = residential, size = .2, color = 'black')


# ggsave(here::here("products", "helsinki_streets_names.pdf"), 
#        width = 12, height = 11.63, device = cairo_pdf, plot = map)



# dark mode ---------------------------------------------------------------

# theme_set(theme_void)
# theme_update(
#   panel.background = element_rect(fill = "#2a3d45 ", 
#                                   color = "#2a3d45 "),
#   plot.background = element_rect(fill = "#2a3d45 ", 
#                                  color = "grey60",
#                                  size = 7))  


# facets ------------------------------------------------------------------

unique(allroads$fclass)
  
allroads_ind <- allroads %>% 
    filter(!is.na(name),
           fclass == unique(.$fclass)[c(8)])

allroads_facets <- allroads %>% 
  filter(!is.na(name))

facets <- ggplot() +
  theme_void() +
  geom_sf(data = allroads_facets, size =.2) +
  facet_wrap(~fclass)

ggsave(here::here("products", "facets.pdf"),
       width = 12, height = 11.63, device = cairo_pdf, plot = facets)




# trying colors 2 ---------------------------------------------------------

colors_2 <- c(
  giants_orange = '#f26419',
  amaranth_red = '#d62828',
  green_blue = '#2176ae',
  tuquoise = "#55dde0",
  saffron = '#f6ae2d',
  iluminating_emerald = '#32936f'
)

# '#f26419', #giants_orange
# '#f6ae2d', #saffron 
# '#812A94' #purle


colors_2 <- c(
  '#d62828', #amaranth_red
  '#2176ae', #green_blue
  '#32936f', #iluminating_emerald
  "#55dde0", #tuquoise
  '#ffbf00', #fluorescent orange,
  'grey70'
)

allroads_plot <- allroads %>% 
  filter(!is.na(name)) %>%
  # filter(fclass %in% c('cicleway')) %>%
  dplyr::mutate(type = case_when(
    str_detect(name, 'tie|Tie') ~ 'tie',
    str_detect(name, 'katu') ~ 'katu',
    str_detect(name, 'kuja|Kuja') ~ 'kuja',
    str_detect(name, 'polku|Polku') ~ 'polku',
    str_detect(fclass, 'motorway|trunk|motorway_link|trunk_link') ~ 'moottoritie',
    # str_detect(name, 'väylä') ~ 'moottoritie',
    TRUE ~ 'other'
  )) %>% 
  mutate(type = factor(type, levels = c(
    'moottoritie',
    'katu',
    'kuja',
    'polku',
    'tie',
    'other'
  )))


ggplot() + 
  blankbg + 
  theme(panel.grid.major = element_line(colour = "transparent")) + 
  # geom_sf(data = water, fill = 'blue') +
  geom_sf(data = allroads_plot, size = .4, aes(color = type),
          show.legend = 'point') +
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5, nrow = 1,
                              label.position = "right",
                              override.aes = list(size = 7.5))) +
  scale_color_manual(values = colors_2) +
  theme(legend.position = "bottom") +
  labs(title = 'Helsinki streets') 


# ggsave(here::here('products', 'streets_v1.pdf'), plot = last_plot(),
#        scale = 1, width = 24, height = 36, units = "in",#,
#        dpi = 500)

ggsave(here::here('products', 'streets_v2.pdf'), 
       width = 12, height = 11.63, device = cairo_pdf)

allroads %>% 
  filter(fclass %in% unique(allroads$fclass)[c(1, 13, 14, 19)]) %>% 
  select(name) %>% 
  View()

unique(allroads$fclass)
