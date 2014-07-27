setwd('/Users/Runze/Github/artsy')
library(rjson)
library(plyr)
library(ggplot2)
library(scales)
library(maps)
library(mapdata)
library(RDSTK)
library(httr)
library(rgdal)
library(maptools)
gpclibPermit()

load(file = 'data/followers.RData')

#get country names from location
location = followers$location[followers$location != '']

#map to standardized names through data sciences toolkit
geo_map = function(addr){
  url = 'http://www.datasciencetoolkit.org/maps/api/geocode/json'
  addr = gsub('[[:punct:]]', ' ', addr)
  response = GET(url, query = list(sensor = 'FALSE', address = addr))
  response = gsub('<|!', '', response)
  json = fromJSON(paste(response, collapse = ''), unexpected.escape = 'keep')
  if (length(json$results) > 0) {
    loc_name = json$results[[1]]$address_components
    country_name = ifelse(length(loc_name) > 0, as.character(loc_name[[length(loc_name)]]), '')
  }
  else {
    country_name = ''
  }
  country_name
}

#note it takes a long time to retrieve all the standardized country names
country_name = unlist(lapply(location, geo_map))
save(country_name, file = 'data/country_name.RData')

#remove blank country names (i.e., those that were not found)
country_name = country_name[country_name != '']

#standardize names to be consistent with the map data used below
country_name = gsub('USA', 'United States', country_name)
country_name = gsub('Iran', 'Iran (Islamic Republic of)', country_name)
country_name = gsub('Libya', 'Libyan Arab Jamahiriya', country_name)
country_name = gsub('Moldova', 'Republic of Moldova', country_name)
country_name = gsub('Northern Ireland', 'Ireland', country_name)
country_name = gsub('South Korea', 'Korea, Republic of', country_name)
country_name = gsub('Syria', 'Syrian Arab Republic', country_name)
country_name = gsub('Tanzania', 'United Republic of Tanzania', country_name)

#collapse to count the frequencies
country_count = data.frame(table(country_name))
names(country_count) = c('id', 'count')
country_count = country_count[order(country_count$count, decreasing = T), ]

#subset to only those countries included in the world map
#first, parse world map data
gpclibPermit()
world_map = readOGR(dsn = 'map_data', layer = 'TM_WORLD_BORDERS_SIMPL-0.3')
world_map = fortify(world_map, region = 'NAME')

#subset
country_count_map = subset(country_count, id %in% world_map$id)

#calculate quantiles (in 10% increment)
q = quantile(country_count_map$count, seq(.1, 1, .1), na.rm = T)
labels = vector()
for (i in 1:length(q)-1) {
  labels[i] = paste(names(q)[i], names(q)[i+1], sep = '-')
}

country_count_map$quantile = cut(country_count_map$count, breaks = q, labels = labels, include.lowest = T)

#plot geographic heatmap
pal = colorRampPalette(c('#F7FBFF', '#08306B'))(length(q)-1)

heatmap = 
  ggplot(country_count_map, aes(map_id = id)) +
  geom_map(aes(fill = quantile), map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_manual(values = pal) +
  labs(fill = 'Quantiles') + 
  xlab('Longitude') + ylab('Lattitude') + 
  ggtitle('Geographic Distribution of Followers\n') + 
  theme(plot.title = element_text(size = rel(1.5), face = 'bold'), legend.position = 'bottom')

ggsave(filename = 'graph/heatmap.jpeg', plot = heatmap, width = 40, height = 40, unit = 'cm')