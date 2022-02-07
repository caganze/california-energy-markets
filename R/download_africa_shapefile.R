# This script is borrowed from StackExchange. The goal is to download and save an "Africa" shape-file 
# I then save this shapefile as an image and use it as a background for my project. 
library(gapminder)
library(sf)
library(ggplot2)

# Comment this section out. We only needed to run it once to extract the Africa Shapefile
# From the World GIS dataset. 

# shp <- readOGR(dsn="Global_Shape/GADM/gadm36.shp", layer="gadm36")
# 
# countryweb <-  
#   "https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv"
# 
# country.csv <- read.csv(countryweb)
# names(country.csv)[5] <- "GID_0"
# africaCountries <- subset(country.csv, Continent_Code=="AF")
# africa_shp <- subset(shp, GID_0 %in% africaCountries$GID_0 )

## Store the Africa shapefile so that you don't have to import the whole world next time:
# writeOGR(africa_shp,
#          ".",
#          "Processed/LNSHUTI.github.io_africa-rgdal", 
#          driver="ESRI Shapefile")

# Add helper function borrowed from John Graves' health-care-markets repo
# This function remove unnecessary axes from shape maps

remove_all_axes <- ggplot2::theme(
  axis.text = ggplot2::element_blank(),
  axis.line = ggplot2::element_blank(),
  axis.ticks = ggplot2::element_blank(),
  panel.border = ggplot2::element_blank(),
  panel.grid = ggplot2::element_blank(),
  axis.title = ggplot2::element_blank(),
  rect = element_blank(), 
  plot.background = element_blank()
)

# Load shape files
africa_shape <-
  read_sf("LNSHUTI.github.io/Processed/LNSHUTI.github.io_africa-rgdal.shp") %>%
  dplyr::select(NAME_0, geometry) %>% 
  unique() 
  
simple_africa_shape <-
  africa_shape %>% 
  ggplot() + 
  geom_sf() + 
  remove_all_axes + 
  coord_sf(datum=NA) 

ggsave(simple_africa_shape,
       filename = file.path(here::here(), "LNSHUTI.github.io/plots/simple_africa_shape_union.png")
       )

# The plot above does not dissolve internal boundaries. Combine boundaries and save new plot
dissolved_africa_shape <- 
  st_union(africa_shape$geometry) %>% 
  ggplot() +
  geom_sf() + 
  remove_all_axes + 
  coord_sf(datum=NA) 

ggsave(dissolved_africa_shape,
       filename = file.path(here::here(), "LNSHUTI.github.io/plots/dissolved_africa_shape.png")
)


#' **References:** 
#  1. Question asked by Thomas and answered by Matthew Yap. 
#     https://gis.stackexchange.com/questions/248143/one-shapefile-for-african-continent-containing-counties-regions
#  2. Visualizing Geographic Connections. https://www.data-to-viz.com/story/MapConnection.html
#  3. https://geocompr.github.io/geocompkg/articles/solutions08.html
