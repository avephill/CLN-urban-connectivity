

#  ---------------------------------------
# Species occurrence data ---------------------------------------
#  ---------------------------------------


library(sf)
library(tidyverse)
# library(rgbif)
# library(terra)
# library(stars)
library(RPostgreSQL)
# library(starsExtra)

# sf_use_s2(T)
counties.sf <- st_read("~/Data/Boundaries/Political/CA_Counties/CA_Counties_TIGER2016.shp") %>% 
  filter(NAME %in% c("Solano", "Contra Costa", "Alameda", "Santa Clara", "San Mateo",
                     "San Francisco", "Marin", "Sonoma", "Napa", "Santa Cruz")) %>% 
  st_transform(crs = 4326)
target_counties.sf <- 
  counties.sf %>% 
  st_union()  # Convert to WGS 84

# # Simplify input polygons and add some buffer zone to it
target_simple.sf <- 
  target_counties.sf %>% 
  st_union(is_coverage = T) %>% 
  st_buffer(4000) %>% # 1km buffer
  st_simplify(dTolerance = 4000) %>% # reduce 
  st_cast("POLYGON")

# write_sf(target_simple.sf[1], "../data/study_area.gpkg") # just write mainland


# Convert to a well known text format of the polygon and check validity
target.ewkt <- 
  target_simple.sf %>% 
  st_union() %>% 
  st_as_text(EWKT = T)

# Now pull from local GBIF database 
con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 host="flor", 
                 port=5432, 
                 user="postgres", 
                 password="postgres"
)

species_names <- c("Taricha torosa")

bigquery <- paste0("
SELECT * FROM ca_core 
INNER JOIN ca_species ON ca_species.taxonKey=ca_core.taxonKey 
INNER JOIN ca_extra ON ca_extra.gbifid=ca_core.gbifid 
WHERE ST_Intersects(ca_core.geom,'",target.ewkt,"') 
AND ca_species.taxonrank='SPECIES' 
AND ca_species.species IN ('",paste(species_names, collapse="','"),"')")

system.time({
  target_spec.sf <- st_read(con, query = bigquery)
})

write_sf(target_spec.sf, 
         paste0("data/occurrence/",
                format(Sys.time(), "%Y-%m-%d"),
                "_target_spec.gpkg"),
         layer = "spec_occ")
