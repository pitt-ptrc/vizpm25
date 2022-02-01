# raster data prep

library(terra)
library(tidyverse)

library(raster)

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
class(my_rast)

my_rast %>% plot()

my_rast

raster::raster()

my_rast %>% extend(c(1, 2))

pm_rast1 <- rast("data-raw/GWRwSPEC_PM25_NA_200001_200001-RH35.nc")
pm_rast2 <- rast("data-raw/GWRwSPEC_PM25_NA_200002_200002-RH35.nc")
pm_rast3 <- rast("data-raw/GWRwSPEC_PM25_NA_200003_200003-RH35.nc")

pm_list <- list(pm_rast1, pm_rast2, pm_rast3)

map(pm_list, ncell)

s <- rast(system.file("ex/logo.tif", package="terra"))   
pm_rast1
# Date"
d <- as.Date("2001-05-04") + 0:2
time(s) <- d
time(s)

# POSIX (time stored as seconds)
time(s) <- as.POSIXlt(d)
time(s)

# "raw" time
time(s) <- as.numeric(d)
time(s)

cpm_rast <- c(pm_rast1, pm_rast2, pm_rast3)

cpm_rast


test_rest_geo



data("zion_points", package = "spDataLarge")
zion_points %>% head()
zion_points %>% class()
elevation = terra::extract(srtm, vect(zion_points))
zion_points = cbind(zion_points, elevation)

st_geometry_type(zion_points)

test_rest_coor <- test_rest_geo %>% select(latitude, longitude)

test_rest_coor
st_point(test_rest_coor)

library(sf)
sf::st_as_sf(test_rest_coor, co)

test_rest_sf <-
  sf::st_as_sf(test_rest_geo,
               coords = c("longitude", "latitude"),
               crs = st_crs(4326))


test_rest_sf

test_poll <- terra::extract(pm_rast1, vect(zion_points), layer = 1)

test_poll

srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
zion = read_sf(system.file("vector/zion.gpkg", package = "spDataLarge"))
zion = st_transform(zion, crs(srtm))

data("zion_points", package = "spDataLarge")
elevation = terra::extract(srtm, vect(zion_points))
zion_points = cbind(zion_points, elevation)

srtm
pm_rast1
pm_rast1a <- rast('NETCDF:"data-raw/GWRwSPEC_PM25_NA_200001_200001-RH35.nc"')
