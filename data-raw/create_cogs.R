# raster data prep

# https://frodriguezsanchez.net/post/accessing-data-from-large-online-rasters-with-cloud-optimized-geotiff-gdal-and-terra-r-package/
# gdal_translate eg_tidync/Data/pm25_brick.tif eg_tidync/Data/pm25_brick_cog.tif -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=DEFLATE


library(terra)
library(magrittr)
# library(tidyverse)
library(purrr)

locs <- tibble::tribble(
  ~X,          ~Y,
  -100.44228,    37.52046,
  -100.71178,    39.91526,
  -101.899544,   38.780769,
  -101.71137,    39.55035,
  -95.97031163, 39.16120974
)



list.files("data-raw/BC", full.names = TRUE)

filelist_temp <- list.files("data-raw/BC", full.names = TRUE)

filelist_temp2 <- list.files("data-raw/BC")

filelist_temp2

filelist_temp[1:5]


tibble(filelist_temp) %>% 
  separate(filelist_temp, into = c("spec", "type", "na", "from", "to"), sep = "\\_|\\.")

library(ncdf4)
nc <- nc_open("https://dapds00.nci.org.au/thredds/dodsC/uc0/Test_pixel_count.nc")

az_nc <- nc_open("https://testpaccmstorage.blob.core.windows.net/test-public-container/BC/GWRwSPEC_BC_NA_200103_200103.nc")

nc

temp_rasters <- rast(filelist_temp[1:2])

tibble(temp_rasters)

seq(1, by = 12, length.out = 16)
seq(12, by = 12, length.out = 16)

temp_rasters %>% writeRaster(filename = "data-raw/bc_test.tif", overwrite = TRUE)

filelist_temp

make_cog <- function(src_dir, dst_dir) {
  
  # req(src_dir)
  # req(dst_dir)
  
  src_paths <- list.files(src_dir, full.names = TRUE)
  src_files <- list.files(src_dir, full.names = FALSE)
  dst_paths <- file.path(dst_dir, src_files)
  
  print(dst_paths) 
  
  convert <- function(src_path, dst_path) {
    
    
    
    gdalUtilities::gdal_translate(
      src_dataset = src_path,
      dst_dataset = dst_path,
      co = matrix(
        c("TILED=YES",
          "COPY_SRC_OVERVIEWS=YES",
          "COMPRESS=DEFLATE"),
        ncol = 1
      )
    )
  }
  
  map2(src_paths, dst_paths, ~ convert(.x, .y))
}



make_cog(src_dir = "data-raw/BC2/", dst_dir = "BC_cog")

gdalUtilities::gdal_translate(src_dataset = "data-raw/BC2/GWRwSPEC_BC_NA_200001_200001.nc", 
                              dst_dataset = "data-raw/BC2/bc_200001_test.tif",
                              co = matrix(c("TILED=YES", 
                                            "COPY_SRC_OVERVIEWS=YES", 
                                            "COMPRESS=DEFLATE"), 
                                          ncol = 1))



bc <- rast("data-raw/bc_test.tif")

bc_test <- rast("data-raw/BC2/bc_200001_test.tif")

bc_test %>% extract(locs)

bc_test %>% plot()

rt <- terra::rast("data-raw/BC/GWRwSPEC_BC_NA_200001_200001.nc")


rt %>% extract(locs)

rt2 <- terra::rast("data-raw/BC2/out.tif")

rt2 %>% terra::extract(locs)

bctest2 <- terra::rast("data-raw/bc_test.tif")

bctest2 %>% extract(locs)

temp_rasters %>% terra::extract(locs)

rnc1 <- terra::rast("data-raw/BC/GWRwSPEC_BC_NA_200001_200001.nc")
rnc2 <- terra::rast("data-raw/BC/GWRwSPEC_BC_NA_200002_200002.nc")

rnc <- c(rnc1, rnc2)

rnc1 %>% terra::extract(locs)

temp_rasters

r <- rast(nrow=18, ncol=36)
m <- matrix(1:ncell(r), nrow=18)
values(r) <- as.vector(t(m))
rx <- flip(r, direction="h")

values(r) <- as.vector(m)
ry <- flip(r, direction="v")

v <- rev(r)

ry <- flip(rnc1, direction="v", filename = )

flip(rnc1, direction="vertical", filename="rnc1v.nc")

r <- terra::rast("GWRwSPEC_PM25_NA_200001_200001-RH35.nc")
rr <- terra::flip(r, "vertical")

sessionInfo()

install.packages("raster")
