#' conn 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom terra rast
#' @import AzureStor
#' @import dplyr
#' @importFrom tidyr separate
#' @import stringr

gdal_virt_fs_head <- "/vsicurl"
blob_name <- "https://testpaccmstorage.blob.core.windows.net"
cont_name <- "test-container"
access_key <- Sys.getenv("ACCESS_KEY")

bl_endp_key <- AzureStor::storage_endpoint(blob_name, key = access_key)
cont <- AzureStor::storage_container(bl_endp_key, cont_name)

assn_dates_names <- function(spatrast){
  names(spatrast) <- 
    seq.Date(as.Date("2000-01-01"), as.Date("2017-12-01"), by = "month")
  
  spatrast
}

mat_rast_list_old <- cont %>% 
  AzureStor::list_storage_files() %>% 
  filter(stringr::str_detect(name, "cog_")) %>% 
  filter(!stringr::str_detect(name, "PM25|SO4|SOIL")) %>% 
  tidyr::separate(name, into = c("cog", "material", "portion"), remove = FALSE, extra = "drop") %>% 
  mutate(vsi_url = file.path(gdal_virt_fs_head, blob_name, cont_name, name)) %>% 
  tibble() %>% 
  split(.$material) %>% 
  map(., ~ pull(.x, vsi_url) %>% 
        terra::rast() %>% 
        c() %>% 
        assn_dates_names())

mat_rast_list_new <- cont %>% 
  AzureStor::list_storage_files() %>% 
  filter(stringr::str_detect(name, "cog_")) %>% 
  filter(stringr::str_detect(name, "PM25|SO4|SOIL")) %>% 
  tidyr::separate(name, into = c("cog", "material", "year"), remove = FALSE, extra = "drop") %>% 
  mutate(vsi_url = file.path(gdal_virt_fs_head, blob_name, cont_name, name)) %>% 
  tibble() %>% 
  split(.$material) %>% 
  map(., ~ pull(.x, vsi_url) %>% 
        terra::rast() %>% 
        c()
        # assn_dates_names()
      )

mat_rast_list <- c(mat_rast_list_old, mat_rast_list_new)

# brick_cog_url <- "/vsicurl/https://testpaccmstorage.blob.core.windows.net/test-container/pm25_brick_cog.tif"
# 
# rt_bcog <- terra::rast(brick_cog_url)

# names(rt_bcog) <- seq(as.Date("2018/01/01"), by = "month", length.out = 3)

# sel_dates <- as.character(seq(as.Date("2018/01/01"), by = "month", length.out = 2))
# 
# terra::subset(rt_bcog, sel_dates)

# data("test_restaurant")
# 
# test_rest_address <- 
#   read_csv("data-raw/test_rest_address.csv")
# 
# test_rest_address
# 
# test_restaurant

# 
# rt_bcog[["2018-01-01"]]
# 
# rt_bcog["^2018-([0]|[0-2])-01"]


# arrow -----------------------------------------------------------------

#' Connection to Arrow Disk Dataset
#'
#' @param parquet_path 
#'
#' @return arrow connection
#' @importFrom dplyr rename
#' @importFrom arrow open_dataset
#' @importFrom magrittr %>%
arrow_con <- function(parquet_path) {
  arrow::open_dataset(
    sources = parquet_path, 
    format = "parquet" 
  )
}

adis <- arrow_con(parquet_path = "inst/extdata/pa_adi2.parquet")
