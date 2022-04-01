#' conn 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom terra rast

brick_cog_url <- "/vsicurl/https://testpaccmstorage.blob.core.windows.net/test-container/pm25_brick_cog.tif"

rt_bcog <- terra::rast(brick_cog_url)

names(rt_bcog) <- seq(as.Date("2018/01/01"), by = "month", length.out = 3)

sel_dates <- as.character(seq(as.Date("2018/01/01"), by = "month", length.out = 2))

terra::subset(rt_bcog, sel_dates)

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



# sqlite ------------------------------------------------------------------

# throws build error, but maybe preferred with a fix?
# db_file = system.file("extdata", "adi-db-pa.sqlite", package = "vizpm25")

db_path <- "inst/extdata/adi-db-pa.sqlite"

con <- pool::dbPool(
  drv = RSQLite::SQLite(),
  dbname = db_path,
  host = NULL,
  username = NULL,
  password = NULL
)

shiny::onStop(function() {
  pool::poolClose(con)
  print("pool closing!")
})

# con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
# 
# shiny::onStop(function() {
#   DBI::dbDisconnect(con)
#   print("db disconnecting!")
# })

# load(system.file("data", package = "zipcodeR", "zip_code_db.rda"))

# print(class(iris))

# zip_code_db <- zipcodeR::zip_code_db

