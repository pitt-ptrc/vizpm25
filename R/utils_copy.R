#' copy 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @importFrom tibble tibble




copy <- list(
  "upload_intro" = "Your file must have a column header that includes the column names 'address' and 'id'. No name or address should appear more than once. If 'lat' and 'lng' are found in your file, we will use that to fetch data.",
  "test_slot" = "this is a test slot.",
  "table_io" = tibble(
    `Data to fetch` = c("PM2.5", "ADI", "Both"),
    `Description` = c("Particulate Matter 2.5", "Area Deprivation Index", ""),
    `Required Columns` = c("'address' OR 'lat' and 'long'", "'address'", "'address' AND/OR 'lat' and 'long'")
  )
)

