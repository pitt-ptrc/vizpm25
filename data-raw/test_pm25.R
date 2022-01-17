## code to prepare `test_pm25` dataset goes here
test_pm25 <- read.csv("data-raw/dataPM25.csv")

test_pm25 <- test_pm25[, c(
  "ReportingYear",
  "StationLocalId",
  "SamplingPoint_Longitude",
  "SamplingPoint_Latitude",
  "AQValue"
)]

names(test_pm25) <- c("year", "id", "long", "lat", "value")

usethis::use_data(test_pm25, overwrite = TRUE)

