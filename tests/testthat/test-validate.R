test_that("validate_format() validates", {
  
  tdf <- data.frame(
    id = "123",
    address = "1600 Pennsylvania Avenue NW, Washington, DC 20500"
  )
  
  tdfe <- data.frame(
    id = "123",
    address = "1600 Pennsylvania Avenue NW, Washington, DC 20500",
    something = "something"
  )
  
  expect_null(validate_format(tdf))
  expect_error(validate_format(tdfe))
})


test_that("validate_address validates real address", {
  address_1 <- "1600 Pennsylvania Avenue NW, Washington, DC 20500"
  address_valid_1 <- "1600 PENNSYLVANIA AVE NW, WASHINGTON, DC 20500-0005"
  
  address_2 <- "4200 Fifth Ave, Pittsburgh, PA 15260"
  address_valid_2 <- "4200 5TH AVE, PITTSBURGH, PA 15260-0001"
  
  expect_equal(address_valid_1, validate_address(address_1)$address)
  expect_equal(address_valid_2, validate_address(address_2)$address)
})

test_that("validate_address throws error when no complete addresses", {
  expect_error(validate_address("NA"))
  expect_error(validate_address("NAN"))
  expect_error(validate_address("4325 Haldane St, Pittsburgh"))
})
