## code to prepare `test_rest_valid` dataset goes here

library(reticulate)
library(addressr)
library(tidyverse)


# python address
virtualenv_create("testenv3", packages = c("usaddress"))
use_virtualenv("testenv3", required = TRUE)
usaddress <- import("usaddress")

test_rest_valid <- 
  test_restaurant %>%
  pull(address) %>% 
  map(usaddress$tag) %>% 
  map(1) %>% 
  data.table::rbindlist(fill = TRUE) %>% 
  unite("Address1", matches("Address|Street"), sep = " ", na.rm = TRUE) %>% 
  rename(City = PlaceName, State = StateName, Zip5 = ZipCode) %>% 
  mutate(Zip4 = NA, Address2 = NA) %>% 
  validateAddress(userid = "558UNIVE2177", address = .) %>% 
  tibble() %>% 
  unite("address_main", Address2, City, State, sep = ", ") %>% 
  unite("zip9", Zip5, Zip4, sep = "", remove = FALSE) %>% 
  mutate(zip = zip9 %>% as.integer()) %>% 
  unite("address_zip", Zip5, Zip4, sep = "-") %>% 
  unite("address", address_main, address_zip, sep = " ")


usethis::use_data(test_rest_valid, overwrite = TRUE)
