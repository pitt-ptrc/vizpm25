library(tidyverse)
library(tidygeocoder)
library(plotly)

test_pm25 %>% 
  ggplot(aes(year, value, group = id)) +
  geom_path()

set.seed(1)

test_pm25 %>% 
  tibble() %>%
  select(id) %>% 
  distinct() %>% 
  slice_sample(n = 20) %>% 
  write_csv("data-raw/station_sample.csv")



# scratch -----------------------------------------------------------------

library(tidyverse)
library(tidygeocoder)

# rest <- read_csv("../../Downloads/alco-restuarant-violations.csv")

rest %>% glimpse()

rest_sub <- 
  rest %>% 
  filter(city == "Pittsburgh") %>% 
  filter(inspect_dt > as.Date("2018-01-01")) %>% 
  group_by(id) %>% 
  filter(n() > 10) %>% 
  ungroup() %>% 
  filter(str_detect(description, "Restaurant")) %>% 
  mutate(chain = if_else(str_detect(description, "Chain"), TRUE, FALSE)) %>% 
  mutate(liquor = if_else(str_detect(description, "Liquor"), TRUE, FALSE)) %>% 
  filter(rating == "V")

rest_sub %>% 
  mutate(
    v_level = case_when(
      low ~ 1,
      medium ~ 2,
      high ~ 3
    )) %>% 
  group_by(id) %>% 
  arrange(inspect_dt) %>% 
  mutate(cs = cumsum(v_level)) %>% 
  ungroup() %>% 
  filter(!is.na(cs)) %>% 
  ggplot(aes(inspect_dt, cs, group = id)) +
  geom_path()

set.seed(1)

ids <- 
  rest_sub$id %>% 
  unique() %>% 
  sample(10)

test_rest <-
  rest_sub %>% 
  filter(is.element(id, ids)) %>% 
  mutate(
    v_level = case_when(
      low ~ 1,
      medium ~ 2,
      high ~ 3
    )) %>% 
  unite(street_address, c("num", "street"), sep = " ") %>% 
  unite(state_zip, c("state", "zip"), sep = " ") %>% 
  unite(address, c("street_address", "city", "state_zip"), sep = ", ")

test_rest %>% 
  write_csv("data-raw/test_rest.csv")

test_test <- test_rest %>% 
  group_by(id) %>% 
  arrange(inspect_dt) %>% 
  mutate(cs = cumsum(v_level)) %>% 
  ungroup()

test_test %>% View()

test_test %>% 
  ggplot(aes(inspect_dt, cs, group = id)) +
  geom_path()

test_rest_address <- 
  test_rest %>% 
  select(id, facility_name, address) %>% 
  distinct()

test_rest_address %>% 
  write_csv("data-raw/test_rest_address.csv")

test_rest_address <- 
  read_csv("data-raw/test_rest_address.csv")


test_rest_address %>% View()
test_rest_geo <- test_rest_address %>% 
  geocode(address, method = 'osm', lat = latitude , long = longitude)

test_rest_geo

test_rest_geo_cen <- test_rest_address %>% 
  geocode(address, method = 'census', lat = latitude , long = longitude)

test_rest_geo_cen

leftover <- test_rest_geo_cen %>%
  filter(latitude %>% is.na()) %>%
  geocode(
    address,
    method = 'census',
    lat = latitude ,
    long = longitude,
    mode = 'single',
    full_results = TRUE,
    return_type = 'geographies'
  )

leftover %>% View()

test_rest_geo_goo <- test_rest_address %>% 
  geocode(address = address, method = "google", lat = latitude , long = longitude)


test_rest_geo_goo

library(dplyr, warn.conflicts = FALSE)
library(tidygeocoder)

# create a dataframe with addresses
some_addresses <- tibble::tribble(
  ~name,                  ~addr,
  "White House",          "1600 Pennsylvania Ave NW, Washington, DC",
  "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",     
  "Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606"                                  
)

# geocode the addresses
lat_longs <- some_addresses %>%
  geocode(addr, method = 'google', lat = latitude , long = longitude)
#> Passing 3 addresses to the Nominatim single address geocoder
#> Query completed in: 3 seconds

lat_longs


some_addresses <- tibble::tribble(
  ~name,                  ~addr,
  "White House",          "1600 Pennsylvania Ave NW, Washington, DC",
  "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",     
  "Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606"                                  
)

# geocode the addresses
lat_longs <- some_addresses %>%
  geocode(addr, method = 'google', lat = latitude , long = longitude)

lat_longs
