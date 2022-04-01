## code to prepare `test_restaurant` dataset goes here

# test_restaurant <- read.csv("data-raw/test_rest.csv") %>% 
#   group_by(id) %>% 
#   arrange(inspect_dt) %>% 
#   mutate(cs = cumsum(v_level)) %>% 
#   ungroup()

test_restaurant <- read.csv("data-raw/test_rest_address.csv")

usethis::use_data(test_restaurant, overwrite = TRUE)
