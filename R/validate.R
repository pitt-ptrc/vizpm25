
#' Validate file format
#'
#' @param df 
#' @param req_cols 
#' @param opt_cols 
#'
#' @return df
#' @export
#'
#' @examples
#' tdf <- data.frame(
#'   id = "123",
#'   address = "1600 Pennsylvania Avenue NW, Washington, DC 20500",
#'   something = "something"
#' )
#' 
#' validate_format(tf)
#' @importFrom shiny validate need

validate_format <- function(df, req_cols = c("address", "id"), opt_cols = c("lat", "lng")) {
  
  data_cols <- colnames(df)
  
  req_cols <- c("address", "id")
  opt_cols <- c("lat", "lng")
  per_cols <- c(req_cols, opt_cols)
  
  validate(
    need(sum(req_cols %in% data_cols) == length(req_cols), "Required columns not present"),
    need(sum(!data_cols %in% per_cols) == 0, "Some columns not permitted"),
    need(nrow(df) == length(unique(df$address)), "Addresses are not unique"),
    need(nrow(df) == length(unique(df$id)), "IDs are not unique"),
    need(sum(grepl("P\\.O\\.|PO Box", df$address, ignore.case = TRUE)) == 0, "P.O. boxes are not valid")
  )
}

#' Validate address with USPS
#'
#' @param address 
#'
#' @return tibble with validation
#' @export
#'
#' @examples
#' address <- "1600 Pennsylvania Avenue NW, Washington, DC 20500"
#' 
#' #' validate_format(tf)
#' 
#' @importFrom dplyr mutate n rename matches pull bind_cols
#' @importFrom magrittr %>%
#' @importFrom tidygeocoder geocode
#' @importFrom purrr map
#' @importFrom tidyr unite
#' @importFrom data.table rbindlist
#' @importFrom addressr validateAddress
#' @importFrom tibble tibble

validate_address <- function(address) {
  
  req_cols <-
    c(
      "AddressNumber",
      "StreetName",
      "PlaceName",
      "StateName",
      "ZipCode"
    )
  
  address_parsed <- 
    address %>% 
    map(usaddress$tag) %>% 
    map(1) %>% 
    rbindlist(fill = TRUE)
  
  if (sum(is.element(colnames(address_parsed), req_cols)) < length(req_cols)) {
    stop("`address` must contain at least one complete address")
  }
  
  address_parsed %>% 
    unite("Address1", matches("Address|Street"), sep = " ", na.rm = TRUE) %>% 
    rename(City = PlaceName, State = StateName, Zip5 = ZipCode) %>% 
    mutate(Zip4 = NA, Address2 = NA) %>% 
    validateAddress(userid = "558UNIVE2177", address = .) %>% 
    tibble() %>% 
    unite("address_main", Address2, City, State, sep = ", ") %>% 
    mutate(zip5 = Zip5) %>% 
    unite("zip9", Zip5, Zip4, sep = "", remove = FALSE) %>% 
    mutate(zip = zip9 %>% as.integer()) %>% 
    unite("address_zip", Zip5, Zip4, sep = "-") %>% 
    unite("address", address_main, address_zip, sep = " ")
}
