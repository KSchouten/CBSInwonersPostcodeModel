install.packages("tidyverse", "tidymodels", "sf", "shiny","shinytest2", "shinyvalidate", "plotly", "leaflet", "htmltools", "waiter", "ranger")
# To run these tests you have to have Chrome installed
chromote::find_chrome()
rsconnect::writeManifest()

# Option 1
# split the pc4_data into yearly data to lessen the memory load inside the app
readRDS("data/pc4_data.Rds") %>% dplyr::group_split(jaar) %>%
  walk2(2015:2023, ~saveRDS(.x, str_c("data/pc4_data_", .y, ".Rds")))

# Option 2
# make the pc4_data smaller by removing useless columns
readRDS("data/pc4_data.Rds") %>% select(jaar, postcode, aantalInwoners) %>% saveRDS("data/pc4_data_small.Rds")
# only keep the geomtry data for one year, assuming that it stays the same over time
readRDS("data/pc4_data.Rds") %>% arrange(postcode, desc(jaar)) %>%
  select(postcode, geometry) %>% distinct(postcode, .keep_all = TRUE) %>% saveRDS("data/pc4_geometry.Rds")

