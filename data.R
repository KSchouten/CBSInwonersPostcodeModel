library(tidyverse)

retrieve_data <- function(){
  #https://nationaalgeoregister.nl/geonetwork/srv/dut/catalog.search#/metadata/e9841ae6-8f1d-460f-9506-a6b5ed2dc15f

  years <- 2015:2023
  #responses <- vector("list", length(years)) %>% setNames(years)
  features <- list()
  for(year in years){
    print(year)

    returned_values <- 1000
    i <- 0
    while(returned_values == 1000){
      url <- str_c("https://service.pdok.nl/cbs/postcode4/", year, "/wfs/v1_0?request=GetFeature&service=WFS&version=1.1.0&srsName=EPSG:4326&outputFormat=application%2Fjson&typeName=postcode4:postcode4&startIndex=",i)
      print(url)
      pcdata <- sf::st_read(url) %>% mutate(
        jaar = year,
        across(.cols = where(is.integer), .fns = ~if_else(. == -99995L,NA_integer_, .)),
        across(.cols = where(is.double), .fns = ~if_else(. == -99995.0,NA_real_, .)),
        across(.cols = where(is.integer), .fns = ~if_else(. == -99997L,0, .)),
        across(.cols = where(is.double), .fns = ~if_else(. == -99997.0,0, .))
      ) %>% mutate(across(.cols = where(is.character), .fns = as.factor)) %>%
        select(postcode, jaar, everything())
      features <- append(features, list(pcdata))
      print(pcdata$postcode[1])
      returned_values <- pcdata %>% nrow()
      print(returned_values)
      i <- i + 1000
    }
  }
  saveRDS(features, "raw_data/pc4_list_data.Rds")
  saveRDS(features %>% list_rbind(), "data/pc4_data.Rds")
}

write_to_db <- function(){
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = "ODBC Driver 18 for SQL Server",
                        server = "tcp:istarion.database.windows.net,1433",
                        database = "cbsdata",
                        uid = "CloudSA02646473",
                        pwd = "Eu40InR41ugUAil9GAdAKTaX",
                        timeout = 30)

  DBI::dbWriteTable(con, "pc4_data", readRDS("data/pc4_data.Rds") %>% select(-geometry), overwrite = TRUE, temporary = FALSE)
  DBI::dbWriteTable(con, "predictions", readRDS("data/predictions.Rds"), overwrite = TRUE, temporary = FALSE)
}


