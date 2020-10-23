dt <- readRDS("~/Desktop/Toronto police case/Toronto-Autotheft-Prediction/data/auto_theft_data.rdata")
library(weathercan)
library(dplyr)
library(lubridate)
library(stringr)

clean_auto_dt <- function(raw_auto_dt) {
  
  # X, Y, Index_, ObjectId, ucr_code, ucr_ext are not useful remove
  dt <- raw_auto_dt %>% 
    dplyr::select(-c(X, Y, Index_, ObjectId, ucr_code, 
                     ucr_ext, event_unique_id, MCI)) %>% 
    dplyr::distinct()
  
  dt <- dt %>% 
    dplyr:: mutate(occurrence_ts = lubridate::ymd_hms(occurrencedate),
                   reported_ts = lubridate::ymd_hms(reporteddate)) %>% 
    dplyr::select(-occurrencedate, -reporteddate, -dplyr::starts_with("report")) %>% 
    dplyr::filter(lubridate::year(occurrence_ts) >= 2016) %>% 
    dplyr::mutate(occurrencemonth = lubridate::month(occurrence_ts))
  
}

get_station_id <- function(year, lat, long) {
  
  station_list <- weathercan::stations_search(coords = c(lat, long), dist = 30, interval = "hour") %>% 
    dplyr::filter(# normals == TRUE,
                  !is.na(WMO_id),
                  start < year, 
                  end >= year) %>% 
    dplyr::arrange(distance) %>% 
    dplyr::slice(1)
  
  station_id <- station_list$station_id
  
  return(station_id)
  
}

get_clean_weather <- function(station_id = 31688, 
                              start_date = "2016-01-01", 
                              end_date = "2020-01-01") {
  
  weather_dt <- weathercan::weather_dl(station_ids = station_id, start = start_date, end = end_date)
  
  weather_clean <- weather_dt %>% 
    dplyr::mutate(year = as.numeric(year),
                  month = as.numeric(month),
                  day = as.numeric(day),
                  hour = as.numeric(stringr::str_replace(hour, ":00", ""))) %>% 
    dplyr::select(-c(prov, lat, lon, elev, climate_id, 
                     WMO_id, TC_id, date, station_operator, time))
  
  return(weather_clean)
  
}

# auto_dt_station <- auto_dt %>% 
#   rowwise() %>% 
#   mutate(station_id = get_station_id(year = occurrenceyear, lat = Lat, long = Long))

weather_dt <- get_clean_weather()

saveRDS(weather_dt, "~/Desktop/Toronto police case/Toronto-Autotheft-Prediction/data/weather_dt_raw.rdata")

# join auto_theft data with weather data

auto_dt <- clean_auto_dt(dt)

join_auto_weather <- function(auto_dt, weather_dt) {
  
  auto_dt_weather <- auto_dt %>% 
    dplyr::left_join(weather_dt, by = c("occurrenceyear" = "year",
                                        "occurrencemonth" = "month",
                                        "occurrenceday" = "day",
                                        "occurrencehour" = "hour")) %>% 
    dplyr::arrange(occurrence_ts) %>% 
    tidyr::fill(weather:wind_spd_flag,.direction = "down") %>% 
    dplyr::select(-station_name, -station_id)
  
  return(auto_dt_weather)
  
}

auto_dt_weather <- join_auto_weather(auto_dt, weather_dt)

