clean_raw_neighbor <- function(raw_neighborhood_dt) {
  
  # Delete columns that are not useful
  data <- raw_neighborhood_dt %>% 
    dplyr::select(-c(1:5)) 
  
  # Convert columns into rows
  data <- data.frame(t(data[-1])) 
  
  # Select useful columns and rename them
  ppl_2016 <- data %>% 
    dplyr::select(c(1,3,5,10,11,12,13,14,15))
  
  names(ppl_2016) <- c('Hood_ID', 'ppl_2016','annual_growth_rate','children_2016' ,'youth_2016','working_2016','pre_retirement_2016','seniors_2016','older_2016')
  
  i = c(1:9)
  ppl_2016[,i] = apply(ppl_2016[ , i], 2, function(x) as.numeric(x))
  
  return(ppl_2016)
  
}

clean_auto_dt <- function(raw_auto_dt) {
  
  # X, Y, Index_, ObjectId, ucr_code, ucr_ext are not useful remove
  dt <- raw_auto_dt %>% 
    dplyr::select(-c(X, Y, Index_, ObjectId, ucr_code, 
                     ucr_ext, event_unique_id, MCI)) %>% 
    # remove duplicated row
    dplyr::distinct()
  
  dt <- dt %>% 
    dplyr::mutate(occurrence_ts = lubridate::ymd_hms(occurrencedate),
                  reported_ts = lubridate::ymd_hms(reporteddate)) %>% 
    dplyr::select(-occurrencedate, -reporteddate, -dplyr::starts_with("report")) %>% 
    dplyr::filter(lubridate::year(occurrence_ts) >= 2016) %>% 
    dplyr::mutate(occurrencemonth = lubridate::month(occurrence_ts))
  
  return(dt)
  
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

join_auto_weather <- function(auto_dt, weather_dt) {
  
  auto_weather_dt <- auto_dt %>% 
    dplyr::left_join(weather_dt, by = c("occurrenceyear" = "year",
                                        "occurrencemonth" = "month",
                                        "occurrenceday" = "day",
                                        "occurrencehour" = "hour")) %>% 
    dplyr::arrange(occurrence_ts) %>% 
    tidyr::fill(weather:wind_spd_flag,.direction = "down") %>% 
    dplyr::select(-station_name, -station_id)
  
  return(auto_weather_dt)
  
}

join_auto_neighbor <- function(auto_dt, neighbor_2016_dt) {
  
  auto_neighbor_dt <- auto_dt %>% 
    dplyr::left_join(neighbor_2016_dt, by="Hood_ID") %>% 
    dplyr::mutate(current_ppl = ppl_2016 * (1 + annual_growth_rate)^(occurrenceyear - 2016),
                  current_children_ppl = children_2016 * (1 + annual_growth_rate)^(occurrenceyear - 2016),
                  current_youth_ppl = youth_2016 * (1 + annual_growth_rate)^(occurrenceyear - 2016),
                  current_working_ppl = working_2016 * (1 + annual_growth_rate)^(occurrenceyear - 2016),
                  current_pre_retirement_ppl = pre_retirement_2016 * (1 + annual_growth_rate)^(occurrenceyear - 2016),
                  current_seniors_ppl = seniors_2016 * (1 + annual_growth_rate)^(occurrenceyear - 2016),
                  current_older_ppl = older_2016 * (1 + annual_growth_rate)^(occurrenceyear - 2016)) # %>% 
    # dplyr::select(-ends_with("_2016"))
  
  return(auto_neighbor_dt)
  
}
