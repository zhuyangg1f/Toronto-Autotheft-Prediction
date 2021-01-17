#' Clean Raw Neighborhood Population Data from Statistics Canada
#' 
#' The function removes the redundant columns in the raw data and renames retained columns,
#' so that the neighborhood population information can be joined to auto theft data by Hood_ID.
#' 
#' @param raw_neighborhood_dt, a tibble object, raw neighborhood data from Statistics Canada
#' @import dplyr
#' @export
#' 

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

#' Clean Raw Auto Theft Data from Toronto Police
#' 
#' The function removes the redundant columns and duplicated rows in the raw data. It also removes
#' all data before 2016 since the data quality is poor.
#' 
#' @param raw_auto_dt, a tibble object, raw auto theft data from Toronto Police
#' @import dplyr
#' @import lubridate
#' @export
#' 

clean_auto_dt <- function(raw_auto_dt) {
  
  # X, Y, Index_, ObjectId, ucr_code, ucr_ext are not useful remove
  dt <- raw_auto_dt %>% 
    # remove columns might cause duplicaton
    dplyr::select(-c(X, Y, Index_, ObjectId, ucr_code, 
                     ucr_ext)) %>% 
    # remove duplicated row
    dplyr::distinct() %>% 
    # remove redundant columns
    dplyr::select(-c(event_unique_id, MCI, premisetype,
                     offence))
  
  dt <- dt %>% 
    dplyr::mutate(occurrence_ts = lubridate::ymd_hms(occurrencedate)) %>% 
    dplyr::select(-occurrencedate, -dplyr::starts_with("report")) %>% 
    dplyr::filter(lubridate::year(occurrence_ts) >= 2014) %>% 
    dplyr::mutate(occurrencemonth = lubridate::month(occurrence_ts))
    # remove columns, since the information is already in occurrence_ts
    # dplyr::select(-c(occurrenceyear, occurrencemonth, occurrenceday,
    #                  occurrencedayofyear, occurrencedayofweek, 
    #                  occurrencehour))
  
  return(dt)
  
}

# get_station_id() has been archived since we only use station 31688 to represent 
# the weather in GTA area

# get_station_id <- function(year, lat, long) {
#   
#   station_list <- weathercan::stations_search(coords = c(lat, long), dist = 30, interval = "hour") %>% 
#     dplyr::filter(# normals == TRUE,
#       !is.na(WMO_id),
#       start < year, 
#       end >= year) %>% 
#     dplyr::arrange(distance) %>% 
#     dplyr::slice(1)
#   
#   station_id <- station_list$station_id
#   
#   return(station_id)
#   
# }

#' Extract Historical Weather Data
#' 
#' The function extracts historical weather data from `weathercan` package. The default is to extract
#' the weather information in GTA Area. Blank columns are removed to minimize the object size of data
#' 
#' @param station_id, an integer object, states the station id 
#' @param start_date, a character object, the start date of interest for historical weather
#' @param end_date, a character object, the end date of interest for historical weather
#' @import dplyr
#' @import weathercan
#' @import stringr
#' @export
#' 

get_historical_weather <- function(station_id = 31688, 
                                   start_date = "2014-01-01", 
                                   end_date = "2020-01-01") {
  
  weather_dt <- weathercan::weather_dl(station_ids = station_id, start = start_date, end = end_date)
  
  weather_clean <- weather_dt %>% 
    dplyr::mutate(year = as.numeric(year),
                  month = as.numeric(month),
                  day = as.numeric(day),
                  hour = as.numeric(stringr::str_replace(hour, ":00", ""))) %>% 
    dplyr::select(-c(station_name, station_id, prov, lat, lon, elev, climate_id, 
                     WMO_id, TC_id, date, station_operator, time, hmdx, precip_amt_flag,
                     wind_spd, wind_dir,  wind_chill_flag, wind_chill, pressure,
                     visib_flag, visib, temp_flag, temp_dew_flag, rel_hum_flag, 
                     hmdx_flag, weather, pressure_flag, wind_spd_flag, wind_dir_flag))
  
  return(weather_clean)
  
}

#' Join Weather Data
#' 
#' The function joins historical weather data from `get_historical_weather()` to another dataframe
#' with time series columns. It also imputes missing values in historical weather data
#' 
#' @param data, a tibble object, states the data to be joined
#' @param weather_dt, a tibble object, neighborhood's population structure from `get_historical_weather()`
#' @import dplyr
#' @import tidyr
#' @export
#'

join_weather <- function(data, weather_dt) {
  
  auto_weather_dt <- data %>% 
    dplyr::left_join(weather_dt, by = c("year","month","day","hour")) %>% 
    dplyr::arrange(occurrence_ts) %>% 
    tidyr::fill(precip_amt:temp_dew,.direction = "downup")
  
  return(auto_weather_dt)
  
}

#' Join Neighborhood Data
#' 
#' The function joins neighborhood population data from `clean_raw_neighbor()` to another data 
#' frame with `Hood_ID` and `year` columns. It calculates the population structure for each
#' neighborhood in current year.
#' 
#' @param data, a tibble object, states the data to be joined
#' @param neighbor_2016_dt, a tibble object, neighborhood's population structure from `clean_raw_neighbor()`
#' @import dplyr
#' @export
#'

join_neighbor <- function(data, neighbor_2016_dt) {
  
  auto_neighbor_dt <- data %>% 
    dplyr::left_join(neighbor_2016_dt, by = "Hood_ID") %>% 
    dplyr::mutate(current_ppl = ppl_2016 * (1 + annual_growth_rate)^(year - 2016),
                  current_children_ppl = children_2016 * (1 + annual_growth_rate)^(year - 2016),
                  current_youth_ppl = youth_2016 * (1 + annual_growth_rate)^(year - 2016),
                  current_working_ppl = working_2016 * (1 + annual_growth_rate)^(year - 2016),
                  current_pre_retirement_ppl = pre_retirement_2016 * (1 + annual_growth_rate)^(year - 2016),
                  current_seniors_ppl = seniors_2016 * (1 + annual_growth_rate)^(year - 2016),
                  current_older_ppl = older_2016 * (1 + annual_growth_rate)^(year - 2016)) %>% 
    dplyr::select(-ends_with("_2016"))
  
  return(auto_neighbor_dt)
  
}

#' Add Geo-Spatial Grid
#' 
#' The function adds geo-spatial grids to auto theft data by using latitude and longitude
#' 
#' @param data, a tibble object, states auto theft data with latitude and longitude columns
#' @param breaks_num, an integer object, states the number of grids on each edge would be used
#' @export
#'

add_geo_grid <- function(data, breaks_num = 45) {
  
  ## Add the latitudes and longitudes between which each observation is located
  ## You can substitute any number of breaks you want. Or, a vector of fixed cutpoints
  ## LATgrid and LONgrid are going to be factors. With ugly level names.
  data$LATgrid <- cut(data$Lat, breaks = breaks_num, include.lowest=T);
  data$LONgrid <- cut(data$Long, breaks = breaks_num, include.lowest=T);
  
  ## Create a single factor that gives the lat,long of each observation. 
  data$IDgrid <- with(data,interaction(LATgrid, LONgrid));
  
  ## Now, create another factor based on the above one, with shorter IDs and no empty levels
  data$IDNgrid <- factor(data$IDgrid); 
  levels(data$IDNgrid) <- seq_along(levels(data$IDNgrid));
  data$IDNgrid <- as.character(data$IDNgrid)
  
  return(data)
  
}

#' Create Complete Time Series By Group
#' 
#' The function creates a time series in each strata. It imputes the missing time series 
#' points for a data.
#' 
#' @param dt, a tibble object, states the data need be imputed.
#' @param colnames, a character vectors, states the columns used as imputation stratum
#' @import dplyr
#' @export
#'

create_complete_group_ts <- function(dt, col_names) {
  
  date_seq <- seq(from = min(dt$occurrence_ts), 
                  to = max(dt$occurrence_ts), by="hour")
  
  output <- data.frame(occurrence_ts = date_seq, v = 1)
  
  for(i in 1:length(col_names)){
    
    tmp <- data.frame(title = unique(dt[[col_names[i]]]), v = 1)
    colnames(tmp) <- c(col_names[i], "v")
    
    output <- output %>% 
      dplyr::full_join(tmp, by = "v")
    
  }
  
  output <- output %>% 
    dplyr::select(-v) %>% 
    dplyr::left_join(dt, by = c("occurrence_ts", col_names))
  
  return(output)
  
}

#' Prepare Auto Theft Data For Modeling
#' 
#' The function preprocess raw auto theft data into a format ready to be modelled. It creates 
#' dichotomous outcome for each hour in each grid to show weather there is auto theft event in
#' that grid at specific time. It also joins weather data and population data at each time point.
#' Please note, we made one-to-one mapping assumption to link grid to Hood_ID and Division here, 
#' which simplifies the data pre-processing approach.
#' 
#' @param raw_auto_data, a tibble object, raw auto theft data from Toronto Police
#' @param raw_neighbor_2016, a tibble object, raw neighborhood population data in 2016 
#' from Statistics Canada
#' @param data_path, a character object, states the data path for saving intermediate 
#' grid mapping data
#' @param save_intermediate, a boolean object, states whether to save the intermediate 
#' grid mapping data
#' @import dplyr
#' @export
#'

prepare_model_data <- function(raw_auto_data, 
                               raw_neighbor_2016,
                               data_path = NULL, 
                               save_intermediate = TRUE) {
  
  # clean raw_auto_data
  clean_auto_data <- clean_auto_dt(raw_auto_dt)

  # add geo-spatial grid 
  grid_dt <- add_geo_grid(clean_auto_data, breaks_num = 40)
  
  grid_mapping <- grid_dt %>% 
    dplyr::group_by(IDNgrid, IDgrid) %>% 
    dplyr::count() %>% 
    dplyr::ungroup()
  
  grid_to_hood <- grid_dt %>% 
    dplyr::group_by(IDNgrid, Hood_ID) %>% 
    dplyr::count() %>%
    dplyr::ungroup() 
  
  grid_to_division <- grid_dt %>% 
    dplyr::group_by(IDNgrid, Division) %>% 
    dplyr::count() %>%
    dplyr::ungroup() 
  
  # save grid mapping data
  if(save_intermediate == TRUE) {
    
    write.csv(grid_mapping, file.path(data_path, "grid_mapping.csv"), row.names = FALSE)
    write.csv(grid_to_hood, file.path(data_path, "grid_to_hood.csv"), row.names = FALSE)
    write.csv(grid_to_division, file.path(data_path, "grid_to_division.csv"), row.names = FALSE)
    
  }
  
  # One-to-one mapping
  grid_to_hood <- grid_to_hood %>% 
    dplyr::group_by(IDNgrid) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-n)
  
  grid_to_division <- grid_to_division %>% 
    dplyr::group_by(IDNgrid) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-n)
  
  # make data into binary
  binary_dt <- grid_dt %>% 
    dplyr::group_by(IDNgrid, occurrenceyear, occurrencemonth, occurrenceday, occurrencehour) %>% 
    # for each hour, can only have one event
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(occurrence_ts, IDNgrid) %>% 
    dplyr::mutate(occurrence_ts = lubridate::floor_date(lubridate::ymd_hms(occurrence_ts), unit = "hour")) %>% 
    # remove these columns for now, the occurrence time would be added back later
    dplyr::select(-c(occurrenceyear, occurrencemonth, occurrenceday, occurrencedayofyear,
                     occurrencedayofweek, occurrencehour, Lat, Long, LATgrid, LONgrid, IDgrid))
  
  ## get population info by neighborhood
  clean_neighbor_dt <- clean_raw_neighbor(raw_neighbor_2016)
  
  ## get weather info by time
  clean_weather_dt <- get_historical_weather()
  
  ## create complete time series for each grid each hour, we need to impute time series 
  ## (2014-01-01 to 2019-12-31) for each grid
  complete_ts_join <- create_complete_group_ts(dt = binary_dt, col_names = c("IDNgrid")) %>% 
    dplyr::mutate(outcome = if_else(is.na(Hood_ID), 0, 1)) %>% 
    dplyr::select(-Division, -Hood_ID, -Neighbourhood) %>% 
    dplyr::left_join(grid_to_hood, by = "IDNgrid") %>% 
    dplyr::left_join(grid_to_division, by = "IDNgrid") %>% 
    dplyr::mutate(year = lubridate::year(occurrence_ts),
                  month = lubridate::month(occurrence_ts),
                  day = lubridate::mday(occurrence_ts),
                  hour = lubridate::hour(occurrence_ts)) %>% 
    join_neighbor(clean_neighbor_dt) %>% 
    dplyr::select(-Hood_ID) %>% 
    join_weather(clean_weather_dt)
  
  # Archived code in case we want to do larger time interval prediction
  # dplyr::mutate(occurrence_time_interval = dplyr::case_when(
  #                 hour >= 1 & hour <= 6 ~ "night",
  #                 hour >= 7 & hour <= 12 ~ "morning",
  #                 ehour >= 13 & hour <= 18 ~ "afternoon",
  #                 (hour >= 19 & hour <= 23) | hour == 0 ~ "evening",
  #                 TRUE ~ "Unknown Time"
  #                 )
  
  return(complete_ts_join)
}