library(weathercan)
library(dplyr)
library(lubridate)
library(stringr)
library(opendatatoronto)
library(fuzzyjoin)


# load the data resource 
neighborhood <- read.csv('/Users/mac/Documents/Toronto-Autotheft-Prediction_1/neighbourhood-profiles-2016-csv.csv',stringsAsFactors = FALSE)
dt <- readRDS("/Users/mac/Documents/Toronto-Autotheft-Prediction-data_exploration/data/auto_theft_data.rdata")

# Select Relative Columns from Raw Neighborhood Data Frame
clean_neighborhood_dt <- function(raw_neighborhood_dt) {
  
  # Delete columns that are not useful
  data <- raw_neighborhood_dt %>% 
    dplyr:: select(-c(1:5)) 
 
  # Convert columns into rows
  data <- data.frame(t(data[-1])) 
 
  # Select useful columns and rename them
  data <- data %>% 
    dplyr:: select(c(1,3,5,10,11,12,13,14,15))
  names(data) <- c('Hood_ID', 'ppl_2016','annual_growth_rate','children_2016' ,'youth_2016','working_2016','pre-retirement_2016','seniors_2016','older_2016')
  
  # Convert columns from character to numeric
  i = c(1:9)
  data[,i] = apply(data[ , i], 2, function(x) as.numeric(x))
  
  # Add estimated population for 2017, 2018, 2019
  data <- data %>% 
    dplyr:: mutate(ppl_2017 = round(ppl_2016 * (1+annual_growth_rate)^1)) %>% 
    dplyr:: mutate(ppl_2018 = round(ppl_2016 * (1+annual_growth_rate)^2)) %>% 
    dplyr:: mutate(ppl_2019 = round(ppl_2016 * (1+annual_growth_rate)^3))
  
  # Estimate population for different ages in 2017
  data[,c('children_2017','youth_2017','working_2017','pre.retirement_2017','seniors_2017','older_2017')] = NA
  col_num = c(13,14,15,16,17,18)
  
  for (i in col_num) {
    data[i] = round(data[i-9] * (1+data[3]))
  }
  
  # Estimate population for different ages in 2018
  data[,c('children_2018','youth_2018','working_2018','pre.retirement_2018','seniors_2018','older_2018')] = NA
  col_num = c(19,20,21,22,23,24)
  
  for (i in col_num) {
    data[i] = round(data[i-6] * (1+data[3]))
  }
  
  # Estimate population for different ages in 2019
  data[,c('children_2019','youth_2019','working_2019','pre.retirement_2019','seniors_2019','older_2019')] = NA
  col_num = c(25,26,27,28,29,30)
  
  for (i in col_num) {
    data[i] = round(data[i-6] * (1+data[3]))
  }
  
 return(data)
  
}

# Select Relative Columns from Raw Auto Data Frame
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

neighbor_dt = clean_neighborhood_dt(neighborhood)
auto_dt <- clean_auto_dt(dt)

# Join Neighborhood Data Into Auto Data
join_auto_neighbor <- function(auto_data, neighbor_data) {
  
  auto_dt_neighbor <- auto_dt %>% 
    dplyr::left_join(neighbor_dt , by = 'Hood_ID')
  
  return(auto_dt_neighbor)
}

auto_dt_neighbor <- join_auto_neighbor(auto_dt, neighbor_dt)
write.csv(auto_dt_neighbor,"/Users/mac/Documents/Toronto-Autotheft-Prediction-data_exploration/data/auto_neighbor.csv")
saveRDS(neighbor_dt, "/Users/mac/Documents/Toronto-Autotheft-Prediction-data_exploration/data/neighbor_dt_raw.rdata")
saveRDS(auto_dt_neighbor, "/Users/mac/Documents/Toronto-Autotheft-Prediction-data_exploration/data/auto_neighbor_dt.rdata")
