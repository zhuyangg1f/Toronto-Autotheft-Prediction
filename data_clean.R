library(dplyr)
library(lubridate)
library(stringr)
library(opendatatoronto)
library(fuzzyjoin)


# load the data resource 
neighborhood <- read.csv('/Users/mac/Documents/Toronto-Autotheft-Prediction-data_exploration/data/neighbourhood-profiles-2016-csv.csv',stringsAsFactors = FALSE)
dt <- readRDS("/Users/mac/Documents/Toronto-Autotheft-Prediction-data_exploration/data/auto_theft_data.rdata")

# Select Relative Columns from Raw Neighborhood Data Frame
clean_auto_neighbor_dt <- function(raw_neighborhood_dt) {
  
  # Delete columns that are not useful
  data <- raw_neighborhood_dt %>% 
    dplyr:: select(-c(1:5)) 
 
  # Convert columns into rows
  data <- data.frame(t(data[-1])) 
 
  # Select useful columns and rename them
  ppl_2016 <- data %>% 
    dplyr:: select(c(1,3,5,10,11,12,13,14,15))
  names(ppl_2016) <- c('Hood_ID', 'ppl_2016','annual_growth_rate','children_2016' ,'youth_2016','working_2016','pre-retirement_2016','seniors_2016','older_2016')
  
  # Convert columns from character to numeric
  i = c(1:9)
  ppl_2016[,i] = apply(ppl_2016[ , i], 2, function(x) as.numeric(x))
  
  
  # Estimate population for different ages in 2017
  ppl_2017 <- ppl_2016 %>% 
    dplyr:: transmute(ppl_2017 = round(ppl_2016 * (1+annual_growth_rate)^1))
  
  ppl_2017[,c('children_2017','youth_2017','working_2017','pre.retirement_2017','seniors_2017','older_2017')] = NA
  col_num = c(2,3,4,5,6,7)
  
  for (i in col_num) {
    ppl_2017[i] = round(ppl_2016[i+2] * (1+ppl_2016[3]))
  }
  
  # Estimate population for different ages in 2018
  ppl_2018 <- ppl_2016 %>% 
    dplyr:: transmute(ppl_2018 = round(ppl_2016 * (1+annual_growth_rate)^2))
  
  ppl_2018[,c('children_2018','youth_2018','working_2018','pre.retirement_2018','seniors_2018','older_2018')] = NA
  col_num = c(2,3,4,5,6,7)
  
  for (i in col_num) {
    ppl_2018[i] = round(ppl_2016[i+2] * (1+ppl_2016[3])^2)
  }
  
  # Estimate population for different ages in 2019
  ppl_2019 <- ppl_2016 %>% 
    dplyr:: transmute(ppl_2019 = round(ppl_2016 * (1+annual_growth_rate)^3))
  
  ppl_2019[,c('children_2019','youth_2019','working_2019','pre.retirement_2019','seniors_2019','older_2019')] = NA
  col_num = c(2,3,4,5,6,7)
  
  for (i in col_num) {
    ppl_2019[i] = round(ppl_2016[i+2] * (1+ppl_2016[3])^3)
  }
  
 comb = cbind(ppl_2016,ppl_2017,ppl_2018,ppl_2019)
  return(comb)
  
}

neighbor_data = clean_auto_neighbor_dt(neighborhood)


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

auto_dt <- clean_auto_dt(dt)

# Join Neighborhood Data Into Auto Data
join_auto_neighbor <- function(auto_data, neighbor_data) {
  
  neighbor_2016 = neighbor_data[1:9]
  auto_dt_neighbor_16 <- auto_dt %>% filter(occurrenceyear == 2016) %>% 
    dplyr::left_join(neighbor_2016 , by = 'Hood_ID')
  
  neighbor_2017 = neighbor_data[c(1,10:16)]
  auto_dt_neighbor_17 <- auto_dt %>% filter(occurrenceyear == 2017) %>% 
    dplyr::left_join(neighbor_2017 , by = 'Hood_ID')
  
  neighbor_2018 = neighbor_data[c(1,17:23)]
  auto_dt_neighbor_18 <- auto_dt %>% filter(occurrenceyear == 2018) %>% 
    dplyr::left_join(neighbor_2018 , by = 'Hood_ID')
  
  neighbor_2019 = neighbor_data[c(1,24:30)]
  auto_dt_neighbor_19 <- auto_dt %>% filter(occurrenceyear == 2019) %>% 
    dplyr::left_join(neighbor_2019 , by = 'Hood_ID')
  
  comb1 = merge(auto_dt_neighbor_16, auto_dt_neighbor_17,all  = TRUE)
  comb2 = merge(comb1, auto_dt_neighbor_18, all=TRUE)
  comb3 = merge(comb2,auto_dt_neighbor_19,all= TRUE)
  
  return(comb3)
}

auto_dt_neighbor = join_auto_neighbor(auto_dt,neighbor_data)
write.csv(auto_dt_neighbor,"/Users/mac/Documents/Toronto-Autotheft-Prediction-data_exploration/data/auto_neighbor.csv")
saveRDS(auto_dt_neighbor, "/Users/mac/Documents/Toronto-Autotheft-Prediction-data_exploration/data/auto_neighbor_dt.rdata")


