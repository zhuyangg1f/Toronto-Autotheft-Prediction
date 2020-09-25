# Data cleaning --------------------------------------------------------------
# Yang Zhu

# load packages --------------------------------------------------------------
library(dplyr)
library(lubridate)

# load data ------------------------------------------------------------------
dt <- readRDS("~/Desktop/Toronto police case/Toronto-Autotheft-Prediction/data/auto_theft_data.rdata")
glimpse(dt)

# check duplicated index
length(unique(dt$Index_)) # keep all
length(unique(dt$event_unique_id)) # There are duplicated event id
length(unique(dt$ObjectId))

duplicated_rows <- dt %>% 
  filter(event_unique_id %in% dt$event_unique_id[duplicated(dt$event_unique_id)]) %>% 
  arrange(event_unique_id)

# X, Y, Index_, ObjectId, ucr_code, ucr_ext are not useful remove
dt <- dt %>% 
  select(-c(X, Y, Index_, ObjectId, ucr_code, ucr_ext)) %>% 
  distinct()

# transform date colum into date object
dt <- dt %>% 
  mutate(occurrence_ts = ymd_hms(occurrencedate),
         reported_ts = ymd_hms(reporteddate)) %>% 
  select(-occurrencedate, -reporteddate, -starts_with("report")) %>% 
  filter(year(occurrence_ts) >= 2016)

# save data as data_clean.rdata
saveRDS(dt, "~/Desktop/Toronto police case/Toronto-Autotheft-Prediction/data/autotheft_clean.rdata")
