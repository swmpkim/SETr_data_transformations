library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)


path <- here::here("data", "submitted", "2020-02-11_GTM.csv")
# using the one sent 2/10 i found some sites where the offset for
# the use of pin set 2 hadn't been applied. Pam quickly updated the file
# so this should be the last transformation for GTM.

# read in data; convert dates and times; add a reserve column
dat1 <- read.csv(path, stringsAsFactors = FALSE) %>% 
    clean_names() %>% 
    mutate(date = lubridate::mdy(date),
           year = year(date),
           month = month(date),
           day = mday(date),
           time2 = case_when(str_detect(time, ":") ~ str_remove(time, ":"),
                             TRUE ~ time),
           time_HH = str_sub(time2, start = 1L, end = -3L),
           time_MM = str_sub(time2, start = -2L, end = -1L),
           reserve = "GTM") 


# qaqc codes will need to be inserted at some point but since we're not excluding any
# i won't worry about it now

# check for problems

# nas in dates?
nadates <- dat1 %>% 
    filter(is.na(date))
# no. good.


# nas in adjusted pin height?
napinht <- dat1 %>% 
    filter(is.na(height_adj_mm))
# yes, 461. maybe these are just missing measurements?
# what i'm concerned about is raw data that just didn't get converted.
sum(!is.na(napinht$raw_height_mm))
# okay, if adjusted height is missing, raw is too
# this is good



# modify columns and select
dat1b <- dat1 %>% 
    mutate(site = as.character(site),
           site = case_when(nchar(site) == 1 ~ paste0("0", site),
                            TRUE ~ site),
           set_id = paste(site, station, sep = "-"),
           arm_position = paste0("arm_", position),
           pin_number = paste0("pin_", pin),
           arm_qaqc_code = NA_character_,
           qaqc_code = NA_character_) %>% 
    select(reserve, set_id, 
           year, month, day,
           arm_position, arm_qaqc_code,
           pin_number,
           height_mm = height_adj_mm, qaqc_code,
           everything()) %>% 
    select(-time, -time2, -pin, -position, -date, -site, -station)


write.csv(dat1b, file = here::here("data", "final", "gtmset_processed.csv"),
          row.names = FALSE)
