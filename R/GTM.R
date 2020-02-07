library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)


path <- here::here("data", "submitted", "2019-05-08_GTM.xlsx")

# in the "Data" worksheet, times are in different formats
# some are e.g. 1245 and others are 12:45
# comments below were in my original plan, but i've run out of timme
# and am now only going to pull in columns i need so we can 
# include the data in analyses.
# workflow stuff will have to come at a later date.

# ignore below comments ----
# so i'll paste date and time together, let lubridate::ymd_hm deal with it,
# then turn it into a character and separate it back out again
# then time is in character format, which seems good
# note that some times were NA, which screwed up ymd_hm, so I inserted 23:59 as a time when necessary
# and turn it back into NA at the end
# -------

dat1 <- read_excel(path, sheet = "Data") %>% 
        clean_names() %>% 
        mutate(date = lubridate::ymd(date))
        
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
# yes, 434. maybe these are just missing measurements?
# what i'm concerned about is raw data that just didn't get converted.
sum(!is.na(napinht$raw_height_mm))
# okay, if adjusted height is missing, raw is too
# this is good


# now only select what i want
dat1b <- dat1 %>% 
        select(sample, date, site, station, degrees, pin, height_adj_mm)

# pretty sure what I need to turn into "set_id" is the combination
# "FOMA00", then a 0 or 1 based on the site column, then A/B/C based on 
# the station column
# emailed Shannon and Silas 2/7/2020 and unless I hear otherwise, that's how i'll proceed

dat1b <- dat1b %>% 
        mutate(site = case_when(nchar(site) == 1 ~ paste0("0", site),
                                TRUE ~ site),
               set_id = paste0("FOMA0", site, station),
               reserve = "GTM") %>% 
        select(reserve, set_id, date,
               arm_position = degrees,
               pin_number = pin,
               height_mm = height_adj_mm)


# do similar things with the 2018 sheet
dat2 <- read_excel(path, sheet = "2018") %>% 
        clean_names() %>% 
        mutate(date = lubridate::ymd(date)) 

# adjust pin heights from different sets
pin_offsets <- tribble(
        ~pin_set, ~offset,
        1, 0,
        2, 150,
        3, 390
)

dat2b <- full_join(dat2, pin_offsets, by = "pin_set") %>% 
        mutate(adj_pin_height = pin_height - offset)

dat2c <- dat2b %>% 
        select(reserve, 
               set_id = station,
               date,
               arm_position = direction,   # turn this into "position" when pre-2018 data has 1,3,5,7 arm positions
               pin_number = pin,
               height_mm = adj_pin_height)

dat_all <- bind_rows(dat1b, dat2c)

dat_all <- dat_all %>% 
        mutate(year = year(date),
               month = month(date),
               day = mday(date),
               arm_position = paste0(arm_position, " deg"),
               pin_number = paste0("pin_", pin_number),
               arm_qaqc_code = NA_character_,
               qaqc_code = NA_character_) %>% 
        select(set_id,
               year,
               month,
               day,
               arm_position,
               arm_qaqc_code,
               pin_number,
               height_mm,
               qaqc_code,
               reserve)


write.csv(dat_all, file = here::here("data", "final", "gtmset_processed.csv"),
          row.names = FALSE)
