library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)

# read in submitted data and make a few modifications
# using 2019-04-18 spreadsheet because there were duplicate readings
# at two site-date combinations
# they have been fixed in the updated spreadsheet
# see metadata folder, files "APA_dupes_kinda.csv" and 
# "Kim Cressman Duplicate data entry question answers.xlsx"

path <- here::here("data", "submitted", "2019-04-18_APA.xlsx")
dat_raw <- read_xlsx(path) 
dat <- dat_raw %>% 
        clean_names() %>% 
        mutate(date = as.Date(set_date)) %>% 
        select(-set_date)


# pull out QC info
qc_notes <- unique(dat$notes)
qc_context <- dat %>% 
        filter(!is.na(notes)) %>% 
        select(date, marsh_site, set_name, notes)


# pivot to long format and separate out arms and pin numbers
# and rename the set_name column to set_id
dat_long <- dat %>% 
        select(-notes) %>% 
        pivot_longer(cols = north_1:west_9,
                     names_to = "arm_pin",
                     values_to = "height_mm") %>% 
        separate(arm_pin, into = c("arm_position", "pin_number"), sep = "_") %>% 
        mutate(pin_number = paste0("pin_", pin_number),
               arm_qaqc_code = NA_character_,
               qaqc_code = NA_character_) %>% 
        rename(set_id = set_name)


# check for duplicates
row_dupes <- get_dupes(dat, set_name, date)
# write_csv(row_dupes, here::here("data", "metadata", "APA_duplicated_kinda.csv"))
dupes <- get_dupes(dat_long, set_id, date, arm_position, pin_number)



#### code copied from other scripts to get this pivoted back to wide
spec <- dat_long %>%
        expand(pin_number, .value = c("height_mm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

dat_wide <- dat_long %>%
        pivot_wider(spec = spec) %>%
        select(set_id, date, arm_position, arm_qaqc_code, 
               pin_1_height_mm, pin_2_height_mm, pin_3_height_mm, 
               pin_4_height_mm, pin_5_height_mm, pin_6_height_mm, 
               pin_7_height_mm, pin_8_height_mm, pin_9_height_mm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything()) 

dupes <- get_dupes(dat_wide, set_id, date, arm_position)

# split up date into year, month, and day columns
# so excel doesn't make dates all crazy
dat_wide <- dat_wide %>% 
        mutate(year = year(date),
               month = month(date),
               day = mday(date)) %>% 
        select(-date) %>% 
        select(set_id, year, month, day, everything())


# write it back out
# create the file path
xlpath <- here::here("data", "final", "apaset.xlsx")
# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R")) 
