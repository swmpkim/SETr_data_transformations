library(tidyverse)
library(readxl)
library(hms)
library(janitor)
library(lubridate)

# data in long format
# all SETs in one worksheet
# empty rows for future data entry; these will be removed

path <- here::here("data", "submitted", "2019-05-07_PDBnewsets.xlsx")

# read in data
dat <- read_excel(path, na = "n/a") %>% 
        clean_names()%>% 
        mutate(time = as.character(as.hms(time, tz = "UTC"))) %>%
        filter(!is.na(date))

# make table of qaqc codes to be associated with original notes in file
probs_table <- data.frame(original = sort(unique(dat$problem_description)),
                          qaqc_code = c("LDP HP",
                                        "LPS",
                                        "LPS",
                                        "LDP SR",
                                        "HPD ST",
                                        "HMD CH",
                                        "HMD CH",
                                        "HMD CH"))

# add those qaqc codes to the original data frame
# remove some of the extra columns
dat2 <- left_join(dat, probs_table, by = c("problem_description" = "original")) %>% 
        select(-c(x12:x17, counter, problem_description)) %>%
        mutate(arm_qaqc_code = NA_character_) %>% 
        select(reserve,
               set_id = set_identifier,
               arm_position = position,
               arm_qaqc_code,
               pin_number = pin,
               date,
               time,
               height_mm = pin_height,
               qaqc_code,
               notes = additonal_notes) %>% 
        mutate(pin_number = paste0("pin_", pin_number),
               arm_position = paste0("arm_", arm_position))

# make a data frame with add'l notes, because i'm going to get rid of them to pivot
keep_notes <- dat2 %>% 
        filter(!is.na(notes))

# and get rid of notes
dat_trimmed <- dat2 %>% 
        select(-notes)


# pivot
spec <- dat_trimmed %>%
        expand(pin_number, .value = c("height_mm", "qaqc_code", "time")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

# pivoting time out too, because often it was recorded only for the first pin in a group
# so will pull that into a "time" field and then remove all the extra columns

dat_wide <- dat_trimmed %>%
        pivot_wider(spec = spec) %>%
        select(set_id, date, arm_position, arm_qaqc_code,
               pin_1_height_mm, pin_2_height_mm, pin_3_height_mm, 
               pin_4_height_mm, pin_5_height_mm, pin_6_height_mm, 
               pin_7_height_mm, pin_8_height_mm, pin_9_height_mm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything()) %>% 
        mutate(time = case_when(!is.na(pin_1_time) ~ pin_1_time,
                                TRUE ~ NA_character_)) %>% 
        select(-c(pin_1_time:pin_9_time))

(dupes <- get_dupes(dat_wide, set_id, date, arm_position))
# good!

# get rid of the trailing :00 on times:
dat_wide$time <- substr(dat_wide$time, 1, nchar(dat_wide$time) - 3)


# split up date into year, month, and day columns
# so excel doesn't make dates all crazy
dat_wide <- dat_wide %>% 
        mutate(year = year(date),
               month = month(date),
               day = mday(date)) %>% 
        select(-date) %>% 
        select(set_id, year, month, day, everything())



# export
# create the file path
xlpath <- here::here("data", "final", "pdb_newsets.xlsx")

# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R"))

