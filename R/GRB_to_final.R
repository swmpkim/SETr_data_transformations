library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)

# after running this script - manually entered the following qaqc codes
# based on the original file and communication with the RC at GRB
# GBF 3, 2013-07-22, arm 4, pin 4: LHE
# SPD1, 2011-08-11, arm 3 arm_qaqc_code: CMO
# SPD1, 2013-10-16, arm 1 pin 7; arm 3 pins 7 and 8: LHE
# SPD1, 2014-04-17, arm 1 arm_qaqc_code: HPD WR
# SPD1, 2014-12-14, arm 3, pins 7 and 8: LHE
# SPD1, 2016-10-05, arm 1, pin 4: LHE
# SPD1, 2017-07-27, arm 1, pin 7: LHE CUW CUS
# SPU1, 2017-12-05, arm 4, arm_qaqc_code: CHL (this is arm bearing 240; was mistyped in original file as 230; also originally only labeled for pin 9 but given local knowledge, RC opted to flag whole arm)
# SPU2, 2011-08-11, arm 4, pin 4: HSM SK
# SPU2, 2013-10-16, arm 3, pin 9: LHE
# SPU2, 2014-04-17, arms 2, 3, and 4 arm_qaqc_code: HPD WR
# SPU2, 2017-12-05, arm 4, pin 4: HSM SK


path <- here::here("data", "submitted", "2018-12-27_GRB.xlsx")

# generate vector for column names
arm_pos <- c("arm_1", "arm_2", "arm_3", "arm_4")
# using pin_0 for arm position column, to keep things in order
pin_nums <- paste0("pin_", c(0:9))
arm_pins <- expand.grid(arm_pos, pin_nums) %>% 
        arrange(Var1, Var2)
arm_pin_names <- paste0(arm_pins$Var1, ".", arm_pins$Var2)
col_names <- c("site", "set_id", "date", "yrs_since_1990", arm_pin_names)


# read in data, format date, change name of pin_0 column
### WOW, THE DATES. They got all messed up originally but
# using "mac pre-2011" for the date_system seemed to fix it
dat <- read_excel(path, sheet = "SET MH Database", range = "A3:AR129",
                 col_names = col_names) %>% 
        mutate(date = excel_numeric_to_date(as.numeric(date), date_system = "mac pre-2011"))
names(dat) <- str_replace(names(dat), "pin_0", "position")


# there are a lot of rows that are empty except for "structural" columns, e.g.
# arm position information. get rid of those.
# criterion is, sum(is.na) in the row less than 36 to keep
dat_trimmed <- dat %>% 
        mutate(count_na = rowSums(is.na(.))) %>% 
        filter(count_na < 36,
               !is.na(date)) %>% 
        select(-count_na)


# first check for dupes
get_dupes(dat_trimmed, set_id, date)
# all good


# pivot to long format
# create some qaqc_code columns
dat_long <- dat_trimmed %>% 
        select(set_id:arm_1.position, 
               arm_2.position, arm_3.position, arm_4.position,
               everything()) %>% 
        pivot_longer(cols = arm_1.pin_1:arm_4.pin_9,
                     names_to = "arm_pin",
                     values_to = "height_mm") %>% 
        separate(arm_pin, into = c("arm_position", "pin_number"), sep = "\\.") %>% 
        mutate(arm_qaqc_code = NA_character_,
               qaqc_code = NA_character_)




# pivot to wide format and transfer the values for arm bearings
spec <- dat_long %>%
        expand(pin_number, .value = c("height_mm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

dat_wide <- dat_long %>%
        pivot_wider(spec = spec) %>%
        mutate(arm_position_bearing = 
                       case_when(arm_position == "arm_1" ~ arm_1.position,
                                 arm_position == "arm_2" ~ arm_2.position,
                                 arm_position == "arm_3" ~ arm_3.position,
                                 arm_position == "arm_4" ~ arm_4.position)) %>% 
        select(set_id, date, arm_position, arm_position_bearing, arm_qaqc_code, 
               pin_1_height_mm, pin_2_height_mm, pin_3_height_mm, 
               pin_4_height_mm, pin_5_height_mm, pin_6_height_mm, 
               pin_7_height_mm, pin_8_height_mm, pin_9_height_mm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything()) %>% 
        select(-ends_with(".position"))


# check for inadvertent dupes
dupes <- get_dupes(dat_wide, set_id, date, arm_position)


# split up date into year, month, and day columns
# so excel doesn't make dates all crazy
dat_wide <- dat_wide %>% 
        mutate(year = year(date),
               month = month(date),
               day = mday(date)) %>% 
        select(-date) %>% 
        select(set_id, year, month, day, everything())


# create the file path
xlpath <- here::here("data", "final", "grbset.xlsx")

# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R"))        

