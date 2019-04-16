library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)

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
dat <- read_xlsx(path, sheet = "SET MH Database", range = "A3:AR129",
                 col_names = col_names) %>% 
        mutate(date = excel_numeric_to_date(as.numeric(date)))
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


# create the file path
xlpath <- here::here("data", "final", "grbset.xlsx")

# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R"))        

