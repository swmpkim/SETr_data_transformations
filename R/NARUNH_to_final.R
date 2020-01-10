library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)

path <- here::here("data", "submitted", "2019-04-17_NARUNH.xls")

dat <- read_excel(path) %>% 
    janitor::clean_names()

# sample dates, per emailing with Kenny Raposa, 10/15/2019:
# 2014: 6/12
# 2015: 6/16
# 2016: 5/20
# 2017: 5/14
# 2018: 6/11
# 2019: 5/15
# for all other years, no date available; use 6/1
samp_dates <- tribble(
    ~year, ~month, ~day,
    2014, 06, 12,
    2015, 06, 16,
    2016, 05, 20,
    2017, 05, 14,
    2018, 06, 11,
    2019, 05, 15
)


# see what's in this file
dat %>% 
    select(1:5) %>% 
    distinct()


# change some names
dat2 <- dat %>% 
    rename(arm_position = corner) %>% 
    mutate(reserve = "NARUNH")


# to long; do some cleanup and add dates
dat_long <- dat2 %>% 
    pivot_longer(ends_with("cm"), 
                 names_to = "col_id",
                 values_to = "pin_height_cm") %>% 
    separate(col_id, into = c("year", "units"), sep = "_") %>% 
    mutate(year = as.numeric(str_remove(year, "x")),
           pin_values = paste0("pin_", pin_number, "_height_cm")) %>% 
    select(-units, -pin_number)


# add in month and day columns based on the data frame above
dat_long2 <- left_join(dat_long, samp_dates, by = "year") %>% 
    mutate(month = case_when(is.na(month) ~ 6,
                             TRUE ~ month),
           day = case_when(is.na(day) ~ 1,
                           TRUE ~ day))

# back to wide
# add in necessary qaqc_code columns
dat_wide <- dat_long2 %>% 
    pivot_wider(names_from = pin_values,
                values_from = pin_height_cm) %>%
    mutate(arm_qaqc_code = NA_character_,
           pin_1_qaqc_code = NA_character_,
           pin_2_qaqc_code = NA_character_,
           pin_3_qaqc_code = NA_character_,
           pin_4_qaqc_code = NA_character_,
           pin_5_qaqc_code = NA_character_,
           pin_6_qaqc_code = NA_character_,
           pin_7_qaqc_code = NA_character_,
           pin_8_qaqc_code = NA_character_,
           pin_9_qaqc_code = NA_character_) %>% 
    select(reserve, set_id, 
           year, month, day,
           arm_position, arm_qaqc_code,
           ends_with("cm"),
           ends_with("qaqc_code"),
           everything())



# check for inadvertent dupes
dupes <- get_dupes(dat_wide, set_id, year, month, day, arm_position)


# export
# create the file path
xlpath <- here::here("data", "final", "narunhset.xlsx")

# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R"))
