library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)

## CBM
# all in one sheet
# in long format

path <- here::here("data", "submitted", "2018-11-13_CBM.xlsx")

dat_raw <- read_xlsx(path, sheet = "Raw SET Data", range = "A26:I5498", 
                 na = c("", "."))

dat <- dat_raw %>%
        clean_names() %>%
        remove_empty("rows") %>%
        mutate(pin = paste0("pin_", pin),
               date = as.Date(date)) %>%
        select(set_id = set_name, date, arm_position,
               pin_number = pin, height_cm = pin_height_cm,
               everything())

# pull out the ones that will need QC codes
qc_needs <- dat %>%
        filter(!is.na(observations)) %>%
        select(c("set_id":"height_cm"), "observations") %>%
        arrange(set_id, date, arm_position, pin_number)

# write to files if desired

# intermedpath1 <- here::here("data", "intermediate_long", "cbm_qc_obs_detailed.csv")
# intermedpath2 <- here::here("data", "intermediate_long", "cbm_qc_obs_unique.csv")
# qc_list <- unique(qc_needs$observations)
# write_csv(qc_needs, intermedpath1)
# write_csv(data.frame(qc_list), intermedpath2)


dat_backup <- dat


# prepare for pivoting
# set up specs for pivoting
spec <- dat %>%
        expand(pin_number, .value = c("height_cm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

dat_wide <- dat %>%
        mutate(qaqc_code = NA_character_,
               arm_qaqc_code = NA_character_) %>%
        select(-observations) %>%
        pivot_wider(spec = spec) %>%
        select(set_id, date, arm_position, arm_qaqc_code,
               pin_1_height_cm, pin_2_height_cm, pin_3_height_cm, 
               pin_4_height_cm, pin_5_height_cm, pin_6_height_cm, 
               pin_7_height_cm, pin_8_height_cm, pin_9_height_cm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything()) 

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


###########################################################################
# write to excel
###########################################################################

# create the file path
xlpath <- here::here("data", "final", "cbmset.xlsx")

# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R"))
