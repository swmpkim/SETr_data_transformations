# this file is simple enough that one script should get the data all the way to the excel output


library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)
library(XLConnect)


path <- here::here('data', 'submitted', '2018-10-10_SOS.xlsx')


dat <- read_excel(path, sheet = 'RSET data') %>%
        clean_names() %>%
        filter(!is.na(set_id))
names(dat) <- c("reserve", "set_id", "arm_position", "date", 
                "pin_1_height_mm", "pin_2_height_mm", "pin_3_height_mm",
                "pin_4_height_mm", "pin_5_height_mm", "pin_6_height_mm",
                "pin_7_height_mm", "pin_8_height_mm", "pin_9_height_mm",
                "pin_1_qaqc_code", "pin_2_qaqc_code", "pin_3_qaqc_code",
                "pin_4_qaqc_code", "pin_5_qaqc_code", "pin_6_qaqc_code",
                "pin_7_qaqc_code", "pin_8_qaqc_code", "pin_9_qaqc_code",
                "notes")

# replace numeric codes with letter codes:
# 1 to CUS - comment - uncertainty about position of surface
# 2 to HPD - high - plant dead (e.g. hummock)
# 3 to CLY - comment - measured less than one year after installation
# make a function
qaqc_replace <- function(x){
        temp_vec <- x
        temp_vec[temp_vec == "1"] <- "CUS"
        temp_vec[temp_vec == "2"] <- "HPD"
        temp_vec[temp_vec == "3"] <- "CLY"
        temp_vec[temp_vec == "3,2"] <- "HPD CLY"
        temp_vec
}

# apply that function to all the qaqc_code columns
code_index <- grep("qaqc_code", names(dat))
for(i in seq_along(code_index)){
        col_index <- code_index[i]
        dat[[col_index]] <- qaqc_replace(dat[[col_index]])
}

grep("not recorded", dat$notes)
glimpse(dat[9:12, ])

# i think that qa/qc issue has been dealt with by now so i will ignore it....
dat$notes <- NULL



# for ease of getting into excel based on existing code
dat_wide <- dat

# write to excel:

# turn date into a character string because otherwise it saves crazily
dat_wide$date <- as.character(dat_wide$date)
dat_wide$arm_position <- as.character(dat_wide$arm_position)

# add a column for arm-level qaqc codes
dat_wide <- dat_wide %>%
        mutate(arm_qaqc_code = NA_character_) %>%
        select(reserve, set_id, date, 
               arm_position, arm_qaqc_code,
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

# create the file path
xlpath <- here::here("data", "final", "sosset.xlsx")

# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R"))
