# in this file, there are certain patterns
# Bearing - 9 - 8 - ... - 1 - Center - 1 - 2 - ... - 9 - Bearing
# times 4

# the following columns and patterns are used in both excel sheets that contain data

# columns A:G and CR:CS  apply to all others

# Bearing on left:   H:Q;  AD:AM;  AZ:BI;  BV:CE

# Bearing on right: S:AB;  AO:AX;  BK:BT;  CG:CP

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(hms)
library(here)


path <- here::here("data", "submitted", "2019-04-30_WEL.xlsx")


# come up with the pattern of column names
names_arm1 <- paste0("arm1_", c("bearing", paste0("pin", seq(9, 1))))
names_arm2 <- paste0("arm2_", c(paste0("pin", seq(1, 9)), "bearing"))

names_arm3 <- paste0("arm3_", c("bearing", paste0("pin", seq(9, 1))))
names_arm4 <- paste0("arm4_", c(paste0("pin", seq(1, 9)), "bearing"))

names_arm5 <- paste0("arm5_", c("bearing", paste0("pin", seq(9, 1))))
names_arm6 <- paste0("arm6_", c(paste0("pin", seq(1, 9)), "bearing"))

names_arm7 <- paste0("arm7_", c("bearing", paste0("pin", seq(9, 1))))
names_arm8 <- paste0("arm8_", c(paste0("pin", seq(1, 9)), "bearing"))



##########################
# LR21
##########################

# read in just the data range (skip the first few header rows)
# get rid of empty columns ("center", and skipped columns between arms)
lr21 <- read_excel(path, sheet = "LR21", range = "A3:CS11") %>% 
        clean_names() %>% 
        remove_empty("cols")


# rename the columns
names(lr21)[8:87] <- c(names_arm1, names_arm2, names_arm3, names_arm4, 
                       names_arm5, names_arm6, names_arm7, names_arm8)


# pivot down the pin readings

# set up the names of columns that need to pivot
arms <- paste0("arm", seq(1, 8))
pins <- paste0("pin", seq(1, 9))
arms_pins <- expand_grid(arms, pins) %>% 
        mutate(combo = paste0(arms, "_", pins))

# and pivot
dat_long <- lr21 %>% 
        pivot_longer(cols = arms_pins$combo,
                     names_to = "arm_pin",
                     values_to = "height_mm")


# do some more cleanup
# split out arm and pin positions
# keep only the bearing for whichever arm is in use
# insert qaqc code columns
dat_long <- dat_long %>% 
        separate(arm_pin, into = c("arm_position", "pin_number")) %>% 
        mutate(arm_bearing_all = case_when(arm_position == "arm1" ~ arm1_bearing,
                                       arm_position == "arm2" ~ arm2_bearing,
                                       arm_position == "arm3" ~ arm3_bearing,
                                       arm_position == "arm4" ~ arm4_bearing,
                                       arm_position == "arm5" ~ arm5_bearing,
                                       arm_position == "arm6" ~ arm6_bearing,
                                       arm_position == "arm7" ~ arm7_bearing,
                                       arm_position == "arm8" ~ arm8_bearing),
               qaqc_code = NA_character_,
               arm_qaqc_code = NA_character_) %>% 
        select(-c(ends_with("bearing"))) %>% 
        rename(arm_bearing = arm_bearing_all)

# put underscores back into pins and arms
dat_long$pin_number <- gsub("pin", "pin_", dat_long$pin_number)
dat_long$arm_position <- gsub("arm", "arm_", dat_long$arm_position)


# pivot to wide format and transfer the values for arm bearings
spec <- dat_long %>%
        expand(pin_number, .value = c("height_mm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

dat_wide <- dat_long %>%
        pivot_wider(spec = spec) %>%
        select(set_id = location, date, time, low_tide, 
               recorder = recorder_s, bar_height, vertical_offset, 
               arm_position, arm_bearing, arm_qaqc_code, 
               pin_1_height_mm, pin_2_height_mm, pin_3_height_mm, 
               pin_4_height_mm, pin_5_height_mm, pin_6_height_mm, 
               pin_7_height_mm, pin_8_height_mm, pin_9_height_mm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything())

# check for inadvertent dupes
dupes <- get_dupes(dat_wide, set_id, date, arm_position)


# split up date into year, month, and day columns
# so excel doesn't make dates all crazy
# clean up times
dat_wide <- dat_wide %>% 
        select(-year) %>% 
        mutate(year = year(date),
               month = month(date),
               day = mday(date),
               time = as.character(as.hms(time, tz = "UTC")),
               low_tide = as.character(as.hms(low_tide, tz = "UTC"))) %>% 
        select(-date) %>% 
        select(set_id, year, month, day, everything())
# get rid of the trailing :00 on times:
dat_wide$time <- substr(dat_wide$time, 1, nchar(dat_wide$time) - 3)
dat_wide$low_tide <- substr(dat_wide$low_tide, 1, nchar(dat_wide$low_tide) - 3)


# save to its own data frame
lr21_wide <- dat_wide




# repeat on other sheet

##########################
# WR8
##########################

# read in just the data range (skip the first few header rows)
# get rid of empty columns ("center", and skipped columns between arms)
wr8 <- read_excel(path, sheet = "WR8", range = "A3:CS10") %>% 
        clean_names() %>% 
        remove_empty("cols")


# rename the columns
names(wr8)[8:87] <- c(names_arm1, names_arm2, names_arm3, names_arm4, 
                       names_arm5, names_arm6, names_arm7, names_arm8)


# pivot down the pin readings

# set up the names of columns that need to pivot
arms <- paste0("arm", seq(1, 8))
pins <- paste0("pin", seq(1, 9))
arms_pins <- expand_grid(arms, pins) %>% 
        mutate(combo = paste0(arms, "_", pins))

# and pivot
dat_long <- wr8 %>% 
        pivot_longer(cols = arms_pins$combo,
                     names_to = "arm_pin",
                     values_to = "height_mm")


# do some more cleanup
# split out arm and pin positions
# keep only the bearing for whichever arm is in use
# insert qaqc code columns
dat_long <- dat_long %>% 
        separate(arm_pin, into = c("arm_position", "pin_number")) %>% 
        mutate(arm_bearing_all = case_when(arm_position == "arm1" ~ arm1_bearing,
                                           arm_position == "arm2" ~ arm2_bearing,
                                           arm_position == "arm3" ~ arm3_bearing,
                                           arm_position == "arm4" ~ arm4_bearing,
                                           arm_position == "arm5" ~ arm5_bearing,
                                           arm_position == "arm6" ~ arm6_bearing,
                                           arm_position == "arm7" ~ arm7_bearing,
                                           arm_position == "arm8" ~ arm8_bearing),
               qaqc_code = NA_character_,
               arm_qaqc_code = NA_character_) %>% 
        select(-c(ends_with("bearing"))) %>% 
        rename(arm_bearing = arm_bearing_all)

# put underscores back into pins and arms
dat_long$pin_number <- gsub("pin", "pin_", dat_long$pin_number)
dat_long$arm_position <- gsub("arm", "arm_", dat_long$arm_position)


# pivot to wide format and transfer the values for arm bearings
spec <- dat_long %>%
        expand(pin_number, .value = c("height_mm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

dat_wide <- dat_long %>%
        pivot_wider(spec = spec) %>%
        select(set_id = location, date, time, low_tide, 
               recorder = recorder_s, bar_height, vertical_offset, 
               arm_position, arm_bearing, arm_qaqc_code, 
               pin_1_height_mm, pin_2_height_mm, pin_3_height_mm, 
               pin_4_height_mm, pin_5_height_mm, pin_6_height_mm, 
               pin_7_height_mm, pin_8_height_mm, pin_9_height_mm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything())

# check for inadvertent dupes
dupes <- get_dupes(dat_wide, set_id, date, arm_position)


# split up date into year, month, and day columns
# so excel doesn't make dates all crazy
# clean up times
dat_wide <- dat_wide %>% 
        select(-year) %>% 
        mutate(year = year(date),
               month = month(date),
               day = mday(date),
               time = as.character(as.hms(time, tz = "UTC")),
               low_tide = as.character(as.hms(low_tide, tz = "UTC"))) %>% 
        select(-date) %>% 
        select(set_id, year, month, day, everything())
# get rid of the trailing :00 on times:
dat_wide$time <- substr(dat_wide$time, 1, nchar(dat_wide$time) - 3)
dat_wide$low_tide <- substr(dat_wide$low_tide, 1, nchar(dat_wide$low_tide) - 3)


# save to its own data frame
wr8_wide <- dat_wide


###################################
# join dfs together and generate excel sheet
###################################

dat_wide <- bind_rows(lr21_wide, wr8_wide)


# export
# create the file path
xlpath <- here::here("data", "final", "welset.xlsx")

# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R"))
