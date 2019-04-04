### this script is developed using the dev version of tidyr
# version 0.8.3.9000
# so I can use the new pivot_wider() function

library(tidyverse)
library(here)
library(janitor)  # to check for dupes
library(XLConnect)

# read in data ----
path <- here::here("data", "intermediate_long", "WQB.csv")
dat <- read.csv(path, stringsAsFactors = FALSE)


# format columns
dat <- dat %>%
        mutate(set_id = as.character(set_id),
               date = as.Date(date),
               pin_number = paste0("pin_", as.character(pin_number)),
               arm_position = as.character(arm_position))



# pivot out pin heights and qaqc codes

################################################
# lots of code testing here for future reference ----
################################################

# test on just a subset
dat_wide <- dat %>%
        select(reserve:front_back) %>%
        pivot_wider(names_from = pin_number,
                    values_from = c(pin_height_cm, qaqc_code))

# specify the names - i want pin number at the front, rather than at the end,
# of the new column names

# much borrowing from this article:
# https://tidyr.tidyverse.org/dev/articles/pivot.html#wider-1

# first see what the function is doing
dat %>%
        pivot_wider_spec(names_from = pin_number, values_from = c(pin_height_cm, qaqc_code))

# now modify
spec <- dat %>%
        expand(pin_number, .value = c("pin_height_cm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, ifelse(.value == "pin_height_cm", "_height_cm", "_qaqc_code")))

# and test this on a subset
dat_wide <- dat %>%
        select(reserve:front_back) %>%
        pivot_wider(spec = spec) %>%
        select(reserve, set_id, date, arm_position, 
               pin_1_height_cm, pin_2_height_cm, pin_3_height_cm, 
               pin_4_height_cm, pin_5_height_cm, pin_6_height_cm, 
               pin_7_height_cm, pin_8_height_cm, pin_9_height_cm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything())

######################################################
# testing over
######################################################


# pivot ----

# spec <- dat %>%
#         expand(pin_number, .value = c("pin_height_cm", "qaqc_code")) %>%
#         mutate(.name = paste0(pin_number, 
#                               ifelse(.value == "pin_height_cm", 
#                                      "_height_cm", "_qaqc_code")))

spec_more <- dat %>%
        expand(pin_number, .value = c("pin_height_cm", "qaqc_code",
                                      "pin_length")) %>%
        mutate(.name = paste0(pin_number, "_", 
                              case_when(.value == "pin_height_cm" ~ "height_cm",
                                        .value == "qaqc_code" ~ "qaqc_code",
                                        TRUE ~ .value)))

# get rid of calculated columns that need a value for each pin,
# then pivot
dat_wide <- dat %>%
        select(-c(pin_length_pin_meas:marsh_set_elevation_navd88_in_m),
               -pin_length_pin_meas_cm,
               -notes,
               -c(spal_nearby:aster_nearby)) %>%
        pivot_wider(spec = spec_more) %>%
        select(reserve, set_id, date, arm_position, 
               time_readings_started, reader,
               pin_1_height_cm, pin_2_height_cm, pin_3_height_cm, 
               pin_4_height_cm, pin_5_height_cm, pin_6_height_cm, 
               pin_7_height_cm, pin_8_height_cm, pin_9_height_cm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything())


# check for inadvertent duplicates
dupes <- get_dupes(dat_wide, set_id, date, arm_position)

# phew! got rid of them all!


# add a column for arm-level qaqc codes
dat_wide <- dat_wide %>%
        mutate(arm_qaqc_code = NA_character_) %>%
        select(reserve, set_id, date, 
               arm_position, arm_qaqc_code,
               everything())



# let's see what got cut out:
dat_trimmed <- dat %>%
        select(c(pin_length_pin_meas:marsh_set_elevation_navd88_in_m),
               pin_length_pin_meas_cm,
               notes,
               c(spal_nearby:aster_nearby)) %>%
        names()


# intermediate file-writing if desired:

# out_path <- here("data", "intermediate_wide", "WQB_wider.csv")
# write_csv(dat_wide, out_path, na = "")


## split up and format for excel ----

# turn date into a character string because otherwise it saves crazily
dat_wide$date <- as.character(dat_wide$date)

# create the file path
xlpath <- here("data", "final", "wqbset.xlsx")

# make the file
source(here::here("R", "excel_sheet_script.R"))
