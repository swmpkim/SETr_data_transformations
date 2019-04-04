library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(lubridate)

### this is pretty simple:
# read in the range for a given site
# split out the arm and pin columns
# use tidyr::fill to fill down set_id and arm

### still need to append reader on the given dates; maybe after it goes back to wide format i can do a lookup table or something

path <- here("data", "submitted", "2019-04-04_ELK.xlsx")

# working with Rubis first
test <- read_excel(path, sheet = "Rubis SET Data", range = "A22:P94") 

dat <- test %>% clean_names()
names(dat)[1:2] <- c("set_id", "arm_pin")

# pin_number = last character of the arm_pin column
# arm_position is first character of arm_pin column when arm_pin has more than two characters; NA otherwise
# then arm_position gets filled down
dat_filled <- dat %>%
        mutate(pin_number = substr(arm_pin, nchar(arm_pin), nchar(arm_pin)),
               arm_position = case_when(nchar(arm_pin) > 1 ~ substr(arm_pin, 1, 1),
                                        TRUE ~ NA_character_)) %>%
        fill(arm_position, .direction = "down") %>%
        fill(set_id, .direction = "down") %>%
        select(-arm_pin) %>%
        select(set_id, arm_position, pin_number, everything())

# pivot so dates are no longer column names
# and mutate to turn them into actual dates
dat_long <- dat_filled %>%
        pivot_longer(-c(set_id, arm_position, pin_number), names_to = "date", values_to = "height_mm") %>%
        mutate(date = substr(date, 2, nchar(date)),
               date = mdy(date))




