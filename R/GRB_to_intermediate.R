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
arm_pin_names <- paste0(arm_pins$Var1, "_", arm_pins$Var2)
col_names <- c("site", "set_id", "date", "yrs_since_1990", arm_pin_names)


# read in data, format date, change name of pin_0 column
dat <- read_xlsx(path, sheet = "SET MH Database", range = "A3:AR129",
                 col_names = col_names) %>% 
        mutate(date = excel_numeric_to_date(as.numeric(date)))
names(dat) <- str_replace(names(dat), "pin_0", "position")


# there are a lot of rows that are empty except for "structural" columns, e.g.
# arm position information. get rid of those.
# criterion is, sum(is.na) in the row less than 36 to keep
sum(!is.na())
dat_trimmed <- dat %>% 
        mutate(count_na = rowSums(is.na(.))) %>% 
        filter(count_na < 36,
               !is.na(date)) %>% 
        select(-count_na)


# first check for dupes
get_dupes(dat_trimmed, set_id, date)
# all good


# pivot to long format
dat_long <- dat_trimmed %>% 
        select(set_id:arm_1_position, 
               arm_2_position, arm_3_position, arm_4_position,
               everything()) %>% 
        pivot_longer(cols = arm_1_pin_1:arm_4_pin_9,
                     names_to = "arm_pin",
                     values_to = "value")
