library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(lubridate)

### this is pretty simple:
# read in the range for a given site
# split out the arm and pin columns
# use tidyr::fill to fill down set_id and arm

#################################################################
### still need to append reader on the given dates; maybe after it goes back to wide format i can do a lookup table or something
#################################################################


# main spreadsheet

path <- here::here("data", "submitted", "2019-04-04_ELK.xlsx")



# function to wrangle that data
# put the sheet and range in quotes like you would inside read_excel()

clean_sheet <- function(sheet, range){

        dat <- read_excel(path, sheet = sheet, range = range) %>%
                clean_names()
        names(dat)[1:2] <- c("set_id", "arm_pin")
        
        
        dat_filled <- dat %>%
                mutate(pin_number = substr(arm_pin, nchar(arm_pin), nchar(arm_pin)),
                       arm_position = case_when(nchar(arm_pin) > 1 ~ substr(arm_pin, 1, 1),
                                                TRUE ~ NA_character_)) %>%
                fill(arm_position, .direction = "down") %>%
                fill(set_id, .direction = "down") %>%
                select(-arm_pin) %>%
                select(set_id, arm_position, pin_number, everything())
        
        
        ### pivot to longer; format date:
        dat_long <- dat_filled %>%
                pivot_longer(
                        cols = starts_with("x"),
                        names_to = "date",
                        names_prefix = "x",
                        values_to = "height_mm"
                ) 
        # removed date formatting step due to issues in big creek and azevedo
        dat_long
}

# wrangle each site
rubis <- clean_sheet(sheet = "Rubis SET Data", range = "A22:P94") %>%
        mutate(date = lubridate::mdy(date))
round_hill <- clean_sheet("Round Hill Set Data", range = "A22:P94") %>%
        mutate(date = lubridate::mdy(date))

big_creek <- clean_sheet("Big Creek SET Data", range = "A23:P95")
azevedo <- clean_sheet("Azevedo SET Data", range = "A22:P94")



## big_creek and azevedo need some date cleanup
big_creek <- big_creek %>%
        mutate(date2 = case_when(is.na(str_match(date, "_")) ~ 
                                        janitor::excel_numeric_to_date(as.numeric(date)),
                                TRUE ~ lubridate::mdy(date))) %>%
        select(-date) %>%
        select(set_id, date = date2, arm_position, pin_number, height_mm)


azevedo <- azevedo %>%
        mutate(date2 = case_when(is.na(str_match(date, "_")) ~ 
                                         janitor::excel_numeric_to_date(as.numeric(date)),
                                 TRUE ~ lubridate::mdy(date))) %>%
        select(-date) %>%
        select(set_id, date = date2, arm_position, pin_number, height_mm)



# bind them all together
dat_all <- bind_rows(rubis, round_hill) %>%
        bind_rows(., big_creek) %>%
        bind_rows(., azevedo) %>%
        select(set_id, date, everything())


# write out intermediate file
out_path <- here::here("data", "intermediate_long", "ELK_intermed.csv")
write_csv(dat_all, out_path)
