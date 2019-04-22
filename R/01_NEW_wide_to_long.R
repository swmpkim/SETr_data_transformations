library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(lubridate)

# interactively choose file to work with
file_path <- choose.files()


# read in all sheets and append them to each other
dat <- file_path %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map_df( ~ read_excel(path = file_path, sheet = .x), .id = "sheet")

# check to make sure the SET ID that was entered matches the name of each sheet
# we want this to be 0
mismatches <- dat$set_id != dat$sheet


# make something happen if they don't match
if(sum(mismatches) > 0){
        print(paste("row number(s):", which(mismatches == 1)))
        print(dat[mismatches, 1:4])
        stop("There are SET IDs that do not match the sheet name. Please check and correct the rows printed above before proceeding.")
        }


# if no problem, format the data:
# get rid of the "sheet" column
# make sure date is date format; 
# several columns should be character: set_id, arm_position, 
# and anything that ends in qaqc_code
dat_formatted <- dat %>% 
        select(-sheet) %>% 
        mutate(date = lubridate::ymd(date)) %>% 
        mutate_at(c("set_id", "arm_position"), as.character) %>% 
        mutate_at(vars(ends_with("qaqc_code")), as.character)

# have to pivot to longer first, then bust up the column names


        