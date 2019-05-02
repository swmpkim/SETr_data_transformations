# ours was mostly in the correct shape before
# but column names need to be changed, and a column for arm_qaqc_code
# needs to be inserted

# so mostly I just need to read in all the sheets, make some modifications,
# and spit it back out


library(here)
library(tidyverse)
library(XLConnect)
library(readxl)
library(lubridate)

path <- here::here("data", "submitted", "2019-04-04_GND.xlsx")


# get a vector of all the sheets:
sheetnames <- excel_sheets(path)

# make a list for output
dat_list <- list()

# read in each sheet
for(i in seq_along(sheetnames)){
        dat_list[[i]] <- read_xlsx(path, sheet = sheetnames[i])
}

# merge all the individual data frames
dat_wide <- reshape::merge_recurse(dat_list)

# rename the columns
old_names <- names(dat_wide)
new_names <- str_replace(old_names, "_mm", "_height_mm")
new_names <- str_replace(new_names, "_flag", "_qaqc_code")
names(dat_wide) <- new_names

# insert a column for arm_qaqc_code
dat_wide$arm_qaqc_code <- NA_character_

# replace 0s in qaqc_code columns with NA_character_
col_index <- str_which(new_names, "_qaqc_code")
for(i in seq_along(col_index)){
        x <- dat_wide[ , col_index[i]]
        y <- str_replace(x, "0", NA_character_)
        dat_wide[ , col_index[i]] <- y
}


# put columns in order
dat_wide <- dat_wide %>%
        select(reserve, set_id, date, 
               arm_position, arm_qaqc_code, everything())

# split up date into year, month, and day columns
# so excel doesn't make dates all crazy
dat_wide <- dat_wide %>% 
        mutate(year = year(date),
               month = month(date),
               day = mday(date)) %>% 
        select(-date) %>% 
        select(set_id, year, month, day, everything())

# create the output file path
xlpath <- here::here("data", "final", "gndset.xlsx")

# make the file
source(here::here("R", "excel_sheet_script.R"))
