library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)

# ALMOST THERE
# just need to replace the long-form site names with the shorter codes



# some sites needed to be omitted; they have been removed from the file that's run through this script.

# some color coding in the original submitted file was off. This was corrected on 5/3/19 based on information sent by the reserve's RC, stored in SETr_data_transformations/data/metadata/del_setcode_color_coding_corrections.csv  


##########################
### This reserve has both deep and shallow 'type' associated with each set_id
# DEAL WTIH THIS
# or nothing will work
##########################


#### TASKS  2019-04-02
# Remove Beaver Branch, Isaacs Branch, Wildcat
# Delete duplicates
# If names in data file don’t match metadata names, change data file names




#############################
# Deal with main data frame
#############################

path <- here::here("data", "submitted", "2019-05-03_DEL.xlsx")

# read tabular data
dat <- read_excel(path, sheet = 'SET data') %>%
    select(-c(10:14)) %>%  #these are to the right of the notes column
    clean_names()

# read formats
fill_colors <- xlsx_formats(path)$local$fill$patternFill$fgColor$rgb

# generate a dataframe showing fill colors for each cell
fills <-
    xlsx_cells(path, sheets = "SET data") %>%
    dplyr::filter(row >= 2) %>% # Omit the header row and name column
    mutate(fill_color = fill_colors[local_format_id]) %>%
    select(row, col, fill_color) %>%
    spread(col, fill_color) %>%
    select(-row, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22) %>%
    set_names(paste0(colnames(dat), "_fill"))

# find out how many values there are in each column  
names <- colnames(fills)
sums <- rep(0, length(names))
for(i in seq_along(colnames(fills))){
    col <- fills[,i]
    sums[i] <- sum(!is.na(col))
}
fill_summary <- data.frame(names, sums)

# only need to keep fill columns that don't have 0
fills_exist <- fill_summary$names[fill_summary$sums > 0]
fills <- fills[ , as.character(fills_exist)]

# bind it with the data frame
dat_all <- bind_cols(dat, fills)

#############################
#############################



#############################
# Deal with color key
#############################

codes <- read_excel(path, sheet = 'SET data') %>%
    select(10) %>%
    set_names('code') %>%
    filter(!is.na(code))
    
codefills <- xlsx_cells(path, sheets = "SET data") %>%
    dplyr::filter(row >= 2, row <= 12, col == 10) %>% # Omit the header row and name column
    mutate(fill_color = fill_colors[local_format_id]) %>% 
    select(row, col, fill_color) %>%
    spread(col, fill_color) %>% 
    select("10") %>%   # looked in spreadsheet to make sure this is correct column
    set_names(paste0(colnames(codes), "_fill")) %>%
    filter(!is.na(code_fill))

# removed the Low Transition code because the fill got misread
# to the same hex as Muskrat Transition
# it is DEFINITELY not the same, but there aren't any in the data file
# so I'm deleting it
codes$fill <- codefills$code_fill
codes <- codes %>% 
        filter(code != "Low Transition (LT)")

#############################
#############################



# get rid of some intermediate objects
rm('codefills', 'col', 'fill_colors', 'fills_exist', 'i', 'names', 'sums')




#############################
# Match fills and codes in main dataframe
#############################

# pin_measurement_mm is typically the column that has color coding
# sometimes set_pin_no does, but that may be resolved - so I'm writing this
# in such a way that the code will run with or without it
# using the if(exists) stuff

dat_coded <- left_join(dat_all, codes, by = c('pin_measurement_mm_fill' = 'fill')) %>%
    mutate(pin_code = code,
           code = NULL)  


if(exists('set_pin_no_fill', dat_coded) == TRUE) {
    dat_coded <- left_join(dat_coded, codes, by = c('set_pin_no_fill' = 'fill')) %>%
        mutate(pin_code2 = code,
               code = NULL)
}


dat_coded <- dat_coded %>%
    mutate(code = case_when(
        !is.na(pin_measurement_mm_fill) & !is.na(pin_code) ~ pin_code,
        !is.na(pin_measurement_mm_fill) & is.na(pin_code) ~ 'other, not in key',
        TRUE ~ '0'
    )
    ) %>%
    select(-pin_measurement_mm_fill, -pin_code)



if(exists('set_pin_no_fill', dat_coded) == TRUE) {
    dat_coded <- dat_coded %>%
        mutate(code = case_when(
            !is.na(set_pin_no_fill) & !is.na(pin_code2) ~ pin_code2,
            !is.na(set_pin_no_fill) & is.na(pin_code2) ~ 'other, not in key',
            !is.na(code) & !is.na(pin_code2) ~ paste0(code, pin_code2),
            TRUE ~ code
        )) %>%
        select(-set_pin_no_fill, -pin_code2)
}



# see how many codes are not in key
# hopefully after the original file is looked at, this will be 0
sum(dat_coded$code == 'other, not in key')
# yay!


# change names to be slightly more consistent
# also get rid of any extra rows
# append Deep/Shallow to the set_id
dat_coded <- dat_coded %>%
    filter(is.na(site_label) == FALSE) %>%
    mutate(reserve = 'DEL',
           set_id = paste0(site_label, '_', type)) %>%
    select(reserve, set_id, date, arm_position = set_arm_position, pin_number = set_pin_no, height_mm = pin_measurement_mm, code, notes, latitude, longitude) 

# check for dupes
get_dupes(dat_coded, set_id, date, arm_position, pin_number)

## change those codes to our 3- and 2-letter qaqc codes:
# first make a table
qaqc_code_list <- data.frame(original = unique(dat_coded$code),
                             qaqc_code = c(NA_character_, "CHL", "CHL", "HPD ST",
                               "LSC", "LHE", "CHL", "CHL", "LDP", 
                               "HMD", "LDP MB"))
# view it
qaqc_code_list


# now join them together
dat_long <- left_join(dat_coded, qaqc_code_list, by = c("code" = "original")) %>% 
        mutate(pin_number = paste0("pin_", pin_number),
               arm_qaqc_code = NA_character_) 

#### THE DUPLICATES ####

# pivot to wide
spec <- dat_long %>%
        expand(pin_number, .value = c("height_mm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

dat_wide <- dat_long %>%
        select(-code, -notes) %>% 
        pivot_wider(spec = spec) %>%
        select(set_id, date, arm_position, arm_qaqc_code,
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
dat_wide <- dat_wide %>% 
        mutate(year = year(date),
               month = month(date),
               day = mday(date)) %>% 
        select(-date) %>% 
        select(set_id, year, month, day, everything())

# change the SET IDs into the coded form from DEL's metadata
set_id_table <- data.frame(indata = sort(unique(dat_wide$set_id)),
                           changeto = c("BBLR_deep",
                                        "BBLR_shallow",
                                        "SJBW_deep",
                                        "SJBW_shallow",
                                        "BBDL_deep",
                                        "BBDL_shallow",
                                        "BBEN_deep",
                                        "BBEN_shallow",
                                        "SJIP_deep",
                                        "SJIP_shallow",
                                        "SJRD_deep",
                                        "SJRD_shallow",
                                        "SJTR_deep",
                                        "SJTR_shallow"))

dat_wide <- left_join(dat_wide, set_id_table, by = c("set_id" = "indata")) %>% 
        mutate(long_name = set_id,
               set_id = changeto) %>% 
        select(-changeto)


# export
# create the file path
xlpath <- here::here("data", "final", "delset.xlsx")

# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R"))

