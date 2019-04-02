library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)

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

path <- 'data/raw_original/DEL.xlsx'

# read tabular data
dat <- read_excel(path, sheet = 'SET data') %>%
    select(-X__1, -X__2, -X__3, -X__4, -X__5) %>%  #these are to the right of the notes column
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
    select(X__1) %>%
    set_names('code') %>%
    filter(!is.na(code))
    
codefills <- xlsx_cells(path, sheets = "SET data") %>%
    dplyr::filter(row >= 2) %>% # Omit the header row and name column
    mutate(fill_color = fill_colors[local_format_id]) %>%
    select(row, col, fill_color) %>%
    spread(col, fill_color) %>%
    select(11) %>%   # looked in spreadsheet to make sure this is correct column
    set_names(paste0(colnames(codes), "_fill")) %>%
    filter(!is.na(code_fill))

codes$fill <- codefills$code_fill

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


# change names to be slightly more consistent
# also get rid of any extra rows
# append Deep/Shallow to the set_id
dat_coded <- dat_coded %>%
    filter(is.na(site_label) == FALSE) %>%
    mutate(reserve = 'DEL',
           set_id = paste0(site_label, '_', type)) %>%
    select(reserve, set_id, date, arm_position = set_arm_position, pin_number = set_pin_no, pin_height_mm = pin_measurement_mm, code, notes, latitude, longitude) 

# spit out csv file
write_csv(dat_coded, 'data/intermediate/DEL.csv')


# neg_pin_hts <- dat_coded %>% filter(pin_height < 0)
# write_csv(neg_pin_hts, 'data/needs_attention/DEL_neg_pin_heights.csv')
