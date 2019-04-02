library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)


#############################
# Deal with main data frame
#############################

path <- here::here('data', 'submitted', '2019-03-27_WQB.xlsx')


## QA/QC codes have been added into the data file
## it looks like all columns have been named
## there are still some rows where there is a comment instead of readings,
# so those will need to be removed through this script





# read tabular data

# every sheet (named by years) will be an element of a list
# set it up
years <- 2013:2018
dat <- list()


# read in the sheets
for(i in seq_along(years)){
    dat[[i]] <- read_excel(path, sheet = as.character(years[i])) %>%
        janitor::clean_names()
}

# name the elements of the list
names(dat) <- paste0('dat', years)



###############################
## Clean up 2013
###############################

# replace (with "NA") the comment in the date column about 1020 not being read
dat$dat2013$date <- gsub("1020", NA, dat$dat2013$date)  

# get rid of column 15 and the last two columns; they're blank and/or metadata
dat$dat2013[c(15, 19, 20)] <- NULL

# first two rows are mostly blank too
dat$dat2013[1:2, ] <- NA


#### THANK YOU tidyverse community for help with assigning an NA
# in case_when using NA_character_
# https://github.com/tidyverse/dplyr/issues/3202#issuecomment-343601409

dat$dat2013 <- dat$dat2013 %>%
        rename(qc1 = primary_qa_qc_code,
               qc2 = secondary_qa_qc_code,
               front_back = f_b) %>%
        mutate(date = excel_numeric_to_date(as.numeric(dat$dat2013$date)),
               pin_height_cm = as.numeric(x2013_measured_pin_height_cm),
               qaqc_code = case_when( !is.na(qc1) & is.na(qc2) ~ qc1,
                                      !is.na(qc1) & !is.na(qc2) ~ paste(qc1, qc2),
                                      is.na(qc1) & !is.na(qc2) ~ qc2,
                                      TRUE ~ NA_character_)) %>%
        select(-qc1, -qc2, 
               -x2013_measured_pin_height_cm) %>%
        janitor::remove_empty("rows") %>%
        select(set_id = set_code, date, arm_position = position, 
               pin_number, pin_height_cm, qaqc_code, 
               everything())


###############################
## Clean up 2014  
###############################

# replace (with "NA") the comment in the "endpt shape" column about 1020 not being read
dat$dat2014$endpt_shape <- gsub("1020", NA, dat$dat2014$endpt_shape)  
# get rid of column 15 and the last two columns; they're blank and/or metadata
dat$dat2014[c(15, 19, 20)] <- NULL
# first two rows are mostly blank too
dat$dat2014[1:2, ] <- NA


dat$dat2014 <- dat$dat2014 %>%
        remove_empty("rows") %>%
        rename(qc1 = primary_qa_qc_code,
               qc2 = secondary_qa_qc_code,
               front_back = f_b) %>%
        mutate(date = as.Date(date),
               pin_height_cm = as.numeric(x2014_measured_pin_height_cm),
               qaqc_code = case_when( !is.na(qc1) & is.na(qc2) ~ qc1,
                                      !is.na(qc1) & !is.na(qc2) ~ paste(qc1, qc2),
                                      is.na(qc1) & !is.na(qc2) ~ qc2,
                                      TRUE ~ NA_character_)) %>%
        select(-qc1, -qc2, -x2014_measured_pin_height_cm) %>%
        select(set_id = set_code, date, arm_position = position, pin_number, pin_height_cm, qaqc_code, everything())


###############################
## Clean up 2015 
###############################

# replace (with "NA") the comment in the "endpt shape" column about 1020 not being read
dat$dat2015$endpt_shape <- gsub("1020", NA, dat$dat2015$endpt_shape)  
dat$dat2015$set_code <- gsub("2020", NA, dat$dat2015$set_code)
# get rid of column 15 and the last three columns (this one has an extra); they're blank and/or metadata
dat$dat2015[c(15, 19, 20, 21)] <- NULL
# first two rows are not blank here



dat$dat2015 <- dat$dat2015 %>%
        remove_empty("rows") %>%
        rename(qc1 = primary_qa_qc_code,
               qc2 = secondary_qa_qc_code,
               front_back = f_b) %>%
        mutate(date = as.Date(date),
               pin_height_cm = as.numeric(x2015_measured_pin_height_cm),
               qaqc_code = case_when( !is.na(qc1) & is.na(qc2) ~ qc1,
                                      !is.na(qc1) & !is.na(qc2) ~ paste(qc1, qc2),
                                      is.na(qc1) & !is.na(qc2) ~ qc2,
                                      TRUE ~ NA_character_)) %>%
        select(-qc1, -qc2, -x2015_measured_pin_height_cm) %>%
        select(set_id = set_code, date, arm_position = position, pin_number, pin_height_cm, qaqc_code, everything())


###############################
## Clean up 2016 
###############################

# replace (with "NA") the comment in the "endpt shape" column about 1020 not being read
dat$dat2016$endpt_shape <- gsub("1020", NA, dat$dat2016$endpt_shape)  
dat$dat2016$set_code <- gsub("2020", NA, dat$dat2016$set_code)
# get rid of column 15 and the last three columns (this one has an extra); they're blank and/or metadata
dat$dat2016[c(15, 19, 20, 21)] <- NULL
# first two rows are not blank here


# insert NAs in date where everything else in the row is NA
index_check <- dat$dat2016[, -1]
empty_index <- which(rowSums(is.na(index_check)) == ncol(index_check))
dat$dat2016[empty_index, 1] <- NA


dat$dat2016 <- dat$dat2016 %>%
        remove_empty("rows") %>%
        rename(qc1 = primary_qa_qc_code,
               qc2 = secondary_qa_qc_code,
               front_back = f_b) %>%
        mutate(date = as.Date(date),
               pin_height_cm = as.numeric(x2016_measured_pin_height_cm),
               qaqc_code = case_when( !is.na(qc1) & is.na(qc2) ~ qc1,
                                      !is.na(qc1) & !is.na(qc2) ~ paste(qc1, qc2),
                                      is.na(qc1) & !is.na(qc2) ~ qc2,
                                      TRUE ~ NA_character_)) %>%
        select(-qc1, -qc2, -x2016_measured_pin_height_cm) %>%
        select(set_id = set_code, date, arm_position = position, pin_number, pin_height_cm, qaqc_code, everything())


###############################
## Clean up 2017
###############################
# this one will be tougher because of the cross-comparisons between readers
# for now only focusing on the main data table

dat$dat2017 <- dat$dat2017 %>%
    mutate(date = as.Date(date),
           pin_height = as.numeric(pin_length),
           front_back = case_when(f_b_front_back == 'F' ~ 'Front',
                                  f_b_front_back == 'B' ~ 'Back',
                                  TRUE ~ f_b_front_back)) %>%
    select(set_id = set_code, date, arm_position = position, pin_number, pin_height, front_back, notes)


###############################
## Clean up 2018
###############################

dat$dat2018 <- dat$dat2018 %>%
    mutate(date = as.Date(date),
           pin_height = as.numeric(pin_length_cm),
           front_back = case_when(f_b_front_back == 'F' ~ 'Front',
                                  f_b_front_back == 'B' ~ 'Back',
                                  TRUE ~ f_b_front_back)) %>%
    select(set_id = set_code, date, arm_position = position, pin_number, pin_height, front_back, notes)

###############################
###############################

# join together all the data frames in the list 'dat'
# this does NOT deal with column class differences
dat_all <- reshape::merge_recurse(dat) %>%
    mutate(sum_na = is.na(set_id) + is.na(date) + is.na(pin_height),
           reserve = 'WQB',
           date = ymd(date)) %>%
    rename(pin_height_cm = pin_height) %>%
    filter(sum_na <3,
           !is.na(date),
           !is.na(pin_number)) %>%
    select(reserve, everything(), -sum_na)

write_csv(dat_all, here('data', 'intermediate', 'WQB.csv'))

###
