library(tidyverse)
library(readxl)
library(hms)
library(janitor)
library(lubridate)

# data in long format
# one sheet per SET
# empty rows for future data entry; these will be removed

path <- here::here("data", "submitted", "2019-05-03_MAR.xlsx")


# get a vector of all the sheets:
sheetnames <- excel_sheets(path)
to_read <- sheetnames[1:15]

# make a list for output
dat_list <- list()

# read in each sheet
for(i in seq_along(to_read)){
        dat_list[[i]] <- read_xlsx(path, sheet = sheetnames[i]) %>% 
                clean_names() %>% 
                mutate(time = as.character(as.hms(time, tz = "UTC"))) %>% 
                filter(!is.na(date))
}

# merge all the individual data frames
dat_all <- reshape::merge_recurse(dat_list)

probs_table <- data.frame(original = sort(unique(dat_all$problem_description)),
                          qaqc_code = c("HMD",
                                       "LHE",
                                       "LHE",
                                       "LHE",
                                       "LDP AP",
                                       "LDP",
                                       "COT",
                                       "HMD",
                                       "HMD",
                                       "HPL RT",
                                       "HPD ST",
                                       "HSM SK",
                                       "HPD ST",
                                       "HPL",
                                       "HPL VH",
                                       "HPL VV",
                                       "LHE",
                                       "LHE",
                                       "HPL",
                                       "HPL",
                                       "COT",
                                       "CRM CUS",
                                       "CCF",
                                       "HPD",
                                       "HPD"))
probs_table # so MAR staff can tell me if I chose wrong

dat_all2 <- left_join(dat_all, probs_table, by = c("problem_description" = "original"))

dat_trimmed <- dat_all2 %>% 
        mutate(arm_position = case_when(!is.na(position_direction) ~ position_direction,
                                        is.na(position_direction) & !is.na(position) ~ position,
                                        TRUE ~ "TROUBLE")) %>%
        select(reserve,
               set_id = set_identifier,
               arm_position,
               pin_number = pin,
               date,
               time,
               height_mm = pin_height,
               qaqc_code,
               rod,
               field_reader,
               field_recorder,
               notes = additonal_notes)

# make a data frame with add'l notes, because i'm going to get rid of them to pivot
keep_notes <- dat_trimmed %>% 
        filter(!is.na(notes))


# get rid of notes column
# do some other modification
dat_trimmed <- dat_trimmed %>%
        select(-notes) %>% 
        mutate(pin_number = paste0("pin_", pin_number),
               arm_qaqc_code = NA_character_)

# there's one reading date/arm where a time is present in the first and no others
# so make them all read noon
dat_trimmed %>% 
        filter(set_id == "HF1",
               arm_position == "NE",
               date > "2016-09-06",
               date < "2016-09-08")
dat_trimmed$time[dat_trimmed$set_id == "HF1" & dat_trimmed$arm_position == "NE" &
                         dat_trimmed$date < "2016-09-08" & dat_trimmed$date > "2016-09-06"] <-  "12:00:00"




# set up pivot specs
# pivot to wide
spec <- dat_trimmed %>%
        expand(pin_number, .value = c("height_mm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

dat_wide <- dat_trimmed %>%
        pivot_wider(spec = spec) %>%
        select(set_id, date, arm_position, arm_qaqc_code,
               pin_1_height_mm, pin_2_height_mm, pin_3_height_mm, 
               pin_4_height_mm, pin_5_height_mm, pin_6_height_mm, 
               pin_7_height_mm, pin_8_height_mm, pin_9_height_mm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything()) 

# get rid of the trailing :00 on times:
dat_wide$time <- substr(dat_wide$time, 1, nchar(dat_wide$time) - 3)

# check for inadvertent dupes
(dupes <- get_dupes(dat_wide, set_id, date, arm_position))



# split up date into year, month, and day columns
# so excel doesn't make dates all crazy
dat_wide <- dat_wide %>% 
        mutate(year = year(date),
               month = month(date),
               day = mday(date)) %>% 
        select(-date) %>% 
        select(set_id, year, month, day, everything())



# export
# create the file path
xlpath <- here::here("data", "final", "marset.xlsx")

# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R"))
