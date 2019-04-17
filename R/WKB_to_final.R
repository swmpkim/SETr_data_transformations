library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)

path <- here::here("data", "submitted", "2019-04-08_WKB.xls")

sheets <- excel_sheets(path)  # spaces at end of sheet names
ranges <- "A27:J135"  # same range in all sheets


clean_sheet <- function(sheet, range){
        # read it in
        # specified that NA because there's at least one "<0" in the SET6-8 sheet
        dat <- read_excel(path, sheet = sheet, range = range, na = "<0") %>%
                clean_names()
        names(dat)[1] <- c("set_arm_pin")
        
        
        # separate out the set id, arm position, and pin number
        # there are different characters separating, and sometimes spaces
        # but the following code works
        dat_filled <- dat %>%
                separate(set_arm_pin, into = c("set_id", "arm_pin"), sep = "([\\-\\_])", fill = "left") %>% 
                mutate(set_id = toupper(set_id),
                       arm_position = case_when(nchar(arm_pin) > 1 ~ 
                                                        substr(arm_pin, nchar(arm_pin) - 1, nchar(arm_pin) - 1),
                                                TRUE ~ NA_character_),
                       pin_number = case_when(nchar(arm_pin) == 1 ~ arm_pin,
                                              TRUE ~ substr(arm_pin, nchar(arm_pin), nchar(arm_pin))),
                       set_id = str_trim(set_id, side = "both")) %>% 
                select(set_id, arm_pin, arm_position, pin_number, everything()) %>%
                fill(set_id, .direction = "down") %>%
                fill(arm_position, .direction = "down") %>%
                select(-arm_pin) 

        
        ### pivot to longer; format date:
        dat_long <- dat_filled %>%
                pivot_longer(
                        cols = starts_with("x"),
                        names_to = "date",
                        names_prefix = "x",
                        values_to = "height_mm"
                )
        
        dat_long
}


###########################################################################
# wrangle each sheet
###########################################################################

# SETs 0 - 2
set0_2 <- clean_sheet(sheet = sheets[1], range = ranges) %>% 
        mutate(date_janitor = excel_numeric_to_date(as.numeric(date)),
               date_lubridate = lubridate::mdy(date),
               date_all = case_when(!is.na(date_lubridate) ~ date_lubridate,
                                    !is.na(date_janitor) ~ date_janitor)) %>% 
        select(-date_lubridate, -date_janitor) 
# make sure that didn't screw up dates
sum(is.na(set0_2$date_all))
# clean up
set0_2 %<>% mutate(date = date_all) %>% 
        select(-date_all) %>% 
        select(set_id, date, arm_position, pin_number, height_mm) %>% 
        arrange(set_id, date, arm_position, pin_number)


# SETs 3-5
set3_5 <- clean_sheet(sheet = sheets[2], range = ranges) %>% 
        mutate(date = excel_numeric_to_date(as.numeric(date))) %>% 
        select(set_id, date, arm_position, pin_number, height_mm) %>% 
        arrange(set_id, date, arm_position, pin_number)


# SETs 6-8
set6_8 <- clean_sheet(sheet = sheets[3], range = ranges) %>% 
        mutate(date = excel_numeric_to_date(as.numeric(date))) %>% 
        select(set_id, date, arm_position, pin_number, height_mm) %>% 
        arrange(set_id, date, arm_position, pin_number) 

# SETs 9 - 11
set9_11 <- clean_sheet(sheet = sheets[4], range = ranges) %>%
        mutate(date = janitor::excel_numeric_to_date(as.numeric(date))) %>% 
        select(set_id, date, arm_position, pin_number, height_mm) %>% 
        arrange(set_id, date, arm_position, pin_number) 



######################################################

# deal with readers

#####################################################

# after combining data, will make a column because it's all been the same person
# moving forward there might be others but this is easy



# bind them all together
# add reader and qaqc_code columns (for pins)
# glue pin_ to the front of pin number
dat_all <- bind_rows(set0_2, set3_5) %>%
        bind_rows(., set6_8) %>%
        bind_rows(., set9_11) %>%
        select(set_id, date, everything()) %>% 
        mutate(reader = "Brunden",
               qaqc_code = NA_character_,
               arm_qaqc_code = NA_character_,
               pin_number = paste0("pin_", pin_number),
               arm_position = factor(arm_position, levels = c("N", "S", "E", "W")))

# set up specs for pivoting
spec <- dat_all %>%
        expand(pin_number, .value = c("height_mm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

# pivot
dat_wide <- dat_all %>%
        pivot_wider(spec = spec) %>% 
        mutate(arm_position = factor(arm_position, levels = c("N", "S", "E", "W"))) %>% 
        select(set_id, date, arm_position, arm_qaqc_code,
               pin_1_height_mm, pin_2_height_mm, pin_3_height_mm, 
               pin_4_height_mm, pin_5_height_mm, pin_6_height_mm, 
               pin_7_height_mm, pin_8_height_mm, pin_9_height_mm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything()) %>% 
        arrange(set_id, date, arm_position)

# check for inadvertent dupes
dupes <- get_dupes(dat_wide, set_id, date, arm_position)


# create the file path
xlpath <- here::here("data", "final", "wkbset.xlsx")

# source the script that generates the excel file
## first go change the excel sheet script but don't push it to github - 
## modify lines 15 and 16 so it doesn't turn arm position into a character
source(here::here("R", "excel_sheet_script.R"))


