library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)

path <- here::here("data", "submitted", "2019-04-08_WKB.xls")


clean_sheet <- function(sheet, range){
        # read it in
        dat <- read_excel(path, sheet = sheet, range = range) %>%
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
set0_2 <- clean_sheet(sheet = "SET0-2 Data ", range = "A27:J135") %>% 
        mutate(date_janitor = excel_numeric_to_date(as.numeric(date)),
               date_lubridate = lubridate::mdy(date),
               date_all = case_when(!is.na(date_lubridate) ~ date_lubridate,
                                    !is.na(date_janitor) ~ date_janitor)) %>% 
        select(-date_lubridate, -date_janitor) %>% 
        glimpse()
# make sure that didn't screw up dates
sum(is.na(set0_2$date_all))

# clean up
set0_2 %<>% mutate(date = date_all) %>% 
        select(-date_all) %>% 
        select(set_id, date, arm_position, pin_number, height_mm) %>% 
        arrange(set_id, date, arm_position, pin_number)



# SETs 9 - 11
set9_11 <- clean_sheet(sheet = "SET9-11 Data ", range = "A27:J135") %>%
        mutate(date = janitor::excel_numeric_to_date(as.numeric(date)))


