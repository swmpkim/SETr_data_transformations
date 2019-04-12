library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)

path <- here::here("data", "submitted", "2019-04-12_CBV_GI.xlsx")
# there were some copy-and-paste issues in the originally submitted file
# they have been corrected in 2019-04-12 file

# looks like I can loop through sheets

sheet_vec <- excel_sheets(path)

dat <- list()
for(i in seq_along(sheet_vec)){
        dat[[i]] <- read_xlsx(path, sheet = sheet_vec[i])
}

dat_all <- reshape::merge_recurse(dat) %>%
        clean_names()

unique(dat_all$code)

dat_all_qc <- dat_all %>%
        mutate(arm_qaqc_code = case_when(!is.na(notes) ~ notes,
                                         TRUE ~ NA_character_),
               pin_qaqc_code = case_when(code == "Shell" ~ "HSM SH",
                                         code == "Hole" ~ "LHE",
                                         code == "Mound" ~ "HMD",
                                         code == "Shel;l" ~ "HSM SH",
                                         code == "Burrow" ~ "LHE CB",
                                         code == "Plant Root" ~ "HPL RT",
                                         code == "Root" ~ "HPL RT",
                                         code == "Clump" ~ "HPL RT",
                                         code == "Plant Mound" ~ "HPL RT",
                                         TRUE ~ NA_character_),
               date = as.Date(date),
               pin_number = paste0("pin_", pin_number)) %>%
        select(reserve, set_id, date, arm_position = arm_positon, arm_qaqc_code,
               pin_number, height_mm = pin_height_mm, qaqc_code = pin_qaqc_code)
# per email from Scott Lerberg on 4/11/19 - clump and plant mound at this site "can probably be classified as plant root, HPL RT".

dupes <- get_dupes(dat_all_qc, set_id, date, arm_position, pin_number)
dupes_5_2 <- get_dupes(dat[[10]] %>% clean_names(), set_id, date, arm_positon, pin_number)
# two sets of 2012-03-20 readings at SET 5-2
# Second one is 9-13 and has been changed
# also in the original spreadsheet the set_id in sheet 4-2 was
# mistakenly pasted as 4-1
# so the file 2019-04-12 IS THE CORRECTED FILE




get_dupes(dat_all_qc, set_id, date, arm_position, pin_number)


spec <- dat_all_qc %>%
        expand(pin_number, .value = c("height_mm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

dat_wide <- dat_all_qc %>%
        filter(set_id != "GI 5-2") %>%
        pivot_wider(spec = spec) %>%
        select(reserve, set_id, date, arm_position, arm_qaqc_code, 
               pin_1_height_mm, pin_2_height_mm, pin_3_height_mm, 
               pin_4_height_mm, pin_5_height_mm, pin_6_height_mm, 
               pin_7_height_mm, pin_8_height_mm, pin_9_height_mm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything()) 

dupes <- get_dupes(dat_wide, set_id, date, arm_position)


# create the file path
xlpath <- here::here("data", "final", "cbv_giset.xlsx")

# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R"))
