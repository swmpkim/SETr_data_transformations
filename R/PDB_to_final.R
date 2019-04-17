library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)

path <- here::here("data", "submitted", "2018-09-21_PDB.xlsx")

# am basically re-using the loop from the original script
# updated to use pivot_longer() rather than gather()


sheetnames <- tolower(excel_sheets(path))
sheetnames <- gsub(' ', '_', sheetnames)

# manually figuring out the last column of the main data table in each sheet:
lastcols <- c('M', 'F', 'P', 'M', 'P', 
              'M', 'P', 'P', 'P', 'M', 
              'P', 'P', 'Q', 'P', 'P',
              'L', 'L', 'L', 'L', 'H',
              'J', 'J', 'K', 'F')
# make sure that's the same length as the sheet names
length(sheetnames) == length(lastcols)

# generate data ranges for reading in sheets
ranges <- paste0('A1:', lastcols, '37')

#############################################
# read in all the sheets
#############################################

# set up the list
dat <- list()

# read the sheets in
for(i in seq_along(sheetnames)){
        dat_in <- read_excel(path, sheet = i, range = ranges[i], 
                             na = c("ND", "NA", "plant", "CLAM")) %>% 
                clean_names()
        
        dat_long <- dat_in %>%
                mutate(pin_number = paste0("pin_", pin)) %>%
                pivot_longer(cols = starts_with("x"),
                             names_to = "date",
                             names_prefix = "x",
                             values_to = "height_cm") %>% 
                mutate(date = excel_numeric_to_date(as.numeric(date)),
                       reserve = 'PDB',
                       set_id = sheetnames[i]) %>%
                select(reserve, set_id, date, arm_position, pin_number, height_cm) %>%
                arrange(set_id, date, arm_position, pin_number)
        
        dat[[i]] <- dat_long
}


############################################
# glue everything together into one big list
############################################

dat_all <- reshape::merge_recurse(dat)


# prepare for pivoting
# set up specs for pivoting
spec <- dat_all %>%
        expand(pin_number, .value = c("height_cm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

dat_wide <- dat_all %>%
        mutate(qaqc_code = NA_character_,
               arm_qaqc_code = NA_character_) %>%
        pivot_wider(spec = spec) %>%
        select(set_id, date, arm_position, arm_qaqc_code,
               pin_1_height_cm, pin_2_height_cm, pin_3_height_cm, 
               pin_4_height_cm, pin_5_height_cm, pin_6_height_cm, 
               pin_7_height_cm, pin_8_height_cm, pin_9_height_cm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything()) 

# create the file path
xlpath <- here::here("data", "final", "pdb_allset.xlsx")

# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R"))


# priority sites only
priority_sites <- c("1n", "so_2", "no_4", "fwo_6", "an_7",
                    "ao_8", "mno_10", "msn_11", "mso_12",
                    "pn_13", "po_14", "14b", "5b", "12b")
dat_wide <- dat_wide %>% 
        filter(set_id %in% priority_sites)
xlpath <- here::here("data", "final", "pdbset.xlsx")
source(here::here("R", "excel_sheet_script.R"))


# secondary sites
# rebuilt dat_wide from dat_all before building this
dat_wide <- dat_wide %>% 
        filter(!(set_id %in% priority_sites))
xlpath <- here::here("data", "final", "pdb_secondaryset.xlsx")
source(here::here("R", "excel_sheet_script.R"))
