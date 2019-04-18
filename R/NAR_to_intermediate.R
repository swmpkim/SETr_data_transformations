library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)

path <- here::here("data", "submitted", "2019-04-18_NAR.xlsx")
# in the 4-18 xlsx file, I've added the dates that Kenny sent
# of Cogg sampling in 2014 and 2015
# (they were missing in original)
# also copied 10-26-16 down into Cog 11 and 12
# because they didn't have dates
# in Nag, Nag 3+ had 2013 dates in 2014. So I copied 10.21.2014 from Nag 1/2


# following along with the small multiples chapter of the 
# tidyxl/unpivotr book: https://nacnudus.github.io/spreadsheet-munging-strategies/small-multiples-with-all-headers-present-for-each-multiple.html


##############################################################################
# background in case it's needed
##############################################################################

# head(cog_cells)


# only take that first table and figure out how to deal with it
# table1 <- all_cells %>% 
#         filter(row %in% 2:13,
#                col %in% 1:5) %>% 
#         select(row, col, data_type, character, numeric, local_format_id)
# rectify(table1)
# 
# t1_tidy <- table1 %>% 
#         behead("NNW", set_date) %>% 
#         behead("N", arm_position) %>% 
#         behead("N", arm_bearing) %>% 
#         behead("W", pin_number) %>% 
#         filter(!is.na(pin_number)) %>% 
#         select(set_date, arm_position, arm_bearing, pin_number,
#                height_mm = numeric) %>%
#         arrange(arm_position, pin_number) 
# that's in a pretty good long format


###############################################################################
# make a function to unpivot a batch of cells
###############################################################################

unpivot <- function(cells){
        cells %>% 
                behead("NNW", set_date) %>% 
                behead("N", arm_position) %>% 
                behead("N", arm_bearing) %>% 
                behead("W", pin_number) %>% 
                filter(!is.na(pin_number),
                       set_date != "Notes") %>% 
                select(set_date, arm_position, arm_bearing, pin_number,
                       height_mm = numeric) %>%
                arrange(arm_position, pin_number) 
}


################################################################################
# work with Cogg
################################################################################

cog_cells <- xlsx_cells(path, sheets = "Cogg data") 

# the anchoring corner of the cells, in the Cogg sheet,
# is cells that start with "Cog"
cog_corners <- 
        cog_cells %>% 
        filter(str_detect(character, "^Cog")) %>% 
        select(row, col)

# partition the cells
cog_partitions <- partition(cog_cells, cog_corners)


cog_tidy <- cog_partitions %>%
        mutate(cells = map(cells, unpivot)) %>%
        unnest() %>%
        select(-corner_row, -corner_col) %>% 
        filter(!is.na(height_mm))  # only doing this because the only NAs happen when the whole SET is missing or a comment got read in from a weird place


###############################################################################
# work with Nag
###############################################################################

nag_cells <- xlsx_cells(path, sheets = "Nag data") 

# the anchoring corner of the cells, in the nagg sheet,
# is cells that start with "nag"
nag_corners <- 
        nag_cells %>% 
        filter(str_detect(character, "^Nag")) %>% 
        select(row, col)

# partition the cells
nag_partitions <- partition(nag_cells, nag_corners)


nag_tidy <- nag_partitions %>%
        mutate(cells = map(cells, unpivot)) %>%
        unnest() %>%
        select(-corner_row, -corner_col) %>% 
        filter(!is.na(height_mm)) 


###############################################################################
# combine
###############################################################################
dat_all <- bind_rows(cog_tidy, nag_tidy)

# separate out set and date
# get date into the right format
# insert qaqc columns
dat_long <- dat_all %>% 
        separate(set_date, into = c("site", "set_num", "date"), sep = " ") %>% 
        mutate(set_id = paste(site, set_num),
               date = lubridate::mdy(date),
               pin_number = tolower(pin_number),
               pin_number = gsub(" ", "_", pin_number),
               qaqc_code = NA_character_,
               arm_qaqc_code = NA_character_) %>% 
        select(-site, -set_num) %>% 
        select(set_id, everything()) 

# check for dupes
janitor::get_dupes(dat_long, set_id, date, arm_position, pin_number)


# pivot to wide
spec <- dat_long %>%
        expand(pin_number, .value = c("height_mm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

dat_wide <- dat_long %>%
        pivot_wider(spec = spec) %>%
        select(set_id, date, arm_position, arm_bearing, arm_qaqc_code,
               pin_1_height_mm, pin_2_height_mm, pin_3_height_mm, 
               pin_4_height_mm, pin_5_height_mm, pin_6_height_mm, 
               pin_7_height_mm, pin_8_height_mm, pin_9_height_mm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything()) 


# export
# create the file path
xlpath <- here::here("data", "final", "narset.xlsx")

# source the script that generates the excel file
source(here::here("R", "excel_sheet_script.R"))
