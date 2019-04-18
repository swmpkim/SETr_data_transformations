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


