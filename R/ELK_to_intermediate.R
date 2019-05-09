library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(lubridate)

### this is pretty simple:
# read in the range for a given site
# split out the arm and pin columns
# use tidyr::fill to fill down set_id and arm



# main spreadsheet

path <- here::here("data", "submitted", "2019-04-04_ELK.xlsx")



# function to wrangle that data
# put the sheet and range in quotes like you would inside read_excel()

clean_sheet <- function(sheet, range){

        dat <- read_excel(path, sheet = sheet, range = range) %>%
                clean_names()
        names(dat)[1:2] <- c("set_id", "arm_pin")
        
        
        dat_filled <- dat %>%
                mutate(pin_number = substr(arm_pin, nchar(arm_pin), nchar(arm_pin)),
                       arm_position = case_when(nchar(arm_pin) > 1 ~ substr(arm_pin, 1, 1),
                                                TRUE ~ NA_character_)) %>%
                fill(arm_position, .direction = "down") %>%
                fill(set_id, .direction = "down") %>%
                select(-arm_pin) %>%
                select(set_id, arm_position, pin_number, everything())
        
        
        ### pivot to longer; format date:
        dat_long <- dat_filled %>%
                pivot_longer(
                        cols = starts_with("x"),
                        names_to = "date",
                        names_prefix = "x",
                        values_to = "height_mm"
                ) 
        # removed date formatting step due to issues in big creek and azevedo
        dat_long
}


###########################################################################
# wrangle each site
###########################################################################

# rubis
rubis <- clean_sheet(sheet = "Rubis SET Data", range = "A22:P94") %>%
        mutate(date = lubridate::mdy(date))

# round hill
round_hill <- clean_sheet("Round Hill Set Data", range = "A22:P94") %>%
        mutate(date = lubridate::mdy(date))

# big creek
big_creek <- clean_sheet("Big Creek SET Data", range = "A23:P95")

# azevedo
azevedo <- clean_sheet("Azevedo SET Data", range = "A22:P94")



## big_creek and azevedo need some date cleanup
big_creek <- big_creek %>%
        mutate(date2 = case_when(is.na(str_match(date, "_")) ~ 
                                        janitor::excel_numeric_to_date(as.numeric(date)),
                                TRUE ~ lubridate::mdy(date))) %>%
        select(-date) %>%
        select(set_id, date = date2, arm_position, pin_number, height_mm)


azevedo <- azevedo %>%
        mutate(date2 = case_when(is.na(str_match(date, "_")) ~ 
                                         janitor::excel_numeric_to_date(as.numeric(date)),
                                 TRUE ~ lubridate::mdy(date))) %>%
        select(-date) %>%
        select(set_id, date = date2, arm_position, pin_number, height_mm)


######################################################

# deal with readers

#####################################################

# function

reader_fun <- function(sheet, range){

        readers <- read_excel(path, sheet = sheet, 
                              range = range, col_names = FALSE)
        col_nums <- seq(1, ncol(readers))
        names_vec <- paste0("col_", col_nums)
        names(readers) <- names_vec
        
        reader_table <- readers %>%
                pivot_longer(-col_1, names_to = "col", values_to = "value") %>%
                pivot_wider(names_from = col_1, values_from = value) %>%
                clean_names() %>%
                mutate(sample_date = lubridate::mdy(sample_date)) %>%
                select(sample_date, reader)
        reader_table
}


# with some manual corrections because honestly it's easier than automating
# since this only has to happen once

readers_rubis <- reader_fun("Rubis SET Data", "B4:P5")
rubis <- left_join(rubis, readers_rubis, by = c("date" = "sample_date"))


readers_round_hill <- reader_fun("Round Hill Set Data", "B4:P5")
readers_round_hill$sample_date[11] <- mdy("12/2/2015")
readers_round_hill$reader[14] <- "Andrea Woolfolk"  # confirmed via email with Charlie on 4/4/19
round_hill <- left_join(round_hill, readers_round_hill, by = c("date" = "sample_date"))


readers_big_creek <- reader_fun("Big Creek SET Data", "B4:P5")
readers_big_creek$sample_date[9:14] <- mdy(c("11/26/2013",
                                             "2/9/2015",
                                             "12/2/2015",
                                             "10/11/2016",
                                             "8/30/2017",
                                             "9/17/2018"))
big_creek <- left_join(big_creek, readers_big_creek, by = c("date" = "sample_date"))



readers_azevedo <- reader_fun("Azevedo SET Data", "B4:P5")
readers_azevedo$sample_date[11:14] <- mdy(c("12/2/2015",
                                            "10/11/2016",
                                            "8/30/2017",
                                            "9/17/2018"))
azevedo <- left_join(azevedo, readers_azevedo, by = c("date" = "sample_date"))




############################################################
# join everything together
# and write intermediate file
############################################################

# bind them all together
dat_all <- bind_rows(rubis, round_hill) %>%
        bind_rows(., big_creek) %>%
        bind_rows(., azevedo) %>%
        select(set_id, date, everything())

# 2019-05-09 have learned this is possible to do in one step:
# dat_all <- bind_rows(list(rubis, round_hill, big_creek, azevedo))
# that also replaces the need for reshape::merge_recurse


# write out intermediate file
out_path <- here::here("data", "intermediate_long", "ELK_intermed.csv")
write_csv(dat_all, out_path)
