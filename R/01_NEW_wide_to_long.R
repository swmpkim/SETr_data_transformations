library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(lubridate)

# interactively choose file to work with
file_path <- choose.files()


# read in all sheets and append them to each other
dat <- file_path %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map_df( ~ read_excel(path = file_path, sheet = .x), .id = "sheet")

# check to make sure the SET ID that was entered matches the name of each sheet
# we want this to be 0
mismatches <- dat$set_id != dat$sheet


# make something happen if they don't match
if(sum(mismatches) > 0){
        print(paste("row number(s):", which(mismatches == 1)))
        print(dat[mismatches, 1:4])
        stop("There are SET IDs that do not match the sheet name. Please check and correct the rows printed above before proceeding.")
        }


# if no problem, format the data:
# get rid of the "sheet" column
# make sure date is date format; 
# several columns should be character: set_id, arm_position, 
# and anything that ends in qaqc_code
dat_formatted <- dat %>% 
        select(-sheet) %>% 
        mutate(date = lubridate::ymd(date)) %>% 
        mutate_at(c("set_id", "arm_position"), as.character) %>% 
        mutate_at(vars(ends_with("qaqc_code")), as.character)

# have to pivot to longer first, then bust up the column names

# have to change column names first because there are too many underscores
names(dat_formatted) <- gsub("qaqc_code", "qaqccode", names(dat_formatted))
names(dat_formatted) <- gsub("pin_", "pin", names(dat_formatted))
names(dat_formatted) <- gsub("height_", "height", names(dat_formatted))


spec <- dat_formatted %>% 
        pivot_longer_spec(
                cols = starts_with("pin1_height"):"pin9_qaqccode",
                names_to = c("pinnum", ".value"),
                names_sep = "_"
        )


dat_long <- dat_formatted %>% 
        pivot_longer(spec = spec)



###############################################################################
# this is how it was set up to pivot to wide:
spec <- dat %>%
        expand(pin_number, .value = c("height_cm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))




###############################################################################
# this is kind of how i did it in earlier scripts:

# find the columns that contain mm or cm in their names
pos_of_hts <- grep("mm|cm", names(dat_formatted))
# find the columns with 'flag' in their names
pos_of_flags <- grep("pin_[c(1,2,3,4,5,6,7,8,9)]_qaqc_code", names(dat_formatted))
# pull all pin heights and flags into long format,
# then separate column names to identify pin numbers and type of information (flag or height)
# and spread back out so there's one row per pin, and a column each for height and flag
dat_long <- dat_formatted %>%
        gather(key = "param", value = "value", c(pos_of_flags, pos_of_hts)) %>%
        separate(param, into = c("pin", "num", "type", "units")) %>%
        mutate(pin_number = paste(pin, num, sep = "_")) %>%
        select(-pin, -num) %>%
        mutate(type = case_when(type == "code" ~ "code",
                                TRUE ~ paste0("pin_height_", type))) %>%
        spread(key = type, value = value)
        