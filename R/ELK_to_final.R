library(tidyverse)
library(here)
library(janitor)  # to check for dupes
library(XLConnect)

# read in data ----
path <- here::here("data", "intermediate_long", "ELK_intermed.csv")
dat <- read.csv(path, stringsAsFactors = FALSE)

# mostly the formatting is okay but need to:
# paste pin_ in front of pin numbers, and
# there are no qaqc columns here so need to insert a blank one
dat <- dat %>%
        mutate(pin_number = paste0("pin_", pin_number),
               qaqc_code = NA_character_)


# set up specs for pivoting
spec <- dat %>%
        expand(pin_number, .value = c("height_mm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, "_", .value))

# pivot
dat_wide <- dat %>%
        pivot_wider(spec = spec) %>%
        select(set_id, date, reader, arm_position,
               pin_1_height_mm, pin_2_height_mm, pin_3_height_mm, 
               pin_4_height_mm, pin_5_height_mm, pin_6_height_mm, 
               pin_7_height_mm, pin_8_height_mm, pin_9_height_mm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything()) 

# codes for set ids
set_lookup <- data.frame(current_name = sort(unique(dat_wide$set_id)),
                         coded_name = c("AZEN", "AZES",
                                        "BIGE", "BIGW",
                                        "RNDN", "RNDS",
                                        "RUBN", "RUBS"))

# join them up and replace the current set_id column
# also go ahead and add a column for arm-level qaqc code
dat_wide <- left_join(dat_wide, set_lookup, by = c("set_id" = "current_name")) %>%
        select(-set_id) %>%
        mutate(arm_qaqc_code = NA_character_) %>%
        select(set_id = coded_name, date, 
               arm_position, arm_qaqc_code,
               everything())


# check for inadvertent dupes
dupes <- get_dupes(dat_wide, set_id, date, arm_position)


# write out intermediate wide file if desired
# out_path <- here("data", "intermediate_wide", "ELK_wider.csv")
# write_csv(dat_wide, out_path, na = "")
