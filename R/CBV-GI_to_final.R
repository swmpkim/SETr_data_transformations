library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)

path <- here::here("data", "submitted", "2019-04-11_CBV_GI.xlsx")

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
        mutate(arm_qaqc_code = NA_character_,
               pin_qaqc_code = case_when(code == "Shell" ~ "HSM SH",
                                         code == "Hole" ~ "LHE",
                                         code == "Mound" ~ "HMD",
                                         code == "Shel;l" ~ "HSM SH",
                                         code == "Burrow" ~ "LHE CB",
                                         code == "Plant Root" ~ "HPL RT",
                                         code == "Root" ~ "HPL RT",
                                         code == "Clump" ~ "HPL",
                                         code == "Plant Mound" ~ "HPL",
                                         TRUE ~ NA_character_))
