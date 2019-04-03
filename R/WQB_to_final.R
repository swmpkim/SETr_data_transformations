### this script is developed using the dev version of tidyr
# version 0.8.3.9000
# so I can use the new pivot_wider() function

library(tidyverse)
library(here)
library(janitor)  # to check for dupes
library(XLConnect)

# read in data ----
path <- here::here("data", "intermediate_long", "WQB.csv")
dat <- read.csv(path, stringsAsFactors = FALSE)


# format columns
dat <- dat %>%
        mutate(set_id = as.character(set_id),
               date = as.Date(date),
               pin_number = paste0("pin_", as.character(pin_number)),
               arm_position = as.character(arm_position))



# pivot out pin heights and qaqc codes

################################################
# lots of code testing here for future reference ----
################################################

# test on just a subset
dat_wide <- dat %>%
        select(reserve:front_back) %>%
        pivot_wider(names_from = pin_number,
                    values_from = c(pin_height_cm, qaqc_code))

# specify the names - i want pin number at the front, rather than at the end,
# of the new column names

# much borrowing from this article:
# https://tidyr.tidyverse.org/dev/articles/pivot.html#wider-1

# first see what it's doing
dat %>%
        pivot_wider_spec(names_from = pin_number, values_from = c(pin_height_cm, qaqc_code))

# now modify
spec <- dat %>%
        expand(pin_number, .value = c("pin_height_cm", "qaqc_code")) %>%
        mutate(.name = paste0(pin_number, ifelse(.value == "pin_height_cm", "_height_cm", "_qaqc_code")))

# and test this on a subset
dat_wide <- dat %>%
        select(reserve:front_back) %>%
        pivot_wider(spec = spec) %>%
        select(reserve, set_id, date, arm_position, 
               pin_1_height_cm, pin_2_height_cm, pin_3_height_cm, 
               pin_4_height_cm, pin_5_height_cm, pin_6_height_cm, 
               pin_7_height_cm, pin_8_height_cm, pin_9_height_cm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything())

######################################################
# testing over
######################################################


# pivot ----

# spec <- dat %>%
#         expand(pin_number, .value = c("pin_height_cm", "qaqc_code")) %>%
#         mutate(.name = paste0(pin_number, 
#                               ifelse(.value == "pin_height_cm", 
#                                      "_height_cm", "_qaqc_code")))

spec_more <- dat %>%
        expand(pin_number, .value = c("pin_height_cm", "qaqc_code",
                                      "pin_length")) %>%
        mutate(.name = paste0(pin_number, "_", 
                              case_when(.value == "pin_height_cm" ~ "height_cm",
                                        .value == "qaqc_code" ~ "qaqc_code",
                                        TRUE ~ .value)))

# get rid of calculated columns that need a value for each pin,
# then pivot
dat_wide <- dat %>%
        select(-c(pin_length_pin_meas:marsh_set_elevation_navd88_in_m),
               -pin_length_pin_meas_cm,
               -notes,
               -c(spal_nearby:aster_nearby)) %>%
        pivot_wider(spec = spec_more) %>%
        select(reserve, set_id, date, arm_position, 
               time_readings_started, reader,
               pin_1_height_cm, pin_2_height_cm, pin_3_height_cm, 
               pin_4_height_cm, pin_5_height_cm, pin_6_height_cm, 
               pin_7_height_cm, pin_8_height_cm, pin_9_height_cm,
               pin_1_qaqc_code, pin_2_qaqc_code, pin_3_qaqc_code, 
               pin_4_qaqc_code, pin_5_qaqc_code, pin_6_qaqc_code, 
               pin_7_qaqc_code, pin_8_qaqc_code, pin_9_qaqc_code,
               everything())


# check for inadvertent duplicates
dupes <- get_dupes(dat_wide, set_id, date, arm_position)

# phew! got rid of them all!


# add a column for arm-level qaqc codes
dat_wide <- dat_wide %>%
        mutate(arm_qaqc_code = NA_character_) %>%
        select(reserve, set_id, date, 
               arm_position, arm_qaqc_code,
               everything())



# let's see what got cut out:
dat_trimmed <- dat %>%
        select(c(pin_length_pin_meas:marsh_set_elevation_navd88_in_m),
               pin_length_pin_meas_cm,
               notes,
               c(spal_nearby:aster_nearby)) %>%
        names()


# intermediate file-writing if desired:

# out_path <- here("data", "intermediate_wide", "WQB_wider.csv")
# write_csv(dat_wide, out_path, na = "")


## split up and format for excel ----

# turn date into a character string because otherwise it saves crazily
dat_wide$date <- as.character(dat_wide$date)

# create the file path
xlpath <- here("data", "final", "wqbset.xlsx")
# create the workbook
wb <- loadWorkbook(xlpath, create = TRUE)


# create worksheets by looping through all SETs. first make list of each SET id
unique_sets <- unique(dat_wide$set_id)

# create styles for the worksheets
# create the style for highlighted rows
highlighted2 <- createCellStyle(wb)
setFillPattern(highlighted2, fill = XLC$"FILL.SOLID_FOREGROUND")
setFillForegroundColor(highlighted2, color = XLC$"COLOR.GREY_25_PERCENT")

# create the style for the header row
header <- createCellStyle(wb)
setFillPattern(header, fill = XLC$"FILL.SOLID_FOREGROUND")
setFillForegroundColor(header, color = XLC$"COLOR.WHITE")
setBorder(header, 'bottom', type = XLC$"BORDER.DOUBLE", color = XLC$"COLOR.BLACK")


# build the worksheets, one SET at a time
for(i in seq_along(unique_sets)) {
        # subset the data
        dat_sub <- dat_wide[dat_wide$set_id == unique_sets[i], ]
        
        # generate a name for the worksheet based on the SET id
        sheetname <- as.character(unique_sets[i])
        # if there are more than 31 characters, use the first 31
        # and generate a warning
        if (nchar(sheetname) > 31) {
                warning(paste(sheetname, "is too long of a worksheet name. Only the first 31 characters will be used."))
                sheetname <- substr(sheetname, 1, 31)
        }
        
        
        
        ### GENERATE A FORMATTED EXCEL SHEET
        # generate a vector of row numbers to highlight in a formatted Excel sheet
        groupsof4 <- nrow(dat_sub)/4
        rowstohighlight <- rep(0, groupsof4*2)
        nexttohighlight <- 1:4
        nexttoindex <- 1:4
        nloop <- ceiling(length(rowstohighlight) / 4)
        for(j in 1:nloop){
                rowstohighlight[nexttoindex] <- nexttohighlight
                nexttohighlight <- nexttohighlight + 8
                nexttoindex <- nexttoindex + 4
        }
        
        
        # create the worksheet
        createSheet(wb, name = sheetname)
        
        # write the subsetted data into the worksheet
        writeWorksheet(wb, dat_sub, sheet = sheetname)
        
        
        # highlight the rows
        rowIndex <- rowstohighlight + 1    
        colIndex <- 1:ncol(dat_sub)
        rc = expand.grid(row = rowIndex, col = colIndex)
        setCellStyle(wb, sheet = sheetname, row = rc$row, col = rc$col, cellstyle = highlighted2)
        
        # format the header row
        setCellStyle(wb, sheet = sheetname, row = 1, col = colIndex, cellstyle = header)
}

saveWorkbook(wb)
