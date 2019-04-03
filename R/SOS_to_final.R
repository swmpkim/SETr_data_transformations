# this file is simple enough that one script should get the data all the way to the excel output


library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(janitor)
library(here)
library(XLConnect)


path <- here::here('data', 'submitted', '2018-10-10_SOS.xlsx')


dat <- read_excel(path, sheet = 'RSET data') %>%
        clean_names() %>%
        filter(!is.na(set_id))
names(dat) <- c("reserve", "set_id", "arm_position", "date", 
                "pin_1_height_mm", "pin_2_height_mm", "pin_3_height_mm",
                "pin_4_height_mm", "pin_5_height_mm", "pin_6_height_mm",
                "pin_7_height_mm", "pin_8_height_mm", "pin_9_height_mm",
                "pin_1_qaqc_code", "pin_2_qaqc_code", "pin_3_qaqc_code",
                "pin_4_qaqc_code", "pin_5_qaqc_code", "pin_6_qaqc_code",
                "pin_7_qaqc_code", "pin_8_qaqc_code", "pin_9_qaqc_code",
                "notes")

# replace numeric codes with letter codes:
# 1 to CUS - comment - uncertainty about position of surface
# 2 to HPD - high - plant dead (e.g. hummock)
# 3 to CLY - comment - measured less than one year after installation
# make a function
qaqc_replace <- function(x){
        temp_vec <- x
        temp_vec[temp_vec == "1"] <- "CUS"
        temp_vec[temp_vec == "2"] <- "HPD"
        temp_vec[temp_vec == "3"] <- "CLY"
        temp_vec[temp_vec == "3,2"] <- "HPD CLY"
        temp_vec
}

# apply that function to all the qaqc_code columns
code_index <- grep("qaqc_code", names(dat))
for(i in seq_along(code_index)){
        col_index <- code_index[i]
        dat[[col_index]] <- qaqc_replace(dat[[col_index]])
}

grep("not recorded", dat$notes)
glimpse(dat[9:12, ])

# i think that qa/qc issue has been dealt with by now so i will ignore it....
dat$notes <- NULL



# for ease of getting into excel based on existing code
dat_wide <- dat

# write to excel:

# turn date into a character string because otherwise it saves crazily
dat_wide$date <- as.character(dat_wide$date)
dat_wide$arm_position <- as.character(dat_wide$arm_position)

# create the file path
xlpath <- here("data", "final", "sosset.xlsx")
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

