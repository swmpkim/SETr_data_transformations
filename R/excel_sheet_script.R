# source this script to turn dat_wide into an excel file
# with one sheet per SET ID

# create the file path in the script that sources this one
# should look like this:
# xlpath <- here::here("data", "final", "wqbset.xlsx")

library(here)
library(XLConnect)


# make sure date and arm position are characters
# arrange things appropriately for splitting
dat_wide <- dat_wide %>%
        mutate(arm_position = as.character(arm_position)) %>%
        arrange(set_id, year, month, day, arm_position)

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

message("\n \n Excel file created! \n \n")
