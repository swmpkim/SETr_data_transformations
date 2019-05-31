# spit out long-format files for all the "final" files


library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)
library(lubridate)
library(here)

# list all the files in the folder
path <- here::here("data", "final")
files <- dir(path, pattern = ".xlsx")

# as of the date I'm running this, I don't want certain files.
# do want these:
file_index <- which((files %in% c("apaset.xlsx",
                                  "cbmset.xlsx",
                                  "cbv_giset.xlsx",
                                  "delset.xlsx",
                                  "elkset.xlsx",
                                  "gndset.xlsx",
                                  "grbset.xlsx",
                                  "marset.xlsx",
                                  "narset.xlsx",
                                  "pdbset.xlsx",
                                  "sosset.xlsx",
                                  "welset.xlsx",
                                  "wkbset.xlsx",
                                  "wqbset.xlsx")))



for(i in seq_along(files)){
        # skip if this file isn't one of the ones we want
        if(!(i %in% file_index)){next}
        
        # otherwise, do everything
        file_path <- paste0(path, "/", files[i])
        
        # read in all sheets and append them to each other
        dat <- file_path %>% 
                excel_sheets() %>% 
                set_names() %>% 
                map_df( ~ read_excel(path = file_path, sheet = .x), .id = "sheet")
        
        # check to make sure the SET ID that was entered matches the name of each sheet
        mismatches <- dat$set_id != dat$sheet
        
        # make this whole script stop if something doesn't match
        if(sum(mismatches) > 0){
                print(dat[mismatches, c("sheet", "set_id", "date", "arm_position")])
                stop("There are SET IDs that do not match the sheet name. Please check and correct the rows printed above before proceeding.")
        }else{
                # if no problem, do everything else
                
                # first, format the data:
                # get rid of the "sheet" column
                # make sure date is date format; 
                # several columns should be character: set_id, arm_position, 
                # and anything that ends in qaqc_code
                dat_formatted <- dat %>% 
                        select(-sheet) %>% 
                        mutate_at(c("set_id", "arm_position"), as.character) %>% 
                        mutate_at(vars(ends_with("qaqc_code")), as.character)
                
                
                # have to change column names first because there are too many underscores
                names(dat_formatted) <- gsub("qaqc_code", "qaqccode", names(dat_formatted))
                names(dat_formatted) <- gsub("pin_", "pin", names(dat_formatted))
                names(dat_formatted) <- gsub("height_", "height", names(dat_formatted))
                
                # set up to pivot
                spec <- dat_formatted %>% 
                        pivot_longer_spec(
                                cols = starts_with("pin1_height"):"pin9_qaqccode",
                                names_to = c("pinnumber", ".value"),
                                names_sep = "_"
                        )
                
                # pivot to longer
                dat_long <- dat_formatted %>% 
                        pivot_longer(spec = spec)
                
                # put underscores back in the names
                names(dat_long) <- gsub("pin", "pin_", names(dat_long))
                dat_long$pin_number <- gsub("pin", "pin_", dat_long$pin_number)
                names(dat_long) <- gsub("qaqccode", "qaqc_code", names(dat_long))
                names(dat_long) <- gsub("height", "height_", names(dat_long))
                
                # format and arrange before output
                dat_long <- dat_long %>% 
                        select(set_id, year, month, day, 
                               arm_position, arm_qaqc_code, 
                               pin_number, starts_with("height"), 
                               qaqc_code, everything()) %>% 
                        arrange(set_id, year, month, day, arm_position, pin_number)
                
                # generate output and write out file
                file_in <- str_extract(file_path, "[:alpha:]+\\.xls")
                file_out <- paste0(substr(file_in, 1, nchar(file_in)-4), "_processed.csv")
                out_path <- here::here("data", "processed")
                file_out <- paste0(out_path, "/", file_out)
                write.csv(dat_long, file_out, row.names = FALSE)
        }
}

message("\n \n Finished! \n \n")
