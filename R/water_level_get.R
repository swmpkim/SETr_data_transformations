library(rnoaa)
library(tidyverse)
library(lubridate)

# https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8735180

# to get the csv of data with seasonal trend removed:
# https://tidesandcurrents.noaa.gov/sltrends/data/8735180_meantrend.csv
# presumably, just replace the station id to get other stations
# and that's everything
# that's what COOPS uses to calculate their trends


url_path <- "https://tidesandcurrents.noaa.gov/sltrends/data/8735180_meantrend.csv"
# should be able to paste together different components of the url and
# loop through stations

dat_path <- here::here("data", "water_level", "8735180_dauph_isl.csv")
download.file(url = url_path, destfile = dat_path)


url_path <- "https://tidesandcurrents.noaa.gov/sltrends/data/8656483_meantrend.csv"
# should be able to paste together different components of the url and
# loop through stations

dat_path <- here::here("data", "water_level", "8656483_beaufort_nc.csv")
download.file(url = url_path, destfile = dat_path)




# get monthly mean water level for Dauphin Island from 2010 - 2017
dat <- coops_search(station_name = 8735180, begin_date = 20100101,
             end_date = 20171231, datum = "stnd", product = "monthly_mean",
             time_zone = "lst")

dat2 <- dat$data %>%
        mutate(date = ymd(paste0(year, month, "-01")))

# calculate linear change
lm_msl <- lm(MSL ~ date, data = dat2)

summary(lm_msl)
# i think the units on that slope are meters/day ???

# get that rate into mm/yr
(msl_mm_yr <- lm_msl$coefficients[2] * 365.25 * 1000)

# time_zone = "lst" makes a big difference:
# rate is 15.42 under lst
# was 17.5 under default of time_zone = "gmt"
# difference is almost not noticeable in the long-term calculations below



# play with long-term data to make sure I'm converting correctly
# max of 10 years to download at once
dat0 <- coops_search(station_name = 8735180, begin_date = 19660101,
                     end_date = 19661231, datum = "stnd", product = "monthly_mean",
                     time_zone = "gmt")
dat1 <- coops_search(station_name = 8735180, begin_date = 19670101,
                    end_date = 19761231, datum = "stnd", product = "monthly_mean",
                    time_zone = "gmt")
dat2 <- coops_search(station_name = 8735180, begin_date = 19770101,
                     end_date = 19861231, datum = "stnd", product = "monthly_mean",
                     time_zone = "gmt")
dat3 <- coops_search(station_name = 8735180, begin_date = 19870101,
                     end_date = 19961231, datum = "stnd", product = "monthly_mean",
                     time_zone = "gmt")
dat4 <- coops_search(station_name = 8735180, begin_date = 19970101,
                     end_date = 20061231, datum = "stnd", product = "monthly_mean",
                     time_zone = "gmt")
dat5 <- coops_search(station_name = 8735180, begin_date = 20070101,
                     end_date = 20161231, datum = "stnd", product = "monthly_mean",
                     time_zone = "gmt")
dat_all <- dat0$data %>% 
        bind_rows(., dat1$data) %>%
        bind_rows(., dat2$data) %>%
        bind_rows(., dat3$data) %>%
        bind_rows(., dat4$data) %>%
        bind_rows(., dat5$data) %>%
        mutate(date = ymd(paste0(year, month, "-01")))

glimpse(dat_all)

# save as .RData file so I don't have to redownload every time:
out_file <- here::here("data", "water_level", "water_lev_dauph_isl.RData")
save(dat_all, file = out_file)



