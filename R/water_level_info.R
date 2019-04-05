library(rnoaa)
library(tidyverse)
library(lubridate)

# https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8735180


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

lm_longterm <- lm(MSL ~ date, data = filter(dat_all, date >= "1967-01-01"))
summary(lm_longterm)
(longterm_mm_yr <- lm_longterm$coefficients[2] * 365.25 * 1000)
# reported change for 1966 - 2016 on the website is 3.50, with CI from 2.88 to 4.12
# my calculations came up with 3.548 mm/yr
# so i think i interpreted this mostly correctly
# and OMG short-term change is 17 mm/yr?????  (see above)

ggplot(dat_all, aes(x = date, y = MSL)) +
        geom_line(col = "blue") +
        labs(title = "Monthly Mean Sea Level at Dauphin Island NWLON Station",
             subtitle = "Station ID 8735180 \ndata from 1966 - 2016") +
        theme_minimal()

# what about CIs
# use std error from lm?
coeffs <- broom::tidy(lm_longterm)
rate_mm_yr <- coeffs[2, 2:3, drop = FALSE] * 365.25 * 1000 
rate_mm_yr %>%
        mutate(ci_low = estimate - 2 * std.error,
               ci_high = estimate + 2 * std.error)


# see if other datums give the same slope
# they're generally close
# i'd feel better though if i could get exactly the same numbers
# that are on Tides and Currents

lm_mlw <- lm(MLW ~ date, data = dat_all)
summary(lm_mlw)
(mlw_mm_yr <- lm_mlw$coefficients[2] * 365.25 * 1000)


lm_mhw <- lm(MHW ~ date, data = dat_all)
summary(lm_mhw)
(mhw_mm_yr <- lm_mhw$coefficients[2] * 365.25 * 1000)


lm_mtl <- lm(MTL ~ date, data = dat_all)
summary(lm_mtl)
(mtl_mm_yr <- lm_mtl$coefficients[2] * 365.25 * 1000)


lm_dtl <- lm(DTL ~ date, data = dat_all)
summary(lm_dtl)
(dtl_mm_yr <- lm_dtl$coefficients[2] * 365.25 * 1000)



# Renee was talking about how the COOPS folks wouldn't use less than 25 years
# for a shorter term change calculation
# let's see how that looks

dat_sub <- dat_all %>%
        filter(date > "1991-01-01")
lm_25yr <- lm(MSL ~ date, data = dat_sub)
summary(lm_25yr)
(rate_mm_yr <- lm_25yr$coefficients[2] * 365.25 * 1000)





## so the most I'll be able to get is:
# 20090101 to 20181231

# make a function with those dates
# spit out a data frame
nerrs_download <- function(stn){
        dat <- rnoaa::coops_search(station_name = stn, 
                            begin_date = 20090101,
                            end_date = 20181231, 
                            datum = "stnd", 
                            product = "monthly_mean",
                            time_zone = "lst")
        
        dat2 <- dat$data %>%
                mutate(date = lubridate::ymd(paste0(year, month, "-01")),
                       stn_id = dat$metadata$id,
                       stn_name = dat$metadata$name)
        
        dat2
}


# make sure that works
ftmeyers <- nerrs_download(8725520)
glimpse(ftmeyers)


# now read in the SLR sheet, pull the station IDs, and loop through them


