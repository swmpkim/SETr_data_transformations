library(rnoaa)
library(tidyverse)
library(lubridate)
library(janitor)


# in_file <- here::here("data", "water_level", "water_lev_dauph_isl.RData")
# load(in_file)



########################################################
# use coops de-seasonalized data

# dauphin island ----

coops_path <- here::here("data", "water_level", "8735180_dauph_isl.csv")
coops_dat_raw <- read_csv(coops_path)

dat <- coops_dat_raw %>%
        clean_names() %>%
        filter(year >= 1966,
               year <= 2016) %>%
        select(year, month, monthly_msl) %>%
        mutate(date = ymd(paste0(year, month, "-01")),
               lag1 = lag(monthly_msl))

# linear regression: monthly msl on date
# this comes out as meters/day
lm_fit <- lm(monthly_msl ~ date, data = dat)
rate_mm_yr <- lm_fit$coefficients[2] * 365.25 * 1000  # DI 3.44
CI_low <- confint(lm_fit)["date", 1] * 365.25 * 1000  # DI 3.088
CI_high <- confint(lm_fit)["date", 2] * 365.25 * 1000 # DI 3.801


# reported change for 1966 - 2016 on the website is 3.50, with CI from 2.88 to 4.12


# see if anything different happens by removing any rows wtih NA for MSL
# (for regression, it should be the same)
dat2 <- dat %>%
        filter(!is.na(monthly_msl))
lm_fit2 <- lm(monthly_msl ~ date, data = dat2)
rate_mm_yr2 <- lm_fit2$coefficients[2] * 365.25 * 1000

coeffs2 <- broom::tidy(lm_fit2)
coeffs_mm_yr2 <- coeffs2[2, 2:3, drop = FALSE] * 365.25 * 1000 
coeffs_mm_yr2 %>%
        mutate(ci_low = estimate - 2 * std.error,
               ci_high = estimate + 2 * std.error)
# phew; that's the same
# will probably matter for arima


# arima
ari_out <- arima(dat$monthly_msl, order = c(1, 0, 0))

ari_out$coef
# this is probably m/d 
# want mm/yr so multiply by 365.25 d/yr and 1000 mm/m
# rough conversion to mm/yr:
ari_out$coef * 365.25 * 1000

# reported change for 1966 - 2016 on the website is 3.50, with CI from 2.88 to 4.12

# switched-up arima
ari_out <- arima(dat$monthly_msl, order = c(1, 0, 0), xreg = dat$date)

time12 <- dat %>% select(year, month)
ari_out3 <- arima(dat$monthly_msl, order = c(1,0,0), xreg = time12)
ari_out3$coef * 365.25 * 1000

ari_out$coef

# rough conversion to mm/yr:
(ari_out_mm_yr <- ari_out$coef * 365.25 * 1000)
ari_out$var.coef  # variance-covariance matrix!!!! want [3,3] for date one
# don't know how to calculate standard error from it though, because what's n???

(sterr <- sqrt(ari_out$var.coef[3,3]) )  # what units are these though

ari_out_mm_yr[3] + c(-2, 2) * sterr


ari_out$coef[2, 3]

broom::tidy(ari_out$coef)

ari_out$coef[3]

#####################################
# beaufort data ----


coops_path <- here::here("data", "water_level", "8656483_beaufort_nc.csv")
coops_dat_raw <- read_csv(coops_path)

dat <- coops_dat_raw %>%
        clean_names() %>%
        filter(year >= 1953,
               year <= 2018) %>%
        select(year, month, monthly_msl) %>%
        mutate(date = ymd(paste0(year, month, "-01")))

# linear regression: monthly msl on date
lm_fit <- lm(monthly_msl ~ date, data = dat)
rate_mm_yr <- lm_fit$coefficients[2] * 365.25 * 1000

coeffs <- broom::tidy(lm_fit)
coeffs_mm_yr <- coeffs[2, 2:3, drop = FALSE] * 365.25 * 1000 
coeffs_mm_yr %>%
        mutate(ci_low = estimate - 2 * std.error,
               ci_high = estimate + 2 * std.error)
# reported change for 1953 - 2018 on the website is for Beaufort is 3.10, with CI from 2.76 to 3.45
# these come out to: estimate = 3.100; CI from 2.862 to 3.338




# arima
ari_out <- arima(dat$monthly_msl, order = c(1, 0, 0), xreg = dat$date)

time123 <- time(dat)
ari_out <- arima(dat$monthly_msl, order = c(1, 0, 0), xreg = time123)

# ari_out <- arima(dat$monthly_msl, order = c(1, 0, 0), xreg = lubridate::decimal_date(dat$date))

ari_out$coef
# this is probably m/... something?
# because of the structure of the data

# rough conversion to mm/yr:
(ari_rate_mm_yr <- ari_out$coef * 365.25 * 1000)
## still 3.106

(ari_var_mm_yr <- ari_out$var.coef[3,3] * 365.25 * 1000)

ari_rate_mm_yr[3] + c(-2, 2)*ari_var_mm_yr

###########################
# wrangling data downloaded through rnoaa

lm_longterm <- lm(MSL ~ date, data = dat_all)
summary(lm_longterm)
(longterm_mm_yr <- lm_longterm$coefficients[2] * 365.25 * 1000)
# reported change for 1966 - 2016 on the website is 3.50, with CI from 2.88 to 4.12
# my calculations came up with 3.548 mm/yr


# so i think i interpreted this mostly correctly


# what about CIs
# use std error from lm?
coeffs <- broom::tidy(lm_longterm)
rate_mm_yr <- coeffs[2, 2:3, drop = FALSE] * 365.25 * 1000 
rate_mm_yr %>%
        mutate(ci_low = estimate - 2 * std.error,
               ci_high = estimate + 2 * std.error)
## 2.989 to 4.107  this is narrower than the COOPS one


# 4/9/19 was just told that COOPS uses ARIMA(1,0,0) for their calculations
ari_out <- arima(dat_all$MSL, order = c(1, 0, 0))
summary(ari_out)

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


