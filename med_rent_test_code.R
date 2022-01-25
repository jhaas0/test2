#Script for Assessing Rental Markets and Evictions 
#Preliminary analysis with Craigslist Data (Tampa, FL; Houston, TX; Columbus, OH)


#####PREP####

setwd("/projects/DESMOND/data_nb/Jasmine/Markets")

library(tidyverse)
library(here)
library(readr)
library(lubridate)
library(readxl)


# LOADING AND COMBINING DATA ------------------------------------------------------------
OH <- read_csv("DATA/OH_CL_geocodio_c67b7ad1c64a30bee599eafea159b42063b89ac9.csv")
FL <- read_csv("DATA/FL_CL_geocodio_2ff734843060bd5dbc0715ed4f8516cbab64d82e.csv")
TX <- read_csv("DATA/TX_CL_geocodio_612a5268f26cf2f84e5aa927a7c4cd7f56f1ce31.csv")

zip_county_xwalk <- readxl::read_excel("/projects/DESMOND/data_nb/Jacob/Eviction Tracker/DATA/ZIP_COUNTY_092021.xlsx", 
                                       guess_max = 1000)

# COMBINE DATA ------------------------------------------------------------
#combining the separated data into one large geocoded dataset 
CL_dta <- rbind(OH, FL, TX)


# GET MONTH LISTINGS  -----------------------------------------------------
#creating a new column for the year and month that a new post was made 
CL_dta <- CL_dta %>% 
  mutate(year_posted = year(date), 
         month_posted = month(date))


# CLEANING ---------------------------

#fix the column for asking price into numeric from character 
CL_dta <- CL_dta %>% 
  mutate(asking_price = parse_number(CL_dta$price))

#removing any outliers there might be in the prices 
#removing asking prices above 20 Mil and below $150
CL_dta.c1 <- CL_dta %>% 
  filter(asking_price > 150) %>% 
  filter(asking_price < 100000)

#take out the random states that aren't OH, TX and FL 
CL_dta.c <- CL_dta.c1 %>% 
  filter(State == "TX"
         | State == "FL"
         | State == "OH") 

#lost around 15K observations because there are about 15K that are missing lat/long information or are listed in different states- 
#not sure what the bypass can be there to incorporate these listings

#######PLACE HOLDER FOR FURTHER CLEANING IF NECESSARY######








#aggregate median rent price for all the postings in each year's month by State and Zip 
CL_dta.c %>% 
  group_by(State, Zip, year_posted, month_posted) %>% 
  summarise(median_ask_rent = median(asking_price)) %>% 
  ungroup() -> CL.dta 

#rename a few things for the later merge to the eviction data 
CL.dta <- rename(CL.dta, xfilemonth = month_posted) 
CL.dta <- rename(CL.dta, ZIP = Zip)
CL.dta$ZIP = as.character(CL.dta$ZIP)

#grab only the last 6 months of postings 
CL_six_month.dta <- CL.dta %>% 
  filter(xfilemonth >=4)

# EVICTION RECORDS --------------------------------------------------------


#set dates for further down in the code 
end_date <- "2021-09-30"
end_month <- month(end_date)
end_day <- day(end_date)

#Load the cleaned current and historical data 
in.dir <- here("/projects/DESMOND/data_nb/Renee/Eviction Tracker/OVERHAUL/CLEANED DATA/CURRENT")
hist.in.dir <- here("/projects/DESMOND/data_nb/Renee/Eviction Tracker/OVERHAUL/CLEANED DATA/HISTORICAL")

#codebook
load(file = here("/projects/DESMOND/data_nb/Renee/Eviction Tracker/OVERHAUL/SYSTEM WORKING FILES/ets_codebook_check.rda"))

#source month-year df 
source(file = here("/projects/DESMOND/data_nb/Jasmine/Markets/CODE/get_month_years.R"))

#source scripts 
source(file = here("/projects/DESMOND/data_nb/Renee/Eviction Tracker/OVERHAUL/SYSTEM SCRIPTS/cleaning_aggregating_functions.R"))

#minus Wisconsin data   
list.files(in.dir)[str_detect(list.files(in.dir), "wisconsin", negate = T)] -> file.names
list.files(hist.in.dir)[str_detect(list.files(hist.in.dir), "wisconsin|stlouis_historical|Archive", negate = T)] -> hist.file.names 

#load it in!
all_current <- NULL
paste(in.dir,
      file.names,
      sep = "/") %>% 
  map(readRDS) %>% 
  map(match_class_df,
      basic_df) %>% 
  map(select,
      1:nrow(mandatory_columns_codebook)) %>%
  bind_rows() %>%
  bind_rows(all_current) -> all_current

#create a column for ZIP 
all_current <- all_current %>% 
  mutate(ZIP = str_extract(xdefendant_csz, '\\d{5}'))

#analytic sample - filtered to include non-duplicates, ONLY HOUSTON, TAMPA AND COLUMBUS data between Jan 2020 and Sept. 2021 
all_current %>% 
  filter(include == T) %>% 
  filter(xsite %in% c("HOUSTON", "TAMPA", "COLUMBUS")) %>%
  filter(xfiledate >= "2020-01-01",
         xfiledate <= end_date)  %>% 
  filter(sealed == F, 
         !is.na(ZIP))-> all_current_samp

#get into last 6 months too 
lastsix <- all_current_samp %>% 
  filter(xfileyear == 2021, 
         xfilemonth >=4)

#get data into table counting the number of filings for each zip code for each week of the month for each year 
current_site_samp <- all_current_samp %>%
  group_by(xsite, ZIP, xfileyear, xfilemonth) %>% 
  summarize(current_num_filings = n()) %>%
  ungroup() %>% 
  complete(nesting(xsite, ZIP), xfileyear, xfilemonth, 
           fill = list(current_num_filings = 0)) 

#join recent years (for current dataset) dataframe to see which months in which years are missing data. 
left_join(current_site_samp, recent_years, by = c("xfileyear")) ->test1

#get last 6 months data into table counting number of filings for each zip for each week of the last six months of 2021 
current_last_six_samp <- lastsix %>%
  group_by(xsite, ZIP, xfileyear, xfilemonth) %>% 
  summarize(current_num_filings = n()) %>%
  ungroup() %>% 
  complete(nesting(xsite, ZIP), xfileyear, xfilemonth, 
           fill = list(current_num_filings = 0)) 

#get the historical data 
all_historical <- NULL

all_historical <- paste(hist.in.dir,
                        hist.file.names,
                        sep = "/") %>% 
  map(readRDS) %>%
  map(match_class_df,
      basic_df) %>% 
  map(select,
      1:nrow(mandatory_columns_codebook)) %>%
  bind_rows(.id = 'xgroup') %>% 
  bind_rows(all_historical)

#restricting it to only include only analytic sites, and for the whole year before sept. 
all_historical_samp <- all_historical %>% 
  filter(include == T, 
         sealed == F) %>% 
  filter(xsite %in% c("HOUSTON", "TAMPA", "COLUMBUS")) #total 116935 observations 

#create a column for ZIP 
all_historical_samp %>% 
  mutate(ZIP = str_extract(all_historical_samp$xdefendant_csz, '\\d{5}')) -> all_historical_sample 

#drop edge zips 


#grab last 6 months of historical data 
lastsixhist <- all_historical_sample %>% 
  filter(xfilemonth >=4 & xfilemonth <=9) 

#get data into table counting the number of filings for each zip code for each month 

zip_monthly_historical <- all_historical_sample %>%
  group_by(xsite, ZIP, xfileyear, xfilemonth) %>% 
  summarize(num_filings_hist = n()) %>% 
  ungroup() %>% 
  complete(nesting(xsite, ZIP, xfileyear), xfilemonth = 1:12, 
           fill = list(num_filings_hist = 0)) %>% 
  group_by(xsite, ZIP, xfilemonth) %>%
  summarize(monthly_hist_filings_avg = mean(num_filings_hist)) %>%
  ungroup()

#get for the last 6 months 
last_six_zip_monthly_historical <- lastsixhist %>%
  group_by(xsite, ZIP, xfileyear, xfilemonth) %>% 
  summarize(num_filings_hist = n()) %>% 
  ungroup() %>% 
  complete(nesting(xsite, ZIP, xfileyear), xfilemonth = 4:9,
           fill = list(num_filings_hist = 0)) %>% 
  group_by(xsite, ZIP, xfilemonth) %>%
  summarize(monthly_hist_filings_avg = mean(num_filings_hist)) %>% 
  ungroup()

#if there are any NAs then our complete() call didn't do its job
zip_monthly_historical %>% filter(is.na(monthly_hist_filings_avg)) %>% nrow()
last_six_zip_monthly_historical %>% filter(is.na(monthly_hist_filings_avg)) %>% nrow()

# Compiling into final tables ---------------------------------------------
# Filter down zips into the ones we want, create a file with each combo of zip/county/site/month/year
# Harris: 48201; Galveston: 48167; Hillsborough: 12057; Pinellas: 12103, Franklin: 39049
# Because some zips may span both pinellas and hillsborough, we sum up the totals between them before filtering.
# So if 45% of a zip is in Harris and 55% is in Galveston, we include that.
zips_we_want <- zip_county_xwalk %>% 
  filter(COUNTY %in% c("48201", "48167", "12057", "12103", "39049")) %>% 
  group_by(ZIP, USPS_ZIP_PREF_STATE) %>% 
  summarize(total = sum(TOT_RATIO, na.rm = T)) %>% 
  ungroup() %>% 
  filter(total >= 0.20) %>% 
  mutate(xsite = case_when(str_detect(ZIP, "^3") ~ "TAMPA", 
                           str_detect(ZIP, "^7") ~ "HOUSTON", 
                           str_detect(ZIP, "^4") ~ "COLUMBUS")) 

#get a data frame of each combo of zip/month/year that we want. 
zip_month_year <- zips_we_want %>% 
  select(-total, -USPS_ZIP_PREF_STATE) %>% 
  left_join(expand.grid(ZIP = unique(zips_we_want$ZIP),
                        xfileyear = 2020:2021, xfilemonth = 1:12), by = "ZIP") %>% 
  filter(!(xfileyear == 2021 & xfilemonth >= 10)) %>% 
  arrange(ZIP, xfileyear, xfilemonth)
  

#FULL EVICTION DATASET MERGE 
eviction.dta.current <- left_join(zip_month_year, current_site_samp,  
          by = c("ZIP", "xsite", "xfileyear", "xfilemonth"))

eviction.dta <- eviction.dta.current %>% 
  left_join(zip_monthly_historical, 
            by = c("ZIP", "xsite", "xfilemonth")) %>% 
  mutate(current_num_filings = replace_na(current_num_filings, 0), 
         monthly_hist_filings_avg = replace_na(monthly_hist_filings_avg, 0))

left_join(current_last_six_samp, last_six_zip_monthly_historical, 
          by = c("ZIP", "xsite", "xfilemonth")) -> sixmonth_eviction.dta

#FULL FINAL DATASET WITH CL data 

left_join(eviction.dta, CL.dta,
          by = c("ZIP", "xfilemonth")) -> FULL_CL_EVICTIONS.dta 

left_join(sixmonth_eviction.dta, CL_six_month.dta, 
          by = c("ZIP", "xfilemonth")) -> SIX_MONTHS_CL_EVICTIONS.dta 

# export -------------------------------------------------------------------

write_csv(FULL_CL_EVICTIONS.dta, here("OUTPUT", "CL_eviction_data.csv"))
write_csv(SIX_MONTHS_CL_EVICTIONS.dta, here("OUTPUT", "sixmonth_CL_eviction_data.csv"))

