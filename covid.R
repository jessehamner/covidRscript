################################################################################
# R script to use the excellent, regularly updated JHU COVID-19 data and 
# New York Times data on the US and global pandemic of 2020.
# https://github.com/CSSEGISandData/COVID-19.git
# and
# https://github.com/nytimes/covid-19-data
#
# Jesse Hamner, 2020
#
################################################################################

library(httr)
library(gdata)

homedir <- Sys.getenv('HOME')
setwd('Dropbox/covidRscript')
# Load helper functions for this work:
source('covidfunctions.R')

# Change this value to average the daily infection count growth rate 
# for the last x days over a longer/shorter period:
lookback_days <- 10


################################################################################
# FIPS list of DFW counties & MSAs: 
# https://dshs.texas.gov/chs/info/info_txco.shtm
# Read the Excel file, converted to csv; get counties in the D/FW/Arlington MSA.
# Use the spreadsheet ('Sheet1').
################################################################################

txfipsurl <- 'http://www.dshs.state.tx.us/chs/info/TxCoPhrMsa.xls'
txfips <- get_texas_metro_county_list(fipsurl = txfipsurl)
metro_msa_name <- 'Dallas-Fort Worth-Arlington'
dfw_fips <- get_metro_fips(txfips, metro_msa_name)

# IF THIS FUNCTION BARFS: you might have to install Perl, or download the file 
# and create the data frame another way. Download the file and convert it to 
# a CSV directly, then execute this command:
if (length(dfw_fips) == 0) {
  message('Oops -- unable to convert the Texas Metro area FIPS file.')
  converted_fips_file_name <- 'PHR_MSA_County_masterlist.csv'
  dfw_fips <- get_metro_fips_locally(fipsdir = paste(Sys.getenv('HOME'), 'Dropbox', sep = '/'), 
                                     msa_name = metro_msa_name,
                                     fipsfilename = converted_fips_file_name
                                    ) 
  }


################################################################################
# State-level FIPS codes sourced from a US Census pipe-delimited file: 
# https://www2.census.gov/geo/docs/reference/state.txt?# 
################################################################################

fipscolclasses <- c("character", "character", "character", "character")
fipsurl <- 'https://www2.census.gov/geo/docs/reference/state.txt?#'
statefipscodes <- import_fips_codes(pageurl = fipsurl, 
                                    fipscolclasses = fipscolclasses
                                   )

################################################################################
# Johns Hopkins's data have changed format a few times.
################################################################################

# After 3/21/2020:
header3 <- c('FIPS', 'Admin2', 'Province_State', 'Country_Region', 'Last_Update',
             'Lat', 'Long_', 'Confirmed', 'Deaths', 'Recovered', 'Active',
             'Combined_Key'
            )

# on or before 3/21/2020 but after 02/29/2020:
header2 <- c('Province/State', 'Country/Region', 'Last Update', 'Confirmed', 
             'Deaths', 'Recovered', 'Latitude', 'Longitude'
            )

# 02/29/2020 and before:
header1 <- c('Province/State', 'Country/Region', 'Last Update', 'Confirmed', 
             'Deaths', 'Recovered'
            )

# datetime format: "2020-02-29T12:13:10"
# Province/State,Country/Region,Last Update,Confirmed,Deaths,Recovered
col_classes1 <- c('Province.State' = 'factor',
                  'Country.Region' = 'character',
                  'Last.Update' = 'character',
                  'Confirmed' = 'numeric',
                  'Deaths' = 'numeric',
                  'Recovered' = 'numeric')

col_classes2 <- c('Province.State' = 'factor',
                  'Country.Region' = 'character',
                  'Last.Update' = 'character',
                  'Confirmed' = 'numeric',
                  'Deaths' = 'numeric',
                  'Recovered' = 'numeric',
                  'Latitude' = 'double',
                  'Longitude' = 'double')

col_classes3 <- c('FIPS' = 'character',
                  'Admin2' = 'character',
                  'Province_State' = 'factor',
                  'Country_Region' = 'character',
                  'Last_Update' = 'character',
                  'Lat' = 'double',
                  'Long_' = 'double',
                  'Confirmed' = 'numeric',
                  'Deaths' = 'numeric',
                  'Recovered' = 'numeric',
                  'Active' = 'numeric',
                  'Combined_Key' = 'character')


################################################################################
# Update and import the Johns Hopkins COVID-19 data:
################################################################################

repo <- 'COVID-19'
datadir <- 'csse_covid_19_data/csse_covid_19_daily_reports/'
covid <- data.frame()

# Get list of daily data files with file pattern like: "04-02-2020.csv"
setwd(paste(homedir, repo, sep = "/"))
system('git pull')

setwd(datadir)
files <- dir(pattern = "\\d{2}-\\d{2}-\\d{4}\\.csv")

# the file format changed on 03.01.2020 and 03.22.2020:
breakpoint1 <- which(files == "02-29-2020.csv")
breakpoint2 <- which(files == "03-21-2020.csv")

files1 <- files[1:breakpoint1]
files2 <- files[(breakpoint1 + 1):breakpoint2]
files3 <- files[(breakpoint2 + 1):length(files)]

covid1 <- import_covid_subset(fileslist = files1, col_classes = col_classes1)
covid2 <- import_covid_subset(fileslist = files2, col_classes = col_classes2)
covid3 <- import_covid_subset(fileslist = files3, col_classes = col_classes3)

# Set up the first two formats for a merge:
covid1$Longitude <- as.double(0.0)
covid1$Latitude <- as.double(0.0)
covid1 <- rbind(covid1, covid2)

# The third format is a lot different.
covid3$newfips <- sprintf("%05.0f", as.integer(covid3$FIPS))
covid3$stfips <- substr(covid3$newfips, 1,2)
covid3$cofips <- substr(covid3$newfips, 3,5)

################################################################################
# New York Times US county-level data:
################################################################################

nyt_repo <- 'nytimescovid'
nyt_header <- c('date','county','state','fips','cases','deaths')
nyt_col_classes <- c('date' = 'character',
                     'county' = 'character',
                     'state' = 'character',
                     'fips' = 'character',
                     'cases' = 'numeric',
                     'deaths' = 'numeric')

setwd(paste(homedir, nyt_repo, sep = '/'))
system('git pull')
nyt <- read.csv('us-counties.csv',
                header = TRUE, 
                stringsAsFactors = FALSE,
                colClasses = nyt_col_classes
)
nyt$posixdate <- as.Date(nyt$date, format = "%Y-%m-%d")


################################################################################
# Make state-level data from imported data, and plot them.
# 
# The next code block makes two plots for each data set for each state:
#  - Daily increase per day, with 2 simple linear regressions
#  - Sum of cases for location for each day, cumulative count (minus deaths)
#    * includes cumulative deaths count as a separate filled polygon
#
################################################################################

setwd(homedir)
lapply(X=statefipscodes[,1],
       FUN=do_state_plots,
       inputjhu = covid3,
       inputnyt = nyt,
       stfipslist = statefipscodes)


# An example of doing one state manually instead of with -lapply-
state <- 'Texas'
stfips <- '48'
state_level <- make_state_data(inputdf = covid3, stfips = stfips)
nyt_state_level <- nyt_state_match(nyt = nyt, stfips = stfips)
plot_daily_increase(state = state, dataset = state_level)
plot_cumulative_cases(state = state,
                      jhu_data = state_level,
                      nyt_data = nyt_state_level)


################################################################################
# Metro plots for DFW (aggregated)
################################################################################
state <- 'DFW Metro'
dfw_covid <- make_metro_subset(inputdf = covid3, 
                               stfips = '48',
                               cofipslist = dfw_fips
                              )
nyt_dfw <- nyt_subset(nytdata = nyt,
                      stfips = '48',
                      countysubset = dfw_fips
                     )
plot_daily_increase(state = state,
                    dataset = dfw_covid
                   )
plot_cumulative_cases(state = state,
                      jhu_data = dfw_covid,
                      nyt_data = nyt_dfw
                     )


################################################################################
# TODO:
#
# Maps of infections, by county until something better is available publicly
#  - It appears that some data are available by zip code?
# Set up map shapefile or table in PostGIS or R
# Set up county- or zipcode- level identifiers for each polygon, if needed
# Copy/merge/cbind the daily info from JHU CSSE for each polygon
#   and/or NYT, TX DPS/DPH, etc.
# Determine county-level rate of change over last x days
# Display basic polygon map
#  - Add date, north arrow, scale, etc.
#
#
################################################################################
