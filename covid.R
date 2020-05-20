################################################################################
# R script to use the excellent, regularly updated JHU COVID-19 data and 
# New York Times data on the US and global pandemic of 2020.
# https://github.com/CSSEGISandData/COVID-19.git
# and
# https://github.com/nytimes/covid-19-data
#
# and
# https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
#
# Jesse Hamner, 2020
#
################################################################################

library(httr)
library(gdata)
library(sf)
library(ggplot2)
library(xml2)
library(rvest)
library(colorspace)

homedir <- Sys.getenv('HOME')
setwd(homedir)
setwd('Dropbox/covidRscript')
# Load helper functions for this work:
source('covidfunctions.R')

# Change this value to average the daily infection count growth rate 
# for the last x days over a longer/shorter period:
lookback_days <- 14


################################################################################
# FIPS list of DFW counties & MSAs: 
# https://dshs.texas.gov/chs/info/info_txco.shtm
# Read the Excel file, converted to csv; get counties in the D/FW/Arlington MSA.
# Use the spreadsheet ('Sheet1').
################################################################################

# FIPS list of nationwide MSAs:
msa_list_url <- 'https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html'
msa_list_filename <- 'https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls'
msafips_columns <- c('CBSACode', 'MetropolitanDivisionCode', 'CSACode',
                     'CBSATitle', 'MSAName', 'MetropolitanDivisionTitle',
                     'CSATitle', 'County_or_Equivalent', 'StateName',
                     'stfips', 'cofips', 'Central_or_Outlying_County')
msafips_colclasses = c('CBSA.Code' = 'character',
                       'Metropolitan.Division.Code' = 'character',
                       'CSA.Code' = 'character',
                       'CBSA.Title' = 'character',
                       'Metropolitan.Micropolitan.Statistical.Area' = 'character',
                       'Metropolitan.Division.Title' = 'character',
                       'CSA.Title' = 'character',
                       'County.County.Equivalent' = 'character',
                       'State.Name' = 'character',
                       'FIPS.State.Code' = 'character',
                       'FIPS.County.Code' = 'character',
                       'Central.Outlying.County' = 'character'
                      )

msalist <- get_msa_list(fileurl = msa_list_filename, 
                        col_classes = msafips_colclasses,
                        msafips_columns = msafips_columns)
msaname <- 'New Orleans-Metairie, LA'
nola_fips <- get_metro_fips_2(msalist, msa_name = msaname, varname = 'CBSATitle')


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
# EU / ECDC data:
################################################################################
ecdcdata <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                     na.strings = "",
                     fileEncoding = "UTF-8-BOM")
ecdclabel <- "Source: European Centre for Disease Prevention and Control"


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
lapply(X = statefipscodes[,1],
       FUN = do_state_plots,
       inputjhu = covid3,
       inputnyt = nyt,
       stfipslist = statefipscodes)


# An example of doing one state manually instead of with -lapply-
state <- 'Texas'
stfips <- '48'
state_level <- make_state_data(inputdf = covid3, stfips = stfips)
nyt_state_level <- nyt_state_match(nyt = nyt, stfips = stfips)
plot_daily_increase(state = state,
                    dataset = state_level,
                    lookback_days = lookback_days)
plot_cumulative_cases(state = state,
                      jhu_data = state_level,
                      nyt_data = nyt_state_level)


################################################################################
# Metro plots for DFW (aggregated)
################################################################################

setwd(homedir)
state <- 'DFW Metro'

make_metro_plots(areaname = state,
                 countysubset = dfw_fips,
                 stfips = '48',
                 jhudata = covid3,
                 nytdata = nyt,
                 lookback_days = lookback_days)



# lapply(X = seq(2000,14000,2000), FUN = function(x){dfw_covid$posixdate[which(dfw_covid$Confirmed > x)][1]})

################################################################################
# TODO:
#
# Display basic polygon map
#  - Add date, north arrow, scale, etc.
# Allow for generic MSA pulls by including state and county FIPS in the 
# function arguments for make_metro_subset() and nyt_subset()
#
################################################################################

mapdir <- 'Downloads/GIS Data'
stateshapes <- 'tl_2017_us_state'
uscountymapdir <- 'countyboundaries'
uscountymap <- 'cb_2015_us_county_5m.shp'

setwd(paste(homedir, mapdir, sep='/'))
setwd(uscountymapdir)

uspopdataurl <- 'https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv'

uscountiesmap <- st_read(uscountymap, stringsAsFactors = FALSE)
txcountymap <- uscountiesmap[which(uscountiesmap$STATEFP == '48'),]
dfw_counties_map <- txcountymap[which(as.numeric(txcountymap$COUNTYFP) %in% dfw_fips),]

txcovid3 <- covid3[which(covid3$stfips == '48'),]

covid3_dfw <- txcovid3[which(as.numeric(txcovid3$cofips) %in% dfw_fips),]
covid3_dfw_yesterday <- covid3_dfw[which(covid3_dfw$date == Sys.Date() - 1),]

dfw_counties_map$Confirmed <- 0
dfw_counties_map$Confirmed <- as.numeric(lapply(X = dfw_counties_map$COUNTYFP, 
       FUN = function(x) {
         as.numeric(covid3_dfw_yesterday$Confirmed[which(covid3_dfw_yesterday$cofips == x)])
        }
      ))

dfw_counties_map$Confirmed[which(dfw_counties_map$GEOID == '48425')] <- 0

poplist <- get_texas_population_by_county(2019)
dfw_counties_map$Population <- 0
dfw_counties_map$Population <- as.numeric(lapply(X = as.numeric(dfw_counties_map$COUNTYFP), 
                                     FUN = function(x) {
                                       gsub(',', '', poplist$Total[which(poplist$FIPS == x)])
                                     }
))

dfw_counties_map$percent_infected <- 100 * (dfw_counties_map$Confirmed / dfw_counties_map$Population)
lowcolor <- "#EEEEEE"
highcolor <- "#00002A"

todaytitle <- sprintf("DFW COVID-19 Cases as of %s & Percent of County Population Infected", Sys.Date())

dfw_plot <- ggplot() +
  geom_sf(data = dfw_counties_map, aes(fill = percent_infected)) + 
  scale_fill_gradient(high = highcolor, low = lowcolor) + 
  ggtitle(todaytitle) +
  coord_sf(label_axes = list(bottom = "Longitude", left = "Latitude")
           ) + 
  geom_sf_label(aes(label = dfw_counties_map$Confirmed,
                geometry = dfw_counties_map$geometry
               )
           ) +
  guides(fill = guide_colorbar(title="Percent\nInfected")) + 
    theme(legend.justification=c(0.0, 0.0), legend.position=c(0.89, 0.02)) + 
  labs(x = "Longitude", y = "Latitude") +
  geom_sf_label(data = dfw_counties_map,
               na.rm = TRUE, 
               nudge_y = 0.09,
               mapping = aes(label = NAME,
                             geometry = geometry
                            ),
               color = "gray40",
               fill = "#ffffdd"
              )

setwd(homedir)

# basic_plot(sprintf('dfw_covid_map_%s.png', Sys.Date()), dfw_plot)
png(filename = 'testmap.png',
    bg = "white",
    res = 300,
    units = "in",
    pointsize = 14,
    width = 7,
    height = 6
   )
  dfw_plot
dev.off()




# covid3 -- 'Confirmed' and 'Deaths', clocked by 'date' -- converted 
# ecdcdata -- "cases" and "deaths", clocked by 'dateRep' or 'day', 'month', 'year'

cname <- 'Afghanistan'
cabbr <- 'AFG'

country_plot(
  countryname = cname,
  iso3abbr = cabbr,
  jhudata = covid3,
  ecdcdata = ecdcdata,
  lookback_days = lookback_days,
  sourcename = ecdclabel
)


