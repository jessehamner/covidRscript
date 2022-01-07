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
library(zoo)
library(readxl)
library(dplyr)

homedir <- Sys.getenv('HOME')
outputdir <- sprintf('%s/%s', homedir, 'covid')
setwd(homedir)
setwd('Dropbox/covidRscript')
# Load helper functions for this work:
source('covidfunctions.R')

isocodes <- get_iso_country_codes()

# Change this value to average the daily infection count growth rate 
# for the last x days over a longer/shorter period:
lookback_days <- 14

start_year = 2020

################################################################################
# FIPS list of DFW counties & MSAs: 
# https://dshs.texas.gov/chs/info/info_txco.shtm
# Read the Excel file, converted to csv; get counties in the D/FW/Arlington MSA.
# Use the spreadsheet ('Sheet1').
################################################################################

# FIPS list of nationwide MSAs:
msa_list_url <- 'https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html'
censusgov <- 'https://www2.census.gov/programs-surveys'
msa_list_filename <- sprintf('%s/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls', censusgov)
uspopdataurl <- sprintf('%s/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv', censusgov)
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

# TODO download this if it isn't local; check for its existence.
uspopdata <- read.csv(uspopdataurl)
currentpop <- 'POPESTIMATE2019'
uspopdata$GEOID <- sprintf('%02g%03g', uspopdata$STATE, uspopdata$COUNTY)

msalist <- get_msa_list(fileurl = msa_list_filename, 
                        col_classes = msafips_colclasses,
                        msafips_columns = msafips_columns)
msaname <- 'New Orleans-Metairie, LA'
nola_fips <- get_metro_fips_2(msalist, msa_name = msaname, varname = 'CBSATitle')

lowcolor <- "#EEEEEE"
highcolor <- "#00002A"

txfipsurl <- 'http://www.dshs.state.tx.us/chs/info/TxCoPhrMsa.xls'

# Check for the file locally so we don't have to go get it every time:


txfips <- get_texas_metro_county_list(fipsurl = txfipsurl, remote=FALSE)
metro_msa_name <- 'Dallas-Fort Worth-Arlington'
dfw_fips <- get_metro_fips(txfips, metro_msa_name)

# IF THIS FUNCTION BARFS: you might have to install Perl, or download the file 
# and create the data frame another way. Download the file and convert it to 
# a CSV directly, then execute this command:
if (length(dfw_fips) == 0) {
  message('Oops -- unable to convert the Texas Metro area FIPS file.')
  converted_fips_file_name <- 'PHR_MSA_County_masterlist.csv'
  dfw_fips <- get_metro_fips_locally(fipsdir = paste(homedir, 'Dropbox', sep = '/'), 
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

dropstates <- c(60, 66, 69, 74, 78)
`%notin%` <- Negate(`%in%`)
statefipscodes <- statefipscodes[which(statefipscodes$STATE %notin% dropstates),]

################################################################################
# Johns Hopkins's data have changed format a few times.
################################################################################

# After 11/08/2020:
header5 <- c('FIPS', 'Admin2', 'Province_State', 'Country_Region', 'Last_Update',
             'Lat', 'Long_', 'Confirmed', 'Deaths', 'Recovered', 'Active',
             'Combined_Key', 'Incidence_Rate', 'Case_Fatality_Ratio'
)

# After 5/29/2020:
header4 <- c('FIPS', 'Admin2', 'Province_State', 'Country_Region', 'Last_Update',
             'Lat', 'Long_', 'Confirmed', 'Deaths', 'Recovered', 'Active',
             'Combined_Key', 'Incidence_Rate', 'Case.Fatality_Ratio'
)

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

# FIPS,Admin2,Province_State,Country_Region,Last_Update,Lat,Long_,Confirmed,Deaths,
# Recovered,Active,Combined_Key,Incidence_Rate,Case-Fatality_Ratio
col_classes4 <- c('FIPS' = 'character',
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
                  'Combined_Key' = 'character',
                  'Incidence_Rate' = 'numeric',
                  'Case.Fatality_Ratio' = 'numeric')


col_classes5 <- c('FIPS' = 'character',
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
                  'Combined_Key' = 'character',
                  'Incident_Rate' = 'numeric',
                  'Case_Fatality_Ratio' = 'numeric')

what5 <- c('character', 'character', 'factor', 'character', 'character', 'double',
           'double', 'numeric', 'numeric', 'numeric', 'numeric', 'character',
           'numeric', 'numeric')

################################################################################
# Update and import the Johns Hopkins COVID-19 data:
################################################################################

repo <- 'COVID-19'
datadir <- 'csse_covid_19_data/csse_covid_19_daily_reports/'
covid <- data.frame()

# Get list of daily data files with file pattern like: "04-02-2020.csv"
setwd(paste(homedir, repo, sep = "/"))
system('git config pull.rebase false')
system('git pull')

setwd(datadir)

files_2020 <- dir(pattern = "\\d{2}-\\d{2}-2020\\.csv")
files_2021 <- dir(pattern = "\\d{2}-\\d{2}-2021\\.csv")
files_2022 <- dir(pattern = "\\d{2}-\\d{2}-2022\\.csv")
files <- c(files_2020, files_2021, files_2022)
# the file format changed on 03.01.2020, 03.22.2020, 05.27.2020, and 11.09.2020:
# Files get sorted alphabetically, meaning Jan 2020 and Jan 2021 get conflated.
breakpoint1 <- which(files == "02-29-2020.csv")
breakpoint2 <- which(files == "03-21-2020.csv")
breakpoint3 <- which(files == "05-28-2020.csv")
breakpoint4 <- which(files == "11-08-2020.csv")

files1 <- files[1:breakpoint1]
files2 <- files[(breakpoint1 + 1):breakpoint2]
files3 <- files[(breakpoint2 + 1):breakpoint3]
files4 <- files[(breakpoint3 + 1):breakpoint4]
files5 <- files[(breakpoint4 + 1):length(files)]

# TODO save these files locally and just check to see if they exist instead
# of re-importing ALL the files every time.
# only covid5 would need to be imported "fresh" each time, and even then
# I could chunk the data into local files by months or something.
covid1 <- import_covid_subset(fileslist = files1, col_classes = col_classes1)
covid2 <- import_covid_subset(fileslist = files2, col_classes = col_classes2)
covid3 <- import_covid_subset(fileslist = files3, col_classes = col_classes3)
covid4 <- import_covid_subset(fileslist = files4, col_classes = col_classes4)

covid5 <- import_covid_subset(fileslist = files5, col_classes = col_classes5)

covid4$newfips <- sprintf("%05.0f", as.integer(covid4$FIPS))
covid4$stfips <- substr(covid4$newfips, 1,2)
covid4$cofips <- substr(covid4$newfips, 3,5)

covid5$newfips <- sprintf("%05.0f", as.integer(covid5$FIPS))
covid5$stfips <- substr(covid5$newfips, 1,2)
covid5$cofips <- substr(covid5$newfips, 3,5)


# Set up the first two formats for a merge:
covid1$Longitude <- as.double(0.0)
covid1$Latitude <- as.double(0.0)
covid1 <- rbind(covid1, covid2)

# The third format is a lot different.
covid3$Incidence_Rate <- 0.0
covid3$Case.Fatality_Ratio <- 0.0

covid3$newfips <- sprintf("%05.0f", as.integer(covid3$FIPS))
covid3$stfips <- substr(covid3$newfips, 1,2)
covid3$cofips <- substr(covid3$newfips, 3,5)

names(covid5) <- names(covid4)
covid3 <- rbind(covid3, covid4, covid5)

# Fix a mammoth typo (three orders of magnitude in Okaloosa Co., FL):
covid3$Active[which(covid3$date == "2020-04-13" & covid3$FIPS == "12091")] <- 102


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
system('git config pull.rebase false')
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
# New data location (weekly, not daily): https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv
ecdcdata <- read.csv("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv",
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

setwd(outputdir)
lapply(X = statefipscodes[,1],
       FUN = do_state_plots,
       inputjhu = covid3,
       inputnyt = nyt,
       stfipslist = statefipscodes)


# An example of doing one state manually instead of with -lapply-
# state <- 'Texas'
# stfips <- '48'
# state_level <- make_state_data(inputdf = covid3, stfips = stfips)
# nyt_state_level <- nyt_state_match(nyt = nyt, stfips = stfips)
# plot_daily_increase(state = state,
#                    dataset = state_level,
#                    lookback_days = lookback_days)
# plot_cumulative_cases(state = state,
#                      jhu_data = state_level,
#                      nyt_data = nyt_state_level)


################################################################################
# Metro plots for MSAs (aggregated)
################################################################################

msa_plot_list <- c('New Orleans-Metairie, LA',
                   'Minneapolis-St. Paul-Bloomington, MN-WI',
                   'Chicago-Naperville-Elgin, IL-IN-WI',
                   'Huntsville, AL',
                   'Atlanta-Sandy Springs-Alpharetta, GA',
                   'Dallas-Fort Worth-Arlington, TX',
                   'San Antonio-New Braunfels, TX',
                   'Houston-The Woodlands-Sugar Land, TX',
                   'Austin-Round Rock-Georgetown, TX',
                   'Sulphur Springs, TX',
                   'San Diego-Chula Vista-Carlsbad, CA')

setwd(outputdir)
texas_metros <- c()
state <- 'DFW Metro'


make_metro_plots(areaname = state,
                 countysubset = dfw_fips,
                 stfips = '48',
                 jhudata = covid3,
                 nytdata = nyt,
                 lookback_days = lookback_days)


# How to make a single-county plot:
state <- 'Hopkins County'
hopkinsfips <- msalist[which(msalist$CBSATitle=='Sulphur Springs, TX'),]
hopkinsfips$cofips <- 223

make_metro_plots(areaname = state,
                 countysubset = hopkinsfips,
                 stfips = '48',
                 jhudata = covid3,
                 nytdata = nyt,
                 lookback_days = lookback_days)


################################################################################
#
# Display basic polygon map
#  TODO:  Add date, north arrow, scale, etc.
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


uscountiesmap <- st_read(uscountymap, stringsAsFactors = FALSE)
uscountiesmap$Population <- 0
for (i in seq(1, nrow(uscountiesmap))) {
  matcher <- which(uspopdata$GEOID == uscountiesmap$GEOID[i])
  if (length(matcher) != 0) {
    uscountiesmap$Population[i] <- uspopdata$POPESTIMATE2019[matcher]
  }
}



#metro_fips$newfips <- sprintf('%s%s', metro_fips$stfips, metro_fips$cofips)
setwd(paste(homedir, mapdir, sep='/'))

msaname <- 'New York-Newark-Jersey City, NY-NJ-PA'
metro_fips <- get_metro_fips_2(msalist, msa_name = msaname, varname = 'CBSATitle')
metrocountymap <- make_metro_map(countiesmap = uscountiesmap,
                                 msa_name = msaname,
                                 msalist = msalist
                                )

metro_covid3 <- covid3[unlist(lapply(X = seq(1, nrow(metro_fips)),
                                     FUN = function(x) { which(covid3$newfips == metro_fips$newfips[x]) }
                                    )
                             )
                      ,]

metro_covid3 <- do_posixtime(metro_covid3) 
covid3_metro_yesterday <- metro_covid3[which(metro_covid3$date == as.Date(max(metro_covid3$posixtime)) - 1),]
metrocountymap$Confirmed <- 0

for (x in seq(1, nrow(covid3_metro_yesterday))) {
    replacement <- covid3_metro_yesterday$Confirmed[which(covid3_metro_yesterday$newfips == metrocountymap$GEOID[x])]
    message(sprintf('X: %s\t FIPS: %s\treplacement value: %s', x, covid3_metro_yesterday$newfips[x], replacement))
    if (length(replacement) > 0) {
      metrocountymap$Confirmed[x] <- replacement
    }
}

metrocountymap$percent_infected <- 100 * (metrocountymap$Confirmed / metrocountymap$Population)


#metrocountymap <- metrocountymap %>% 
#  dplyr::mutate(newcases_07da = zoo::rollmean(new_today, k=7, fill=NA)) %>% 
#  dplyr::ungroup()
#metrocountymap <- metrocountymap[which(!is.na(metrocountymap$newcases_07da)),]


todaytitle <- sprintf("%s COVID-19 Cases as of %s", msaname, Sys.Date())
todaysubtitle <- sprintf("and Percent of County Population Infected")

metro_plot <- metro_map_plot(metrocountymap, todaytitle, todaysubtitle)

todaymapfilename <- sprintf("%s_covid19_metromap_%s.png", gsub(' ', '', gsub('-|,', '', msaname)), Sys.Date())
setwd(outputdir)
ggsave(todaymapfilename, plot=metro_plot)

# Graphs of infections, deaths, and rates:
#make_complete_metro_plot(msalist = msalist,
#                         msa_name = msaname,
#                         uscountiesmap = uscountiesmap,
#                         covid_data = covid3,
#                         dest_dir = homedir
#                        )

for(i in seq(1:length(msa_plot_list))) {
  make_complete_metro_plot(msalist = msalist,
                           msa_name = msa_plot_list[i],
                           uscountiesmap = uscountiesmap,
                           covid_data = covid3,
                           dest_dir = outputdir)
}


txcountymap <- uscountiesmap[which(uscountiesmap$STATEFP == '48'),]
dfw_counties_map <- txcountymap[which(txcountymap$COUNTYFP %in% dfw_fips$cofips),]
txcovid3 <- covid3[which(covid3$stfips == '48'),]

# covid3_dfw <- make_metro_subset(inputdf = covid3, cofipslist = dfw_fips)
covid3_dfw <- txcovid3[which(txcovid3$cofips %in% dfw_fips$cofips),]
covid3_dfw_yesterday <- covid3_dfw[which(covid3_dfw$date == Sys.Date() - 1),]

dfw_counties_map$Confirmed <- 0
dfw_counties_map$Confirmed <- as.numeric(lapply(X = dfw_counties_map$GEOID, 
       FUN = function(x) {
         as.numeric(covid3_dfw_yesterday$Confirmed[which(covid3_dfw_yesterday$FIPS == x)])
        }
      ))

dfw_counties_map$Confirmed[which(is.na(dfw_counties_map$Confirmed))] <- 0

# poplist uses NUMERIC FIPS codes for counties. The rest of the data frames should not.
poplist <- get_us_population_by_county(2019)


dfw_counties_map$Population <- 0
dfw_counties_map$Population <- as.numeric(lapply(X = as.numeric(dfw_counties_map$GEOID), 
                                                 FUN = function(x) {
                                                                    gsub(',', '', poplist$POPESTIMATE2019[which(poplist$newfips == x)])
                                                                   }
                                                )
                                         )

dfw_counties_map$percent_infected <- 100 * (dfw_counties_map$Confirmed / dfw_counties_map$Population)

todaytitle <- sprintf("%s COVID-19 Cases as of %s", metro_msa_name, Sys.Date())
todaysubtitle <- sprintf("and Percent of County Population Infected")

dfw_plot <- ggplot() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)
       ) +
  geom_sf(data = dfw_counties_map,
          aes(fill = percent_infected)
         ) + 
  scale_fill_gradient(high = highcolor,
                      low = lowcolor
                     ) + 
  ggtitle(label = todaytitle,
          subtitle = todaysubtitle
         ) +
  coord_sf(label_axes = list(bottom = "Longitude",
                             left = "Latitude")
          ) + 
  geom_sf_label(data = dfw_counties_map,
                aes(label = Confirmed,
                    geometry = geometry
                   )
               ) +
  guides(fill = guide_colorbar(title="Percent\nInfected")) + 
    theme(legend.justification=c(0.0, 0.0),
          legend.position=c(0.89, 0.02)
         ) + 
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


mapname <- sprintf("%s_map.png", gsub('-', '', gsub(' |,', '', metro_msa_name)))
# basic_plot(sprintf('dfw_covid_map_%s.png', Sys.Date()), dfw_plot)
png(filename = mapname,
    bg = "white",
    res = 300,
    units = "in",
    pointsize = 14,
    width = 7,
    height = 6
   )
  dfw_plot
dev.off()


# Make a county-level plot by state, using, say, txcountymap:
state_code <- '48'
state_map <- uscountiesmap[which(uscountiesmap$STATEFP == state_code),]
covid3_state <- txcovid3[which(txcovid3$stfips == state_code),]
covid3_state_counties <- unique(covid3_state$cofips)
state_map$POPESTIMATE2019 <- 0
state_map$POPESTIMATE2019 <- unlist(lapply(X=state_map$GEOID,
                                           FUN = function(x){poplist$POPESTIMATE2019[which(poplist$newfips == x)]}))


# sum cases over unique counties (no dates)
# sum new cases *today* over unique counties (only today's date)
# 



# covid3 -- 'Confirmed' and 'Deaths', clocked by 'date' -- converted 
# ecdcdata -- "cases" and "deaths", clocked by 'dateRep' or 'day', 'month', 'year'



countries <- c('DEU', 'FRA')
#cabbr <- 'AFG'

for(i in seq(1,length(countries))) {
  cabbr = countries[i]
  country_plot(
    countryname = isocodes$officialname[which(isocodes$alpha3 == cabbr)],
    iso3abbr = cabbr,
    jhudata = covid3,
    ecdcdata = ecdcdata,
    lookback_days = lookback_days,
    sourcename = ecdclabel
  )
}

# Export data for one county:
california <- covid3[which(covid3$Country_Region == "US" & covid3$Province_State == "California"),]
sandiego <- california[which(california$Admin2 == "San Diego"),]
sandiego$newcases <- 0
sandiego$newcases[2:nrow(sandiego)] <- unlist(sapply(X=seq(1,nrow(sandiego)), 
                                                     FUN = function(x) {sandiego$Confirmed[x] - sandiego$Confirmed[x-1]}))
write.csv(x=sandiego,
          file="san_diego_california_usa.csv", 
          quote = TRUE,
          row.names = FALSE, 
          eol="\n"
         )


okaloosa <- covid3[which(covid3$FIPS==12091),]
write.csv(x=okaloosa, file="okaloosa.csv")
nebraska <- covid3[which(covid3$Province_State == "Nebraska"),]
write.csv(x=nebraska, file="nebraska.csv")

