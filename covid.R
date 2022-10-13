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
# Jesse Hamner, 2020-2022
#
# Note that Perl must be installed and accessible to R.
# Especially the Spreadsheet::ParseXLSX module is expected.
# You should first install CPAN::DistnameInfo, XML::XPath, 
#   IO::Socket::SSL or Net::SSL as well
#
#
# Test::Pod 
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

message(sprintf('As of today, there are %s days of data available.',
                Sys.Date() - as.Date(strftime('2020-03-22', format="%Y-%m-%d")))
       )

homedir <- Sys.getenv('HOME')
outputdir <- sprintf('%s/%s', homedir, 'covid')
setwd(homedir)
setwd('covidRscript')
# Load helper functions for this work:
source('covidfunctions.R')

# Change this value to average the daily infection count growth rate 
# for the last x days over a longer/shorter period:
lookback_days <- 14
start_year = 2020
fn_stub <- 'r_covid_'

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

################################################################################
# FIPS list of DFW counties & MSAs: 
# https://dshs.texas.gov/chs/info/info_txco.shtm
# Read the Excel file, converted to csv; get counties in the D/FW/Arlington MSA.
# Use the spreadsheet ('Sheet1').
################################################################################

# FIPS list of nationwide MSAs:

msafips_columns <- c('CBSACode', 'MetropolitanDivisionCode', 'CSACode',
                     'CBSATitle', 'MSAName', 'MetropolitanDivisionTitle',
                     'CSATitle', 'County_or_Equivalent', 'StateName',
                     'stfips', 'cofips', 'Central_or_Outlying_County')

# POPESTIMATE2019 is the most recent estimated population field in this file:
uspopdata <- get_pop_data(localfilename=uscopopdata_filename, 
                          census_url=uspopdataurl)
# poplist uses NUMERIC FIPS codes for counties. The rest of the data frames should not.
poplist <- get_us_population_by_county(2019)

msalist <- get_msa_list(fileurl = msa_list_filename, 
                        col_classes = msafips_colclasses,
                        msafips_columns = msafips_columns)

mapdir <- 'Downloads/GIS Data'
uscountiesmap <- get_county_maps(homedir=homedir, 
                                 mapdir=mapdir, 
                                 uspopdata=uspopdata)

assign("lowcolor", "#FFFFBB", envir = .GlobalEnv)
# lowcolor <- "#FFFFBB"
# lowcolor <- "#FFFF22"
# midcolor <- "#3030A0"
assign("midcolor", "#1111a0", envir = .GlobalEnv)
# midcolor <- "#0000a0"
# midcolor <- "#b00000"
# highcolor <- "#b00000"
assign("highcolor", "#000030", envir = .GlobalEnv)
# highcolor  <- "#000010"

setwd(homedir)
setwd('covidRscript')

txfips <- get_texas_metro_county_list(fipsurl = txfipsurl, remote=FALSE)
metro_msa_name <- 'Dallas-Fort Worth-Arlington'
dfw_fips <- get_metro_fips(txfips, metro_msa_name)

# IF THIS FUNCTION BARFS: you might have to install Perl, or download the file 
# and create the data frame another way. Download the file and convert it to 
# a CSV directly, then execute this command:
if (length(dfw_fips) == 0) {
  stop('Oops -- unable to convert the Texas Metro area FIPS file.')
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
statefipscodes <- import_fips_codes(pageurl = fipsurl, 
                                    fipscolclasses = fipscolclasses
                                   )

dropstates <- c(60, 66, 69, 74, 78)
`%notin%` <- Negate(`%in%`)
statefipscodes <- statefipscodes[which(statefipscodes$STATE %notin% dropstates),]

# Province/State,Country/Region,Last Update,Confirmed,Deaths,Recovered
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

# ISO country codes:
setwd(homedir)
setwd('covidRscript')
isocodes <- get_iso_country_codes()

################################################################################
# Update and import the Johns Hopkins COVID-19 data:
# https://github.com/CSSEGISandData/COVID-19/
#
# To mirror the repository locally, in (here) a directory called COVID-19:
#
# git clone https://github.com/CSSEGISandData/COVID-19.git ./COVID-19
################################################################################

repo <- 'COVID-19'
datadir <- 'csse_covid_19_data/csse_covid_19_daily_reports/'
sync_jhu(homedir=homedir, repo=repo)
setwd(paste(homedir, repo, datadir, sep = '/'))

# Check for the csv for each completed year; this saves a lot of time loading.
covid_2020 <- import_jhu_2020(filestub=fn_stub, uspopdata=uspopdata, statefipscodes=statefipscodes)

covid_2021 <- import_jhu(year=2021, uspopdata=uspopdata, statefipscodes=statefipscodes, check=FALSE)
covid_2022 <- import_jhu(year=2022, uspopdata=uspopdata, statefipscodes=statefipscodes, check=FALSE)
if(isFALSE(covid_2022)) { stop('Unable to import because of bad FIPS code formatting.') }

covid3 <- rbind(covid_2020, covid_2021, covid_2022)
covid3 <- covid3[which(covid3$FIPS %notin% c("00", "0")),]

# assign MSA values to each county observation:
covid3 <- assign_msa(covid3, msalist)


options(scipen=1000000)
hist(covid3$percent_infected, 
     xlab="Percent Infected in Each County", 
     xlim=c(0, 60),
     ylab="Frequency",
     ylim=c(0, 1500000),
     main="Histogram of Percent Infected in the US, by County")

# Some very small counties have infection rates over 100%, probably because of
# seasonal workers or visitors.  We'll still count them, but just make the 
# Maximum infected percentage equal to counties a bit larger, to avoid 
# unbalancing the range of infection rates.
max_infected_pct <- 0.95 * max(covid3$percent_infected[which(covid3$population > 2000)])
assign("max_infected_pct", max_infected_pct, envir = .GlobalEnv)
message(sprintf("Max infected percent of counties over 2,000 population: %0.2f%%", max_infected_pct))

if(max(covid3$posixdate) <= strptime(x = sprintf("12-31-%s", 
                                                 strftime(Sys.Date() - 365,
                                                          format="%Y")),
                                     format="%m-%d-%Y")) {
  stop('Most recent date in Covid-19 data is from last year!!')
}


################################################################################
# New York Times US county-level data:
################################################################################

sync_nyt(homedir=homedir)
nyt <- get_nyt(homedir=homedir)

################################################################################
# EU / ECDC data:
################################################################################
# New data location (weekly, not daily): 
# https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv
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

#
# do_state_plots(stfips = '01', inputjhu = covid3, inputnyt = nyt, stfipslist = statefipscodes)
#

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
# GIS data can be obtained from:
# https://www2.census.gov/geo/tiger/TIGER2017/STATE/
#
# https://www.census.gov/geographies/mapping-files/2015/geo/carto-boundary-file.html
# 
#
################################################################################

#metro_fips$newfips <- sprintf('%s%s', metro_fips$stfips, metro_fips$cofips)
setwd(paste(homedir, mapdir, sep='/'))

msaname <- 'New York-Newark-Jersey City, NY-NJ-PA'
metro_fips <- get_metro_fips_2(msalist, msa_name = msaname, varname = 'CBSATitle')
metrocountymap <- make_metro_map(countiesmap = uscountiesmap,
                                 msa_name = msaname,
                                 msalist = msalist
                                )

metro_covid3 <- covid3[unlist(lapply(X = seq(1, nrow(metro_fips)),
                                     FUN = function(x) { 
                                       which(covid3$newfips == metro_fips$newfips[x])
                                     }
                                    )
                             )
                      ,]

metro_covid3 <- do_posixtime(metro_covid3) 
covid3_metro_yesterday <- metro_covid3[which(metro_covid3$date == as.Date(max(metro_covid3$posixtime)) - 1), ]
metrocountymap$Confirmed <- 0

for (x in seq(1, nrow(covid3_metro_yesterday))) {
    replacement <- covid3_metro_yesterday$Confirmed[which(covid3_metro_yesterday$newfips == metrocountymap$GEOID[x])]
    message(sprintf('X: %s\t FIPS: %s\treplacement value: %s', 
                    x, covid3_metro_yesterday$newfips[x], replacement))
    if (length(replacement) > 0) {
      metrocountymap$Confirmed[x] <- replacement
    }
}

metrocountymap$percent_infected <- 100 * (metrocountymap$Confirmed / metrocountymap$Population)

todaytitle <- sprintf("%s COVID-19 Cases as of %s", msaname, Sys.Date())
todaysubtitle <- sprintf("and Percent of County Population Infected")

metro_plot <- metro_map_plot(metrocountymap,
                             todaytitle,
                             todaysubtitle,
                             max_infected_pct=max_infected_pct)

todaymapfilename <- sprintf("%s_covid19_metromap_%s.png", 
                            gsub(' ', '', gsub('-|,', '', msaname)), Sys.Date()
                           )
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
                           max_infected_pct = max_infected_pct,
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
  scale_fill_gradient2(high= highcolor,
                      mid = midcolor,
                      low = lowcolor,
                      limits = c(0, max_infected_pct),
                      midpoint = 40
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

uscountiesmap$POPESTIMATE2019 <- 0
uscountiesmap$POPESTIMATE2019 <- sapply(X=uscountiesmap$GEOID,
                                        FUN=function(x){
                                          poplist$POPESTIMATE2019[which(poplist$newfips==x)]
                                        }
                                        )

# Make a county-level plot foe one state:
setwd(outputdir)
state_map <- make_state_map(uscountiesmap=uscountiesmap, 
               state_code='48', 
               statefipscodes=statefipscodes)


lapply(X=statefipscodes$STATE[which(statefipscodes$STATE %notin% c("02", "15"))],
       FUN=function(x) {make_state_map(uscountiesmap=uscountiesmap, 
                                       state_code=x, 
                                       statefipscodes=statefipscodes)})

make_state_map(uscountiesmap=uscountiesmap, 
               state_code="49", 
               statefipscodes=statefipscodes)

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
                                                     FUN = function(x) {
                                                       sandiego$Confirmed[x] - sandiego$Confirmed[x-1]
                                                     }))
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

