#####
#
#
#####

library(httr)
library(curl)


msa_list_url <- 'https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html'
censusgov <- 'https://www2.census.gov/programs-surveys'
excel_msa_list <- 'list1_2020.xls'
msa_list_filename <- sprintf('%s/metro-micro/geographies/reference-files/2020/delineation-files/%s',
                             censusgov, excel_msa_list)
uscopopdata_filename <- 'co-est2019-alldata.csv'
uspopdataurl <- sprintf('%s/popest/datasets/2010-2019/counties/totals/%s',
                        censusgov, uscopopdata_filename)

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

txfipsurl <- 'http://www.dshs.state.tx.us/chs/info/TxCoPhrMsa.xls'

fipsurl <- 'https://www2.census.gov/geo/docs/reference/state.txt?#'


import_fips_codes <- function(pageurl,
                              fipscolclasses,
                              filename = "statefipscodes.txt"){
  
  if (file.exists(filename) == TRUE) {
    message(sprintf("File %s exists locally.", filename))
    fipsurl = filename
  } else {
    message(sprintf("File %s does not exist locally; will retrieve from URL.", filename))
    curl_download(url = pageurl, destfile = filename, quiet = TRUE, mode = "wb", handle = new_handle())
    if (file.exists(filename) == FALSE) {
      message(sprintf("WARNING: Unable to download file %s from the web.", filename)) 
      stop()
    }
  }
  
  statefipscodes <- read.delim(filename, 
                               header=TRUE, 
                               sep = '|', 
                               colClasses = fipscolclasses
  )  
  return(statefipscodes)
}



get_us_population_by_county <- function(year = 2019) {
  # Good for 2010-2019:
  uspopdataurl <- 'https://www2.census.gov/programs-surveys/popest/datasets'
  uspoppath <- sprintf('2010-%s/counties/totals', year)
  uspopfile <- sprintf('co-est%s-alldata.csv', year)
  urlpath <- paste(uspopdataurl, uspoppath, uspopfile, sep='/')
  
  # check for local file first:
  if (file.exists(uspopfile) == TRUE) {
    message(sprintf("File %s exists locally.", uspopfile))
  } else {
    message(sprintf("File %s does not exist locally; will download and save it.", uspopfile))
    curl_download(url = urlpath,
                  destfile = uspopfile, 
                  quiet = TRUE, 
                  mode = "wb", 
                  handle = new_handle())
    if (file.exists(uspopfile) == FALSE) {
      message(sprintf("WARNING: Unable to download file %s from the web.", uspopfile)) 
      stop()
    }
  }
  
  uspopbycounty <- read.csv(file=uspopfile, header=TRUE)
  uspopbycounty$stfips <- sprintf("%02.0f", uspopbycounty$STATE)
  uspopbycounty$cofips <- sprintf("%03.0f", uspopbycounty$COUNTY)
  uspopbycounty$GEOID <- sprintf('%02g%03g', uspopbycounty$STATE, uspopbycounty$COUNTY)
  uspopbycounty$newfips <- sprintf("%02g%03g", uspopbycounty$STATE, uspopbycounty$COUNTY)
  return(uspopbycounty)
}


get_texas_population_by_county <- function(year = 2019) {
  mainsite <- 'https://demographics.texas.gov'
  zipfile <- sprintf("%g_ASRE_Estimate_alldata_csv.zip", year)
  localfile <- 'alldata.csv'
  texas_population_url <- sprintf('%s/Resources/TPEPP/Estimates/%g/%s', mainsite, year, zipfile)
  
  if(file.exists(zipfile) == FALSE) {
    message(sprintf("Source file not found. Will download from %s", mainsite))
    curl_download(texas_population_url, 
                  zipfile, 
                  quiet = TRUE, 
                  mode = "wb", 
                  handle = new_handle()
    )
  }
  
  if (file.exists(zipfile) == FALSE) {
    message(sprintf("WARNING: Unable to download file %s from network", zipfile)) 
  } else {
    rawfile <- unzip(zipfile)
    texaspop <- read.csv(localfile, header=TRUE, stringsAsFactors = FALSE)
  }
  
  return(texaspop)
}


import_covid_subset <- function(fileslist, col_classes){
  
  covid <- data.frame()
  for (filename in fileslist) {
    if (!file.exists(filename)) {
      message(sprintf("Unable to find file %s", filename))
      next()
    }
    message(sprintf("Importing file %s ", filename))
    f1 <- read.csv(filename, 
                   header = TRUE, 
                   stringsAsFactors = FALSE,
                   colClasses = col_classes,
                   na.strings='#DIV/0!'
    )
    
    # Some states only report aggregated results for some counties, 
    # such as Massachusetts, that reports Dukes county and Nantucket county
    # in a combined fashion, with NO FIPS CODE attached. I choose to combine them
    # into a FIPS code of 25099, and to merge the two county polygons. 
    # Also need to merge their populations, lest they report weird percentages.
    # What a pain in the ass.
    
    f1$FIPS[which(f1$Combined_Key == "Dukes and Nantucket, Massachusetts, US")] <- "25099"
    

    
    message(sprintf("Adding parsed date: %s", strsplit(filename, '\\.')[[1]][1]))
    f1$date <- as.Date(strsplit(filename, '\\.')[[1]][1], format = "%m-%d-%Y")
    f1$posixdate <- as.Date(f1$date, format = "%Y-%m-%d")
    f1$year <- as.numeric(strftime(f1$posixdate, format = "%Y"))
    f1 <- cbind(f1, as.data.frame(matrix(data="0", nrow=nrow(f1), 
                                         ncol=3, byrow=TRUE))
               )
    names(f1)[which(names(f1) %in% c("V1", "V2", "V3"))] <- 
      c('newfips', 'stfips', 'cofips')
    
    if ("FIPS" %in% names(f1)) {
      nofips <- which(is.na(f1$FIPS))
      f1$FIPS[nofips] <- "00"
      f1$FIPS <- sprintf("%05g", as.integer(f1$FIPS))
      f1$newfips <- f1$FIPS
      f1$stfips <- substr(f1$newfips, 1, 2)
      f1$cofips <- substr(f1$newfips, 3, 5)
    } else {
      f1$FIPS <- "0"
    }
    
    covid <- rbind(covid, f1)
  }
    
  if(check_unique_fips(covid) == FALSE) {
    stop(sprintf('Bad formatting for FIPS codes in file ', filename), call.=TRUE)
    return(FALSE)
  }
  
  return(covid)
}


get_texas_metro_county_list <- function(fipsurl, remote=TRUE){
  txfips_colclasses <- c('character')
  txfips_colnames <- c('CountyName', 
                       'FIPS',
                       'County',
                       'PublicHealthRegion',
                       'HealthServiceRegion',
                       'MSA',
                       'MD',
                       'MetroArea',
                       'NCHSUrbanRural_2006',
                       'NCHSUrbanRural_2013',
                       'Border_La_Paz',
                       'Border'
  )
  
  if (remote == FALSE){
    portions <- unlist(strsplit(x = fipsurl, split = "/"))
    localfile <- portions[length(portions)]
    if (file.exists(localfile) == TRUE) {
      message(sprintf("File %s exists locally.", localfile))
      fipsurl = localfile
    } else {
      message(sprintf("File %s does not exist locally; will retrieve from URL.", localfile))
      curl_download(txfipsurl, localfile, quiet = TRUE, mode = "wb", handle = new_handle())
      if (file.exists(localfile) == FALSE) {
        message(sprintf("WARNING: Unable to download file %s from network", localfile)) 
      } else {
        fipsurl = localfile
      }
    }
  }
  
  txfips <- read.xls(xls = fipsurl,
                     sheet = 'Sheet1',
                     colClasses = txfips_colclasses
  )
  names(txfips) <- txfips_colnames
  return(txfips)
}



get_msa_list <- function(fileurl, col_classes, msafips_columns) {
  localfilename <- 'excelfips.xls'
  # Check to see if the file exists locally first:
  if (file.exists(localfilename) == TRUE) {
    message(sprintf("File %s exists locally.", localfilename))
  } else {
    message(sprintf("File %s does not exist locally; will retrieve from URL.", localfilename))
    curl_download(url = fileurl,
                  destfile = localfilename, 
                  quiet = TRUE, 
                  mode = "wb", 
                  handle = new_handle())
    if (file.exists(localfilename) == FALSE) {
      message(sprintf("WARNING: Unable to download file %s from the web.", localfilename)) 
      stop()
    }
  }
  
  msafips <- read.xls(xls = localfilename,
                      pattern = 'CBSA Code',
                      colClasses = col_classes
  )
  names(msafips) <- msafips_columns
  msafips$FIPS <- paste(msafips$stfips, msafips$cofips, sep='')
  return(msafips)
}


get_pop_data <- function(localfilename, census_url) {
  
  if (file.exists(localfilename) == TRUE) {
    message(sprintf("File %s exists locally.", localfilename))
  } else {
    message(sprintf("File %s does not exist locally; will retrieve from URL.", localfilename))
    curl_download(url = census_url,
                  destfile = localfilename, 
                  quiet = TRUE, 
                  mode = "wb", 
                  handle = new_handle())
    if (file.exists(localfilename) == FALSE) {
      message(sprintf("WARNING: Unable to download file %s from the web.", localfilename)) 
      stop()
    }
  }
  
  uspopdata <- read.csv(localfilename)
  uspopdata$GEOID <- sprintf('%02g%03g', uspopdata$STATE, uspopdata$COUNTY)
  
  # Get Dukes and Nantucket counties, MA:
  newrow <- uspopdata[which(uspopdata$GEOID %in% c("25007", "25019")),]
  newrow[1, 8:110] <- newrow[1, 8:110] + newrow[2, 8:110]
  newrow$CTYNAME[1] <- "Dukes and Nantucket Counties"
  newrow$COUNTY[1] <- 99
  newrow[1, 111:164] <- NA  # Can't fairly report rates
  uspopdata <- rbind(uspopdata, newrow[1,])

  return(uspopdata)
}


get_iso_country_codes <- function() {
  iso_codes_url <- "https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes"
  filename <- 'isocodes.csv'
  isonames <- c('countryname', 'officialname', 'sovereignty', 'alpha2',
                'alpha3', 'numeric', 'subdiv', 'internet_tld')
  
  # Check for a locally-cached copy first:
  
  if (file.exists(filename) == TRUE) {
    message(sprintf("File %s exists locally.", filename))
    cleaned_isocodes <- read.csv(filename)
    
  } else {
    
    message(sprintf("File %s does not exist locally; will retrieve from URL.", filename))
    isop1 <- GET(url=iso_codes_url)
    if(identical(status_code(isop1), 200L)){
      isopage <- read_html(isop1)
    }
    isocodes <- isopage %>% html_nodes("table") %>% .[[1]] %>% html_table(fill = TRUE)
    names(isocodes) <- isonames
    cleaned_isocodes <- isocodes[!grepl(' – See ', isocodes$countryname,
                                        ignore.case = TRUE, perl = TRUE),]
    cleaned_isocodes <- cleaned_isocodes[!grepl('Country name', cleaned_isocodes$countryname,
                                                ignore.case = TRUE, perl = TRUE),]
    
    # Save the good data into filename:
    write.csv(x = cleaned_isocodes, file = filename, quote = TRUE)
    
    # confirm the data write worked okay:
    if (file.exists(filename) == FALSE) {
      message(sprintf("WARNING: Unable to download file %s from the web.", filename)) 
      stop()
    }
  }
  return(cleaned_isocodes)
}





import_jhu_2020 <- function(filestub='r_covid_', uspopdata, statefipscodes) {
  year  <- 2020
  # Check for the csv for each completed year; this saves a lot of time loading.
  fn_year = sprintf('%s%s.csv', filestub, year)
  if (file.exists(fn_year) == TRUE) {
    message(sprintf("File %s exists locally.", fn_year))
    
    ccc1 <- c('FIPS' = 'character',
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
              'Case_Fatality_Ratio' = 'numeric',
              'date' = 'character',
              'posixdate' = 'character', 
              'year' = 'numeric'
              )
    
    # "FIPS","Admin2","Province_State","Country_Region","Last_Update",
    # "Lat","Long_","Confirmed","Deaths","Recovered","Active","Combined_Key",
    # "Incidence_Rate","Case.Fatality_Ratio","date","posixdate","year",
    # "newfips","stfips","cofips","population","percent_infected"
    covid3 <- read.csv(fn_year, stringsAsFactors = FALSE, colClasses = ccc1)
    # Force date and posixtime to be doubles, not character strings:
    covid3$date <- as.Date(covid3$date)
    covid3$posixdate <- as.Date(covid3$posixdate)

    return(covid3)
  } else {
    message(sprintf("File %s not found locally; reading individual files", fn_year))
  }
  
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
  
  what5 <- c('character', 'character', 'factor', 'character', 'character',
             'double', 'double', 'numeric', 'numeric', 'numeric', 'numeric',
             'character', 'numeric', 'numeric')
  
  files_2020 <- dir(pattern = "\\d{2}-\\d{2}-2020\\.csv")
  files <- c(files_2020)
  # the file format changed on 03.01.2020, 03.22.2020, 05.27.2020, and 11.09.2020:
  # Files get sorted alphabetically, meaning Jan 2020 and Jan 2021 get conflated.
  breakpoint1 <- which(files == "02-29-2020.csv")

  # Files from here on out have FIPS codes for US entities.
  breakpoint2 <- which(files == "03-21-2020.csv")
  breakpoint3 <- which(files == "05-28-2020.csv")
  breakpoint4 <- which(files == "11-08-2020.csv")
  
  files1 <- files[1:breakpoint1]
  files2 <- files[(breakpoint1 + 1):breakpoint2]
  files3 <- files[(breakpoint2 + 1):breakpoint3]
  files4 <- files[(breakpoint3 + 1):breakpoint4]
  files5 <- files[(breakpoint4 + 1):length(files)]
  
  covid1 <- import_covid_subset(fileslist = files1, col_classes = col_classes1)
  covid2 <- import_covid_subset(fileslist = files2, col_classes = col_classes2)
  covid3 <- import_covid_subset(fileslist = files3, col_classes = col_classes3)
  covid4 <- import_covid_subset(fileslist = files4, col_classes = col_classes4)
  covid5 <- import_covid_subset(fileslist = files5, col_classes = col_classes5)

  # Set up the first two formats for a merge:
  covid1$Longitude <- as.double(0.0)
  covid1$Latitude <- as.double(0.0)
  covid1 <- rbind(covid1, covid2)
  
  # The third format is a lot different.
  covid3$Incidence_Rate <- 0.0
  covid3$Case.Fatality_Ratio <- 0.0
  
  names(covid5) <- names(covid4)
  covid3 <- rbind(covid3, covid4, covid5)
  
  #covid3$newfips <- sprintf("%05.0f", as.integer(covid3$FIPS))
  #covid3$stfips <- substr(covid3$newfips, 1,2)
  #covid3$cofips <- substr(covid3$newfips, 3,5)
  covid3$date <- as.Date(covid3$date)
  covid3$posixdate <- as.Date(covid3$posixdate)
  
  # Fix a mammoth typo (three orders of magnitude in Okaloosa Co., FL):
  covid3$Active[which(covid3$date == "2020-04-13" & covid3$FIPS == "12091")] <- 102
  
  covid3 <- add_percent_infected(covid3, uspopdata, newfips=statefipscodes)
  
  message(sprintf('Writing file %s locally.', fn_year))
  write.csv(covid3, file=fn_year, quote=TRUE, row.names=FALSE)
  
  return(covid3)
}




import_jhu <- function(year, 
                       uspopdata, 
                       statefipscodes, 
                       check=TRUE, 
                       filestub='r_covid_') {
  # Check for the csv for each completed year; this saves a lot of time loading.
  fn_year = sprintf('%s%s.csv', filestub, year)
  if (file.exists(fn_year) == TRUE && isTRUE(check)) {
    message(sprintf("File %s exists locally.", fn_year))
    ccc1 <- c('FIPS' = 'character',
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
              'Case_Fatality_Ratio' = 'numeric',
              'date' = 'character',
              'posixdate' = 'character', 
              'year' = 'numeric'
    )
    covid3 <- read.csv(fn_year, stringsAsFactors = FALSE)
    covid3 <- read.csv(fn_year, stringsAsFactors = FALSE, colClasses = ccc1)
    covid3$date <- as.Date(covid3$date)
    covid3$posixdate <- as.Date(covid3$posixdate)
    return(covid3)
  } else if(isFALSE(check)) {
    message(sprintf("Check set to False. Will read in individual files for %g.", year))
  } else {
    message(sprintf("File %s does not exist locally; will read in individual files", fn_year))
  }
  
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
  
  files_yr <- dir(pattern = sprintf("\\d{2}-\\d{2}-%g\\.csv", year))
  files <- c(files_yr)
  covid5 <- import_covid_subset(fileslist = files, 
                                col_classes = col_classes5
                               )
  if(isFALSE(covid5)) {
    return(FALSE)
  }
  #covid5$newfips <- sprintf("%05.0f", as.integer(covid5$FIPS))
  #covid5$stfips <- substr(covid5$newfips, 1, 2)
  #covid5$cofips <- substr(covid5$newfips, 3, 5)
  names(covid5) <- c("FIPS", "Admin2", "Province_State", "Country_Region", 
                     "Last_Update", "Lat", "Long_", "Confirmed", "Deaths", 
                     "Recovered", "Active", "Combined_Key", "Incidence_Rate",
                     "Case.Fatality_Ratio", "date", "posixdate", "year", 
                     "newfips", "stfips", "cofips")
  
  covid5 <- add_percent_infected(covid5, uspopdata=uspopdata, newfips=statefipscodes)
  
  message(sprintf('Writing file %s locally.', fn_year))
  write.csv(covid5, file=fn_year, quote=TRUE, row.names=FALSE)
  
  return(covid5)
}


do_posixtime <- function(covid_data) {
  covid_data$posixtime <- strptime(x = covid_data$Last_Update, 
                                   format="%Y-%m-%d %H:%M:%S",
                                   tz="GMT")
  covid_data[which(is.na(covid_data$posixtime)),]$posixtime <- strptime(x = covid_data[which(is.na(covid_data$posixtime)),]$Last_Update,
                                                                        tz="GMT",
                                                                        format="%D %R")
  covid_data[which(is.na(covid_data$posixtime)),]$posixtime <- strptime(x = covid_data[which(is.na(covid_data$posixtime)),]$Last_Update,
                                                                        tz="GMT",
                                                                        format="%m/%e/%y %R")
  return(covid_data)
}


sync_nyt <- function(homedir) {
  nyt_repo <- 'nytimescovid'
  setwd(paste(homedir, nyt_repo, sep = '/'))
  system('git config pull.rebase false')
  system('git pull')
}


get_nyt <- function(homedir) {
  nyt_repo <- 'nytimescovid'
  setwd(paste(homedir, nyt_repo, sep = '/'))
  nyt_header <- c('date','county','state','fips','cases','deaths')
  nyt_col_classes <- c('date' = 'character',
                       'county' = 'character',
                       'state' = 'character',
                       'fips' = 'character',
                       'cases' = 'numeric',
                       'deaths' = 'numeric')
  
  nyt <- read.csv('us-counties.csv',
                  header = TRUE, 
                  stringsAsFactors = FALSE,
                  colClasses = nyt_col_classes
  )
  nyt$posixdate <- as.Date(nyt$date, format = "%Y-%m-%d")

  return(nyt)
}


sync_jhu <- function(homedir, repo) {
  # Get list of daily data files with file pattern like: "04-02-2020.csv"
  setwd(paste(homedir, repo, sep = "/"))
  system('git config pull.rebase false')
  system('git pull')
}


get_county_maps <- function(homedir, mapdir, uspopdata) {
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

  # merge combined county data into both Dukes and Nantucket counties because
  # MA doesn't report them separately.
  merged_polys <- dukes_and_nantucket(cofips1="019",
                                      cofips2="007",
                                      statecode="25", 
                                      mergedname="Dukes and Nantucket",
                                      countymap=uscountiesmap,
                                      newcountycode="099"
                                     )
  
  uscountiesmap <- rbind(uscountiesmap[which(uscountiesmap$GEOID %notin% c("25019", "25007")),],
                         merged_polys)
  
  return(uscountiesmap)
}


dukes_and_nantucket <- function(cofips1, cofips2, statecode, 
                                mergedname, countymap,  newcountycode="099") {
  # merge combined county data into both Dukes and Nantucket counties because
  # MA doesn't report them separately.
  d1 <- sprintf("%s%s", statecode, cofips1)
  d2 <- sprintf("%s%s", statecode, cofips2)
  dukes <- which(countymap$GEOID==d1)
  nantucket <- which(countymap$GEOID==d2)
  merged_polys <- st_union(x=countymap[nantucket, ],
                           y=countymap[dukes, ],
                           by_feature=FALSE,
                           is_coverage=TRUE
  )
  merged_polys <- merged_polys[, c(1:9, 21, 10)]
  
  merged_polys$GEOID <- sprintf("%s%s", statecode, newcountycode)
  merged_polys$COUNTYFP <- newcountycode
  merged_polys$NAME <- mergedname
  merged_polys$ALAND <- countymap$ALAND[dukes] + countymap$ALAND[nantucket]
  merged_polys$AWATER <- countymap$AWATER[dukes] + countymap$AWATER[nantucket]
  merged_polys$Population <- countymap$Population[dukes] + countymap$Population[nantucket]
  merged_polys$AFFGEOID <- NA
  merged_polys$COUNTYNS <- NA

  return(merged_polys)
}



add_percent_infected <- function(covid_data, uspopdata, newfips) {
  cd <- covid_data[which(covid_data$Country_Region == "US"),]
  uniquefips <- unique(cd$newfips)
  
  if(isFALSE(check_unique_fips(cd))) {
    stop(sprintf('Bad formatting for FIPS codes in covid data frame.'))
    return(FALSE)
  }
  
  cd$population <- NA
  uspopdata$stfips <- sprintf("%02g", uspopdata$STATE)
  uspopdata$cofips <- sprintf("%03g", uspopdata$COUNTY)
  countypop <- uspopdata[which(uspopdata$COUNTY != 0),]
  pop_states_list <- unique(countypop$stfips)
  badstates <- c()
  
  for(y in seq(1, length(uniquefips))) {
    myrow <- which(cd$newfips == uniquefips[y])
    st_abbr <- unique(cd$stfips[myrow])
    if("00" %in% st_abbr) {
      next();
    }
    if(length(st_abbr) > 1) {
      stop("More than one state FIPS code for this entry!")
      bad_fips <- "Offending FIPS: "
      for(i in seq(1, length(st_abbr))) {
        bad_fips <- (sprintf("%s %s,", bad_fips, st_abbr[i]))        
      }
      message(bad_fips)
    }
    if(st_abbr %in% pop_states_list) {
      message(sprintf("%s -- %s, count: %g", uniquefips[y], st_abbr, length(myrow)))
      if (length(which(countypop$GEOID == uniquefips[y])) == 0) {
        message("Skipping!")
        next()
      }
      cd$population[myrow] <- 
        countypop$POPESTIMATE2019[which(countypop$GEOID==uniquefips[y])]
    } else {
      if (!st_abbr %in% badstates) {
        badstates <- c(badstates, st_abbr)
      }
    }
  }
  
  bs <- "Skipped State FIPS Codes: "
  if(length(badstates == 0)) {
    bs <- sprintf("%s None.", bs)
  } else {
    for (i in seq(1, length(badstates))) {
      bs <- sprintf("%s, %s", bs, badstates[i]) 
    }
  }
  message(bs)
  cd$population[which(is.na(cd$cofips))] <- 0
  cd$percent_infected <- 100 * cd$Confirmed / cd$population
  
  return(cd)
}



check_unique_fips <- function(dataset) {
  uniquefips <- unique(dataset$newfips)
  for(y in seq(1, length(uniquefips))) {
    myrow <- which(dataset$newfips == uniquefips[y])
    st_abbr <- unique(dataset$stfips[myrow])
    if("00" %in% st_abbr) {
      next();
    }
    if(length(st_abbr) > 1) {
      message("More than one state FIPS code for this entry!")
      bad_fips <- "Offending FIPS: "
      for(i in seq(1, length(st_abbr))) {
        bad_fips <- (sprintf("%s %s,", bad_fips, st_abbr[i]))        
      }
      message(bad_fips)
      return(FALSE)
    }
  }
  return(TRUE)
}


gather_bad_fips <- function(covid_data, uspopdata, newfips) {
  cd <- covid_data[which(covid_data$Country_Region == "US"),]
  uniquefips <- unique(cd$newfips)
  
  if(isFALSE(check_unique_fips(cd))) {
    stop(sprintf('Bad formatting for FIPS codes in covid data frame.'))
    return(FALSE)
  }
  
  cd$population <- NA
  uspopdata$stfips <- sprintf("%02g", uspopdata$STATE)
  uspopdata$cofips <- sprintf("%03g", uspopdata$COUNTY)
  countypop <- uspopdata[which(uspopdata$COUNTY != 0),]
  pop_states_list <- unique(countypop$stfips)
  badstates <- c()
  skipped <- c()
  skiptext <- ""

  for(y in seq(1, length(uniquefips))) {
    myrow <- which(cd$newfips == uniquefips[y])
    st_abbr <- unique(cd$stfips[myrow])
    state_alpha <- cd$Province_State[myrow[1]]
    if(length(st_abbr) > 1) {
      stop("More than one state FIPS code for this entry!")
      bad_fips <- "Offending FIPS: "
      for(i in seq(1, length(st_abbr))) {
        bad_fips <- (sprintf("%s, %s: %s,", bad_fips, st_abbr[i], state_alpha))
      }
      message(bad_fips)
    }
    if(st_abbr %in% pop_states_list) {
      # message(sprintf("%s -- %s, count: %g", uniquefips[y], st_abbr, length(myrow)))
      if (length(which(countypop$GEOID == uniquefips[y])) == 0) {
        if (!uniquefips[y] %in% skipped) {
          skipped <- c(skipped, uniquefips[y])
          skiptext <- sprintf("%s, %s, %s, %s", 
                              cd$FIPS[myrow], cd$stfips[myrow], 
                              cd$cofips[myrow], cd$date[myrow])
        }
        next()
      }
      next()
    }
    
    if (!st_abbr %in% badstates) {
      badstates <- c(badstates, st_abbr)
      }
  }
  bs <- as.character(badstates[1])
  message('Bad state codes: ')
  for (i in seq(2, length(badstates))) {
    bs <- sprintf("%s %s, ", bs, as.character(badstates[i]))
  }
  message(sprintf("%s", bs))
  
  
  if (length(skipped) > 0) {
    message(sprintf("Skipped:\n%s", skiptext))
  } else {
    message("Skipped: None")
  }
}


# EOF