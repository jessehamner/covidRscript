################################################################################
# covidfunctions.R: a small R function library to use the JHU COVID-19 data and 
# New York Times data on the US and global pandemic of 2020.
#
# Jesse Hamner, 2020
#
################################################################################

library(httr)


import_fips_codes <- function(pageurl, fipscolclasses){
  if(identical(status_code(GET(pageurl)), 200L)){
    statefipscodes <- read.delim(url(pageurl), 
                                 header=TRUE, 
                                 sep = '|', 
                                 colClasses = fipscolclasses
    )
  } else {
    stop()
  }
  return(statefipscodes)
}


do_state_plots <- function(stfips, inputjhu, inputnyt, stfipslist) {
  stname = stfipslist$STATE_NAME[which(stfipslist$STATE == stfips)]
  state_level <- make_state_data(inputdf = inputjhu, stfips = stfips)
  nyt_state_level <- nyt_state_match(nyt = inputnyt, stfips = stfips)
  if (isFALSE(state_level)) {
    message('No usable data in JHU data subset.')
    return(FALSE)
  }
  if (length(state_level) == 0) {
    message('No usable data in JHU data subset.')
    return(FALSE)
  }
  if (nrow(state_level) < 2) {
    message('No usable data in JHU data set.')
    return(FALSE)
  }
  if (isFALSE(nyt_state_level)){
    message('No usable data in NYT data set.')
    return(FALSE)
  }
  if (nrow(nyt_state_level) < 2) {
    message('No usable data in NYT data set.')
    return(FALSE)
  }
    
  plot_daily_increase(state = stname, dataset = state_level)
  plot_cumulative_cases(state = stname,
                        jhu_data = state_level,
                        nyt_data = nyt_state_level)
  
  return()
}


nyt_subset <- function(nytdata, stfips, countysubset){
  n_subset <- nytdata[which(nytdata$fips %in% paste(stfips,sprintf('%03.0f', countysubset), sep = '')),]
  n_subset_covid <- aggregate(x = n_subset$cases,
                              FUN = sum,
                              by = list(n_subset$posixdate))
  names(n_subset_covid) <- c('date', 'cases')
  nyt_subset_covid_match <- n_subset_covid[which(n_subset_covid$date > as.Date("2020-03-21", 
                                                                               format = "%Y-%m-%d")
  ),]
  nyt_subset_covid_match$posixdate <- as.Date(nyt_subset_covid_match$date, format = "%Y-%m-%d")
  return(nyt_subset_covid_match)
}

plot_daily_increase <- function(state, dataset) {
  message(sprintf('Writing daily case increase graphic for %s', state))
  basic_plot(sprintf('%s_covid19_confirmed_daily_increase.png', gsub(' ', '_', state)),
             daily_increase_plot(dataset, metro_label = state)
  )
  return (0)
}


plot_cumulative_cases <- function(state, jhu_data, nyt_data){
  message(sprintf('Writing daily confirmed cases graphic for %s', state))
  basic_plot(sprintf('%s_covid19_confirmed_daily_active_cases.png', gsub(' ', '_', state)),
             cumulative_cases_plot(jhu_data, nyt_metro_covid_match = nyt_data, area_label = state )
  )
  return (0)
}


nyt_state_match <- function(nyt, stfips, startdate = "2020-03-21"){
  nyt_st <- nyt[which(grepl(sprintf('^%s', stfips), nyt$fips)),]
  
  if (nrow(nyt_st) < 2) {
    message(sprintf('FIPS code %s has too few entries to use in the NYT data.', stfips))
    return(FALSE)
  }
  
  nyt_st_covid <- aggregate(x = nyt_st$cases,
                            FUN = sum,
                            by = list(nyt_st$posixdate))
  names(nyt_st_covid) <- c('date', 'cases')
  nyt_st_covid_match <- nyt_st_covid[which(nyt_st_covid$date > as.Date(startdate, 
                                                                       format = "%Y-%m-%d")
  )
  ,]
  nyt_st_covid_match$posixdate <- as.Date(nyt_st_covid_match$date, format = "%Y-%m-%d")
  return(nyt_st_covid_match)
}


make_metro_subset <- function(inputdf, stfips, cofipslist) {
  
  metro_covid_base <- inputdf[which(inputdf$stfips == stfips & inputdf$cofips %in% sprintf('%03.0f', cofipslist)),]
  metro_covid_base$posixdate <- as.Date(metro_covid_base$date, format = "%Y-%m-%d")
  metro_covid <- aggregate(x = metro_covid_base$Confirmed,
                           FUN = sum,
                           by = list(metro_covid_base$posixdate))
  names(metro_covid) <- c('posixdate', 'Confirmed')
  metro_covid$new_today <- ave(metro_covid$Confirmed,
                               FUN = function(x) c(0, diff(x)))
  metro_covid$total_recovered <- aggregate(x = metro_covid_base$Recovered,
                                           FUN = sum,
                                           by = list(metro_covid_base$posixdate))$x
  metro_covid$total_dead <- aggregate(x = metro_covid_base$Deaths,
                                      FUN = sum,
                                      by = list(metro_covid_base$posixdate))$x
  
  metro_covid$active_cases <- metro_covid$Confirmed - metro_covid$total_dead
  return(metro_covid)
}


make_state_data <- function(inputdf, stfips = '48') {
  st_covid_base <- inputdf[which(inputdf$stfips == stfips),]
  
  if (length(st_covid_base) == 0) {
    message('No usable data in JHU data subset.')
    return(FALSE)
  }
  if (nrow(st_covid_base) < 2) {
    message('No usable data in JHU data subset.')
    return(FALSE)
  }
  
  st_covid_base$posixdate <- as.Date(st_covid_base$date, format = "%Y-%m-%d")
  st_covid <- aggregate(x = st_covid_base$Confirmed,
                        FUN = sum,
                        by = list(st_covid_base$posixdate))
  names(st_covid) <- c('posixdate', 'Confirmed')
  st_covid$new_today <- ave(st_covid$Confirmed,
                            FUN = function(x) c(0, diff(x)))
  
  st_covid$total_dead <- aggregate(x = st_covid_base$Deaths,
                                   FUN = sum,
                                   by = list(st_covid_base$posixdate))$x
  
  st_covid$active_cases <- st_covid$Confirmed - st_covid$total_dead
  return(st_covid)
}


get_us_population_by_county <- function(year = 2019) {
  # Good for 2010-2019:
  uspopdataurl <- 'https://www2.census.gov/programs-surveys/popest/datasets'
  uspoppath <- sprintf('2010-%s/counties/totals', year)
  uspopfile <- sprintf('co-est%s-alldata.csv', year)
  urlpath <- paste(uspopdataurl, uspoppath, uspopfile, sep='/')
  
  
  return(0)
}


get_texas_population_by_county <- function(year = 2019) {
  texas_population_url <- sprintf('https://www.dshs.state.tx.us/chs/popdat/st%s.shtm', year)
  tp1 <- GET(texas_population_url)
  if(identical(status_code(tp1), 200L)){
    texaspop <- read_html(tp1)
    poplist <- texaspop %>%
      html_nodes("table") %>%
      .[[2]] %>% html_table()
  } else {
    stop()
  }
  
  cols_to_fix <- c('Total', 'Anglo', 'Black', 'Hispanic', 'Other')
  colnums_to_fix <- which(names(poplist) %in% cols_to_fix)
  
  for(x in seq(1, length(cols_to_fix))) {
    poplist[[cols_to_fix[x]]] <- as.numeric(gsub(',', '', poplist[[cols_to_fix[x]]]))
  }

  return(poplist)
}


basic_plot <- function(filename, plotobj, destination = Sys.getenv('HOME')) {
  png(filename = filename, 
      bg = "white", 
      res = 300, 
      units = "in", 
      pointsize = 14, 
      width = 7,
      height = 6
  )
  
  plotobj
  dev.off()
  return()
}


daily_increase_plot <- function(dfw_metro_covid, metro_label){
  plot(dfw_metro_covid$posixdate, 
       dfw_metro_covid$new_today, 
       type = "l",
       main = sprintf('%s Daily New Confirmed Cases of COVID-19', metro_label),
       xlab = "",
       ylab = "New Cases Each Day"
  )
  
  daily_regression <- lm(dfw_metro_covid$new_today ~ dfw_metro_covid$posixdate)
  slope_today <- daily_regression$coefficients[[2]]
  slope_label <- sprintf("%0.1f", slope_today)
  last_week <- dfw_metro_covid[which(dfw_metro_covid$posixdate >= (max(dfw_metro_covid$posixdate) - lookback_days)),]
  daily_regression_last_week <- lm(last_week$new_today ~ last_week$posixdate)
  slope_last_week_label <- sprintf("%0.1f", daily_regression_last_week$coefficients[[2]])
  textposition <- (as.numeric(max(dfw_metro_covid$posixdate)) - as.numeric(min(dfw_metro_covid$posixdate))) * 0.5
  
  text(x = as.numeric(max(dfw_metro_covid$posixdate)) - textposition,
       y = 0,
       labels = c("Source: Johns Hopkins Univ. Center for Systems Science and Engineering"),
       cex = 0.5,
       font = 3
  )
  
  abline(daily_regression, col = '#6666bb', lty = 2, lwd = 3)
  
  lines(last_week$posixdate,
        predict.lm(daily_regression_last_week),
        col = '#aa6666',
        lwd = 3
  )
  
  ymax_today <- max(dfw_metro_covid$new_today)
  
  legend(x = as.numeric(min(dfw_metro_covid$posixdate)) - 0.5,
         y = ymax_today,
         legend = c(sprintf("Average daily acceleration: %s cases", slope_label), 
                    sprintf("Avg. acceleration (last %s days): %s cases", 
                            lookback_days, slope_last_week_label
                           )
                   ), 
         lty = c(2, 1), 
         lwd = c(3, 3), 
         col = c("#6666bb", "#aa6666"))
  
  text(x = as.numeric(max(dfw_metro_covid$posixdate)) - textposition,
       y = ymax_today * 0.05,
       cex = 0.85, 
       labels = c("Note: data under-reported due to lack of testing."))
  
  return(0)
}


cumulative_cases_plot <- function(dfw_metro_covid,
                                  nyt_metro_covid_match,
                                  area_label) {
  dailymax <- ceiling(max(dfw_metro_covid$active_cases)/1000) * 1000
  
  covid_dead <- aggregate(x = dfw_metro_covid$total_dead,
                          FUN = sum,
                          by = list(dfw_metro_covid$posixdate)
  )
  names(covid_dead) <- c('posixdate', 'total_dead')
  covid_dead <- rbind(covid_dead,
                      data.frame(posixdate = max(covid_dead$posixdate), 
                                 total_dead = 0
                      )
  )
  covid_dead <- rbind(covid_dead,
                      data.frame(posixdate = min(covid_dead$posixdate),
                                 total_dead = 0
                      )
  )
  
  plot(dfw_metro_covid$posixdate, 
       dfw_metro_covid$active_cases, 
       type = "l",
       main = sprintf('%s Confirmed Cases of COVID-19', area_label),
       xlab = "", 
       ylab = "Confirmed Cases",
       ylim = c(0, dailymax)
  )
  
  lines(covid_dead$posixdate,
        covid_dead$total_dead,
        lty = 1,
        lwd = 2,
        col = "#880000"
  )
  
  polygon(covid_dead$posixdate,
          covid_dead$total_dead,
          col = "#bb7777"
  )
  
  lines(nyt_metro_covid_match$posixdate,
        nyt_metro_covid_match$cases,
        col = "gray50",
        lty = 2)
  
  legend(x = min(dfw_metro_covid$posixdate),  y = dailymax - 100,
         legend = c('New York Times', 'Johns Hopkins Univ.'), 
         lty = c(2, 1),
         lwd = c(3, 3),
         col = c("gray50", "black"))
  
  text(x = as.numeric(max(dfw_metro_covid$posixdate)),
       y = max(dfw_metro_covid$total_dead) + dailymax * 0.03,
       adj = c(1,0), 
       labels = c(sprintf("Total dead: %d", max(dfw_metro_covid$total_dead)))
  )
  
  return(0)
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
                   colClasses = col_classes
    )
    f1$date <- as.Date(strsplit(filename, '\\.')[[1]][1], format = "%m-%d-%Y")
    covid <- rbind(covid, f1)
  }
  return(covid)
}


get_texas_metro_county_list <- function(fipsurl){
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
  txfips <- read.xls(xls = fipsurl,
                     sheet = 'Sheet1',
                     colClasses = txfips_colclasses
                    )
  names(txfips) <- txfips_colnames
  return(txfips)
}


get_metro_fips <- function(fipslist, msa_name){
  metro_i <- which(fipslist$MSA == msa_name)
  metro_fips <- as.numeric(fipslist$FIPS[metro_i])
  return(metro_fips)
}


get_metro_fips_locally <- function(fipsdir = Sys.getenv('HOME'), 
                                   msa_name = 'Dallas-Fort Worth-Arlington',
                                   fipsfilename = 'PHR_MSA_County_masterlist.csv'){
  
  txfips <- read.csv(paste(fipsdir, fipsfilename, sep='/'),
                     header = TRUE,
                     stringsAsFactors = FALSE
                    )
  dfw_i <- which(txfips$Metropolitan.Statistical.Area..MSA. == msa_name)
  dfw_fips <- txfips$FIPS..[dfw_i]
  return(dfw_fips)
}

# EOF