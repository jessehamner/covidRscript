################################################################################
# covidfunctions.R: a small R function library to use the JHU COVID-19 data and 
# New York Times data on the US and global pandemic of 2020.
#
# Jesse Hamner, 2020--2021
#
################################################################################

library(httr)
library(curl)

source('covid_data_retrieval_functions.R')


make_country_subset <- function(dataset, cvar, cname) {
  country_subset <- dataset[which(dataset[[cvar]] == cname),]
  return(country_subset)
}


do_country_plot <- function(countryname, inputjhu) {
  
  
  return(TRUE)
}


do_state_plots <- function(stfips, 
                           inputjhu,
                           inputnyt,
                           stfipslist,
                           sourcename="") {
  if(sourcename == "") {
    sourcename <- "Source: Johns Hopkins Univ. Center for Systems Science and Engineering"
  }
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
    

  plot_7day_ma(state = stname,
               dataset = state_level,
               sourcename = sourcename)
  
#  plot_daily_increase(state = stname,
#                      dataset = state_level,
#                      sourcename = sourcename)
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

plot_daily_increase <- function(state, dataset, 
                                lookback_days = 14,
                                sourcename = "Source: Johns Hopkins Univ. Center for Systems Science and Engineering") {
  message(sprintf('Writing daily case increase graphic for %s', state))
  basic_plot(sprintf('%s_covid19_confirmed_daily_increase.png', gsub(' ', '_', state)),
             daily_increase_plot(dataset,
                                 metro_label = state,
                                 lookback_days = lookback_days,
                                 sourcename = sourcename)
  )
  return (0)
}


plot_7day_ma <- function(state, 
                         dataset, 
                         lookback_days = 14,
                         sourcename = "Source: Johns Hopkins Univ. Center for Systems Science and Engineering") {
  message(sprintf('plot_7day_ma(): Writing daily case increase graphic for %s', state))
  basic_plot(sprintf('%s_covid19_confirmed_daily_increase.png', gsub(' ', '_', state)),
             sevenday_mov_avg_plot(dataset,
                                   metro_label = state,
                                   lookback_days = lookback_days,
                                   sourcename = sourcename)
  )
  return (0)
}



plot_cumulative_cases <- function(state, jhu_data, nyt_data){
  message(sprintf('Writing daily confirmed cases graphic for %s', state))
  basic_plot(sprintf('%s_covid19_confirmed_daily_active_cases.png', gsub(' ', '_', state)),
             cumulative_cases_plot(jhu_data, 
                                   nyt_metro_covid_match = nyt_data, 
                                   area_label = state )
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


make_metro_map <- function(countiesmap, msa_name, msalist, 
                           varname = 'CBSATitle') {

  msa_fips <- get_metro_fips_2(msalist, msa_name=msa_name, varname=varname)
  countiesmap$newfips <- sprintf('%s%s', 
                                 countiesmap$STATEFP, 
                                 countiesmap$COUNTYFP)
  return(countiesmap[sapply(X = seq(1, nrow(msa_fips)), 
                            FUN = function(x){ 
                              which(countiesmap$newfips == msa_fips$newfips[x])
                            }),]
        )
}


# a tad long to be an anonymous function, so defined separately and more flexibly here.
return_rows <- function(statefpname = 'stfips', cofpname = 'cofips', inputdf, msa_fips, recno) {
  answer <- which(inputdf[[statefpname]] == msa_fips$stfips[recno] & 
                  inputdf[[cofpname]] == msa_fips$cofips[recno]
                 )
  return(answer)  
}


make_metro_subset <- function(inputdf, cofipslist) {
  
  # Check both state and county per each row in cofipslist,
  # as some metro areas cross state boundaries.
  covid_rows <- unlist(sapply(X = seq(1, nrow(cofipslist)),
                       FUN = function(x) {
                         which(as.numeric(inputdf[["stfips"]]) == as.numeric(cofipslist$stfips[x]) & 
                               as.numeric(inputdf[["cofips"]]) == as.numeric(cofipslist$cofips[x])
                              )
                       }
                      ))
  
  metro_covid_base <- inputdf[covid_rows,]
  
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



make_state_subset <- function(inputdf, state_map) {
  cofipslist <- unique(inputdf$cofips)
  
  # Check county fips or combined FIPS variable for each row in cofipslist
  covid_rows <- sapply(X = seq(1,length(cofipslist)),
                       FUN = function(x) { 
                           which(as.numeric(inputdf[["cofips"]]) == as.numeric(state_map$COUNTYFP[x]))
                       }
                      )
  
  metro_covid_base <- inputdf[covid_rows,]
  metro_covid_base$posixdate <- as.Date(metro_covid_base$date, format = "%Y-%m-%d")
  metro_covid <- aggregate(x = metro_covid_base$Confirmed,
                           FUN = sum,
                           by = list(metro_covid_base$posixdate,
                                     metro_covid_base$cofips)
                          )
  names(metro_covid) <- c('posixdate', 'cofips', 'Confirmed')
  
  metro_covid$new_today <- ave(metro_covid$Confirmed, metro_covid$cofips,
                               FUN = function(x) c(0, diff(x)))
  
  metro_covid$total_dead <- aggregate(x = metro_covid_base$Deaths,
                                      FUN = sum,
                                      by = list(metro_covid_base$posixdate, 
                                                metro_covid_base$cofips))$x

  metro_covid$active_cases <- metro_covid$Confirmed - metro_covid$total_dead
  
  # Now we have county-level data for each day. Can push it into the map.
  return(metro_covid)
}


make_state_data <- function(inputdf, stfips = '48') {
  #stfips <- as.numeric(stfips)
  message(sprintf('Making state-level data for state FIPS %s', stfips))

  st_covid_base <- inputdf[which(inputdf$stfips == stfips),]
  
  if (length(st_covid_base) == 0) {
    message('No usable data in JHU data subset.')
    return(FALSE)
  }
  if (nrow(st_covid_base) < 2) {
    message('No usable data in JHU data subset.')
    return(FALSE)
  }
  
  # st_covid_base$posixdate <- as.Date(st_covid_base$date, format = "%Y-%m-%d")
  st_covid <- aggregate(x = st_covid_base$Confirmed,
                        FUN = sum,
                        by = list(st_covid_base$posixdate))
  names(st_covid) <- c('posixdate', 'Confirmed')
  st_covid$new_today <- ave(st_covid$Confirmed,
                            FUN = function(x) c(0, diff(x)))
  
  st_covid$total_dead <- aggregate(x = st_covid_base$Deaths,
                                   FUN = sum,
                                   by = list(st_covid_base$posixdate))$x
  
  st_covid$total_dead[which(is.na(st_covid$total_dead))] <- 0
  st_covid$Confirmed[which(is.na(st_covid$Confirmed))] <- 0
  st_covid$total_dead[which(is.null(st_covid$total_dead))] <- 0
  st_covid$Confirmed[which(is.null(st_covid$Confirmed))] <- 0
  
  st_covid$active_cases <- st_covid$Confirmed - st_covid$total_dead
  
  st_covid <- st_covid %>% 
    dplyr::mutate(newcases_07da = zoo::rollmean(new_today, k=7, fill=NA)) %>% 
    dplyr::ungroup()
  
  st_covid <- st_covid[which(!is.na(st_covid$newcases_07da)),]
  
  return(st_covid)
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


longest_improvement <- function(metro_covid, min_days = 14) {
  alldates <- seq(range(metro_covid$posixdate)[1], range(metro_covid$posixdate)[2], 1)
  nrecords <- nrow(metro_covid)
  best_day <- metro_covid$posixdate[1]
  best_slope <- lm(metro_covid$new_today ~ metro_covid$posixdate)$coef[2]
  
  if (nrow(metro_covid) - min_days <= 1) {
    return(FALSE)
  }
  
  for (i in seq(1, (nrow(metro_covid) - min_days))) {
    seqlength <- min_days
    moving_window <- i + seqlength
    testslope <- lm(metro_covid$new_today[i:moving_window] ~ metro_covid$posixdate[i:moving_window])$coef[2]
    # message(sprintf('%s) Duration: %s days; slope: %0.1f',i, seqlength, testslope))
    if (testslope < best_slope) {
      best_slope <- testslope
      best_day <- i
    }
  
  }
  message(sprintf('%s) Best slope: %0.1f -- for %s consecutive days.', best_day, best_slope, seqlength))
  
  return(c(best_day, seqlength, best_slope)) 
}


daily_increase_plot <- function(metro_covid,
                                metro_label,
                                lookback_days = 14, 
                                sourcename="Source: Johns Hopkins Univ. Center for Systems Science and Engineering"){
  ymax_today <- max(metro_covid$new_today) * 1.2
  #format(metro_covid$new_today,scientific=FALSE)
  options(scipen=5)
  plot(metro_covid$posixdate, 
       metro_covid$new_today, 
       type = "l",
       main = sprintf('%s\nDaily New Confirmed Cases of COVID-19', metro_label),
       xlab = "",
       ylab = "New Cases Each Day",
       ylim = c(0, ymax_today),
       cex.axis = 0.9,
       cex.lab = 0.9
  )
  
  daily_regression <- lm(metro_covid$new_today ~ metro_covid$posixdate)
  slope_label <- sprintf("%0.1f", daily_regression$coefficients[[2]])
  last_week <- metro_covid[which(metro_covid$posixdate >= (max(metro_covid$posixdate) - lookback_days)),]
  daily_regression_last_week <- lm(last_week$new_today ~ last_week$posixdate)
  slope_last_week_label <- sprintf("%0.1f", daily_regression_last_week$coefficients[[2]])

  abline(daily_regression, col = '#6666bb', lty = 2, lwd = 3)
  
  lines(last_week$posixdate,
        predict.lm(daily_regression_last_week),
        col = '#aa6666',
        lwd = 3
  )
  
  
  
  best_params <- longest_improvement(metro_covid, lookback_days)
  if (is.na(best_params[1]) || is.na(best_params[2])) {
    return(FALSE)
  }
  best_period <- metro_covid[best_params[1]:(best_params[1] + best_params[2]),]
  
  best_daily_regression <- lm(best_period$new_today ~ best_period$posixdate)
  lines(best_period$posixdate,
        predict.lm(best_daily_regression),
        col = '#66aa66',
        lwd = 3
  )
  best_slope_label <- sprintf("%0.1f", best_daily_regression$coefficients[[2]])
  
  legend(x = as.numeric(min(metro_covid$posixdate)) - 0.5,
         y = ymax_today,
         legend = c(sprintf("Average daily acceleration: %s cases", slope_label), 
                    sprintf("Avg. acceleration (last %s days): %s cases", 
                            lookback_days, slope_last_week_label
                           ),
                    sprintf("Avg. acceleration (best %s days): %s cases", 
                            lookback_days, best_slope_label
                    )
                   ), 
         lty = c(2, 1, 1), 
         lwd = c(3, 3, 3), 
         col = c("#6666bb", "#aa6666", "#66aa66"))
  
  
  textposition <- (as.numeric(max(metro_covid$posixdate)) - as.numeric(min(metro_covid$posixdate))) * 0.5
  xmax <- as.numeric(max(metro_covid$posixdate))
  xmin <- as.numeric(min(metro_covid$posixdate))
  framewidth <- xmax - xmin
  
  rect(xleft = (xmin + (framewidth * 0.125)), 
       xright = (xmax - (framewidth * 0.125)),
       ytop = (ymax_today * 0.09),
       ybottom = (0 - (ymax_today * 0.02)),
       col="#f9f9f9dd", 
       border = "#ccccccee")
  text(x = as.numeric(max(metro_covid$posixdate)) - textposition,
       y = 0,
       labels = c(sourcename),
       cex = 0.5,
       font = 3
  )
  text(x = as.numeric(max(metro_covid$posixdate)) - textposition,
       y = ymax_today * 0.05,
       cex = 0.85, 
       labels = c()
       # labels = c("Note: data under-reported due to lack of testing.")
  )
  
  return(0)
}


sevenday_mov_avg_plot <- function(metro_covid,
                                  metro_label,
                                  lookback_days = 14, 
                                  sourcename="Source: Johns Hopkins Univ. Center for Systems Science and Engineering"){
  
  ymax_today <- max(metro_covid$newcases_07da) * 1.3

  metro_covid <- metro_covid[which(!is.na(metro_covid$posixdate)), ]
    
  xmax <- as.numeric(max(metro_covid$posixdate))
  xmin <- as.numeric(min(metro_covid$posixdate))
  framewidth <- xmax - xmin
  textposition <- 0.5 * framewidth
  #format(metro_covid$new_today,scientific=FALSE)
  options(scipen=5)
  plot(metro_covid$posixdate, 
       metro_covid$newcases_07da, 
       type = "l",
       main = sprintf('%s\n7-day moving average of new COVID-19 cases', metro_label),
       xlab = "",
       ylab = "New Cases Each Day, Avg.",
       ylim = c(0, ymax_today),
       cex.axis = 0.9,
       cex.lab = 0.9
  )
  

  daily_regression <- lm(metro_covid$newcases_07da ~ metro_covid$posixdate)
  slope_label <- sprintf("%0.1f", daily_regression$coefficients[[2]])
  last_week <- metro_covid[which(metro_covid$posixdate >= (max(metro_covid$posixdate) - lookback_days)),]
  daily_regression_last_week <- lm(last_week$newcases_07da ~ last_week$posixdate)
  slope_last_week_label <- sprintf("%0.1f", daily_regression_last_week$coefficients[[2]])
  
  abline(daily_regression, col = '#6666bb', lty = 2, lwd = 3)
  lines(last_week$posixdate,
        predict.lm(daily_regression_last_week),
        col = '#aa6666',
        lwd = 3
  )
  
  best_params <- longest_improvement(metro_covid, lookback_days)
  if (is.na(best_params[1]) || is.na(best_params[2])) {
    return(FALSE)
  }
  best_period <- metro_covid[best_params[1]:(best_params[1] + best_params[2]),]
  best_daily_regression <- lm(best_period$newcases_07da ~ best_period$posixdate)
  lines(best_period$posixdate,
        predict.lm(best_daily_regression),
        col = '#66aa66',
        lwd = 3
  )
  best_slope_label <- sprintf("%0.1f", best_daily_regression$coefficients[[2]])
  
  # Draw the legend:
  legend(x = as.numeric(min(metro_covid$posixdate)) - 0.5,
         y = ymax_today,
         legend = c(sprintf("Average daily acceleration: %s cases",
                            slope_label
                    ), 
                    sprintf("Avg. acceleration (last %s days): %s cases", 
                            lookback_days, slope_last_week_label
                    ),
                    sprintf("Avg. acceleration (best %s days): %s cases", 
                            lookback_days, best_slope_label
                    )
         ), 
         lty = c(2, 1, 1), 
         lwd = c(3, 3, 3), 
         col = c("#6666bb", "#aa6666", "#66aa66"))
  
  rect(xleft = xmin + (framewidth * 0.125), 
       xright = xmax - (framewidth * 0.125),
       ytop = ymax_today * 0.03,
       ybottom = - ymax_today * 0.02,
       col="#f9f9f9bb", 
       border = "#ccccccee"
      )
  text(x = xmax - textposition,
       y = 0,
       labels = c(sourcename),
       cex = 0.5,
       font = 3
      )
  text(x = xmax - textposition,
       y = ymax_today * 0.05,
       cex = 0.85, 
       labels = c()
       # labels = c("Note: data under-reported due to lack of testing.")
      )
  
  return(0)
}



holt_winters_smoothing <- function(covid) {
  # Trend lines are one thing. Smoothing out regular spikes
  # (cf the "Tuesday Effect") is another. Holt-Winters is an uncomplicated 
  # smoothing and forecasting model transformation.
  
  # level component:
  alpha_param <- NULL
  
  # trend component:
  beta_param <- NULL
  
  # seasonal component:
  gamma_param <- NULL
  
  start_periods <- 7
  frequency_param <- 7
  ts_start <- min(covid$posixtime)
  ts_end <- max(covid$posixtime) 
  time_series <- ts(covid, start = ts_start, end = ts_end, frequency = frequency_param) 
  
  fit <- HoltWinters(time_series, alpha = alpha_param, beta = beta_param, gamma = gamma_param)
  
  return(fit)
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


assign_msa <- function(dataframe, msalist) {
  # assign CBSACode, CBSATitle, CSACode, CSATitle, and Central_or_Outlying_County
  # based on stfips and cofips values (note they are text fields) in dataframe
  
  dataframe$CBSACode <- NA
  dataframe$CSACode <- NA
  dataframe$CBSATitle <- NA
  dataframe$CSATitle <- NA
  dataframe$Central_or_Outlying_County <- NA
  ucs <- unique(dataframe$FIPS)
  for(i in seq(1, nrow(msalist))) {
    if(msalist$FIPS[i] %in% ucs) {
      right_rows <- which(dataframe$FIPS == msalist$FIPS[i])
      dataframe$CBSACode[right_rows] <- msalist$CBSACode[i]
      dataframe$CBSATitle[right_rows] <- msalist$CBSATitle[i]
      dataframe$CSACode[right_rows] <- msalist$CSACode[i]
      dataframe$CSATitle[right_rows] <- msalist$CSATitle[i]
      dataframe$Central_or_Outlying_County <- msalist$Central_or_Outlying_County[i]
    }
  }

  return(dataframe)
}
  



# For Texas only, at the present moment
get_metro_fips <- function(fipslist, msa_name, statefips='48'){
  
  metro_i <- which(fipslist$MSA == msa_name)
    # metro_fips <- as.numeric(fipslist$FIPS[metro_i])
  metro_fips <- subset(x = fipslist[metro_i,], 
                       select = which(names(fipslist) %in% c('FIPS'))
                      )
  metro_fips$stfips <- statefips
  names(metro_fips) <- c('cofips', 'stfips')
  return(metro_fips)
}


# Some MSAs cross state lines:
get_metro_fips_2 <- function(msafips, msa_name, varname = 'CBSATitle'){
  msa_subset <- msafips[which(msafips[[varname]] == msa_name),]
  retval <- subset(msa_subset, select=c('stfips', 'cofips'))
  retval$newfips <- sprintf('%s%s', retval$stfips, retval$cofips)
  return(retval)
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


make_metro_plots <- function(areaname, 
                             countysubset,
                             sourcename,
                             jhudata,
                             nytdata,
                             stfips,
                             lookback_days) {
  
  metro_covid <- make_metro_subset(inputdf = jhudata, 
                                   cofipslist = countysubset)
  message('Metro subset dates:')
  message(sprintf("%s,\n", metro_covid$posixdate))
  nyt_metro <- nyt_subset(nytdata = nytdata,
                          stfips = stfips,
                          countysubset = as.numeric(countysubset$cofips)
  )
  plot_daily_increase(state = areaname,
                      dataset = metro_covid,
                      lookback_days = lookback_days
  )
  plot_cumulative_cases(state = areaname,
                        jhu_data = metro_covid,
                        nyt_data = nyt_metro
  )
  
  return(TRUE)  
}


country_plot <- function(countryname,
                         iso3abbr,
                         jhudata,
                         ecdcdata,
                         lookback_days,
                         sourcename) {
  
  country_subset_1 <- make_country_subset(jhudata, 'Country_Region', countryname)
  country_subset_2 <- make_country_subset(ecdcdata, 'countryterritoryCode', iso3abbr)
  
  country_subset_2$month <- strftime(strptime(country_subset_2$dateRep, format="%d/%m/%Y"), "%m")
  country_subset_2$day <- strftime(strptime(country_subset_2$dateRep, format="%d/%m/%Y"), "%d")
  country_subset_2$year <- strftime(strptime(country_subset_2$dateRep, format="%d/%m/%Y"), "%Y")
  
  country_subset_2$posixdate <- as.Date(sprintf("%s-%s-%s", country_subset_2$month,
                                                country_subset_2$day,
                                                country_subset_2$year),
                                        format = "%m-%d-%Y")
  co_covid <- aggregate(x = country_subset_2$cases,
                        FUN = sum,
                        by = list(country_subset_2$posixdate))
  names(co_covid) <- c('posixdate', 'Confirmed')
  co_covid$deaths <- aggregate(x = country_subset_2$deaths,
                               FUN = sum,
                               by = list(country_subset_2$posixdate))$x
  names(co_covid) <- c('posixdate', 'new_today', 'deaths')
  
  plot_daily_increase(state = countryname, 
                      dataset = co_covid, 
                      lookback_days = lookback_days,
                      sourcename = sourcename)
  
  
  # plot overall cases and deaths:
  #plot_cumulative_cases(state = stname,
  #                      jhu_data = state_level,
  #                      nyt_data = nyt_state_level)
  
  return(TRUE)  
}


make_metro_map_generic <- function(countiesmap, msa_name, msalist, covid_data) {
  metro_fips <- get_metro_fips_2(msalist, 
                                 msa_name = msa_name, 
                                 varname = 'CBSATitle'
  )
  met_co_map <- make_metro_map(countiesmap = countiesmap,
                               msa_name = msa_name,
                               msalist = msalist
  )
  
  # Aggregated data for the entire MSA. COVID-19 data broken out by counties
  m_c_3_data_rows <- sapply(X = seq(1, nrow(metro_fips)),
                            FUN = function(x) { 
                              which(covid_data$stfips == metro_fips$stfips[x] & 
                                      covid_data$cofips == metro_fips$cofips[x]
                              )
                            }, simplify=TRUE)
  
  if(is.integer(m_c_3_data_rows)) {
    message(sprintf("There are %g rows in the metro_covid3 subset.", nrow(m_c_3_data_rows)))
  }
  if(is.list(m_c_3_data_rows)) {
    message(sprintf("The metro_covid3 subset has a problem, and thinks it should be a list of length %g", length(m_c_3_data_rows)))
    m_c_3_data_rows <- unlist(m_c_3_data_rows)   # ugly fix.
  }
  
  # COVID-19 data broken out by counties:
  message('Subsetting metro_covid3 with the rows in m_c_3_data_rows.')
  metro_covid3 <- covid_data[m_c_3_data_rows,]
  
  cv_m_yest <- metro_covid3[which(metro_covid3$date == max(metro_covid3$date)),]
  met_co_map$Confirmed <- 0
  met_co_map$Confirmed <- cv_m_yest$Confirmed[unlist(sapply(X = seq(1, nrow(met_co_map)),
                                                     FUN = function(x) {
                                                       which(cv_m_yest$cofips == met_co_map$COUNTYFP[x] & 
                                                             cv_m_yest$stfips == met_co_map$STATEFP[x]
                                                            )
                                                     }
                                                    ))
                                             ]
  
  met_co_map$Confirmed[which(is.na(met_co_map$Confirmed))] <- 0
  uspopbycounty <- get_us_population_by_county(year = 2019)
  
  met_co_map$Population <- 0
  met_co_map$Population <- 
    uspopbycounty$POPESTIMATE2019[sapply(X = seq(1,nrow(met_co_map)), 
                                         FUN = function(x) {
                                           which(uspopbycounty$stfips == met_co_map$STATEFP[x] & 
                                                 uspopbycounty$cofips == met_co_map$COUNTYFP[x]
                                                )
                                         })]
  
  met_co_map$percent_infected <- 0
  met_co_map$percent_infected <- 100 * (met_co_map$Confirmed / met_co_map$Population)
  
  return(met_co_map)
}


metro_map_plot <- function(metrocountymap, 
                           todaytitle, 
                           todaysubtitle, 
                           max_infected_pct) {
  theplot <- ggplot() +
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_sf(data = metrocountymap,
            aes(fill = percent_infected)
    ) + 
    scale_fill_gradient2(high= highcolor,
                        mid = midcolor,
                        low = lowcolor,
                        limits = c(0, max_infected_pct),
                        na.value = "#aaaaaa",
                        space="Lab",
                        midpoint=max_infected_pct/2
    ) + 
    ggtitle(label = todaytitle,
            subtitle = todaysubtitle
    ) +
    coord_sf(label_axes = list(bottom = "Longitude",
                               left = "Latitude")
    ) + 
    geom_sf_label(data = metrocountymap,
                  na.rm = TRUE,
                  mapping = aes(label = Confirmed,
                                geometry = geometry
                               )
    ) +
    guides(fill = guide_colorbar(title="Percent\nInfected")) + 
    theme(legend.justification=c(0.0, 0.0),
          legend.position=c(0.89, 0.02)
    ) + 
    labs(x = "Longitude", y = "Latitude") +
    geom_sf_label(data = metrocountymap,
                  na.rm = TRUE, 
                  nudge_y = 0.09,
                  mapping = aes(label = NAME,
                                geometry = geometry
                  ),
                  color = "gray40",
                  fill = "#ffffdd"
    )
  return(theplot)
}


make_complete_metro_plot <- function(msalist,
                                     msa_name,
                                     uscountiesmap,
                                     covid_data,
                                     dest_dir,
                                     max_infected_pct,
                                     varname='CBSATitle') {
  
  setwd(dest_dir)
  message(sprintf("MSA: %s", msa_name))
  metro_fips <- get_metro_fips_2(msalist, 
                                 msa_name = msa_name, 
                                 varname = 'CBSATitle')
  metrocountymap <- make_metro_map_generic(countiesmap = uscountiesmap,
                                           msa_name = msa_name,
                                           msalist = msalist, 
                                           covid_data = covid_data
                                          )
  
  todaytitle <- sprintf("%s COVID-19 Cases as of %s", msa_name, Sys.Date())
  todaysubtitle <- sprintf("and Percent of County Population Infected")
  metro_plot <- metro_map_plot(metrocountymap, 
                               todaytitle, 
                               todaysubtitle, 
                               max_infected_pct=max_infected_pct)
  todaymapfilename <- sprintf("%s_covid19_metromap_%s.png", 
                              gsub(' ', '', gsub('-|,', '', msa_name)), Sys.Date())
  message(sprintf("Creating map: %s", todaymapfilename))
  ggsave(todaymapfilename, plot=metro_plot)
  
  return(TRUE)
}


make_state_map <- function(uscountiesmap, state_code, statefipscodes) {
  statename <- statefipscodes$STATE_NAME[which(statefipscodes$STATE == as.character(state_code))]
  todaytitle <- sprintf("%s COVID-19 County Map as of %s", statename, Sys.Date())
  todaysubtitle <- sprintf("with Percent of County Population Infected")
  message(sprintf("Statewide map for %s", statename))
  state_map <- uscountiesmap[which(uscountiesmap$STATEFP == state_code),]
  state_map$infected_pct <- NA
  state_map$infected_pct <- sapply(X=state_map$GEOID, 
                                   FUN=function(x) {
                                     covid3$percent_infected[which(covid3$FIPS==x & covid3$posixdate == max(covid3$posixdate))]
                                   }
  )
  
  state_map$rounded_percent <- round(state_map$infected_pct, digits = 0)
  bad <- which(state_map$rounded_percent > 60)
  worst <- which(state_map$rounded_percent == round(max(state_map$infected_pct), digits=0))
  least <- which(state_map$rounded_percent == round(min(state_map$infected_pct), digits=0))
  state_map$county_label <- ""
  state_map$county_label[least] <- sprintf("%g%%", state_map$rounded_percent[least])
  state_map$county_label[bad] <- sprintf("%g%%", state_map$rounded_percent[bad])
  state_map$county_label[worst] <- sprintf("%g%%", state_map$rounded_percent[worst])
  
  for (i in seq(worst)) {
    if(state_map$rounded_percent[i] > 100){
      state_map$county_label[i] <- ">100%"
    }
  }
  
  stmap1 <- state_map[which(state_map$county_label != ""),]
  
  state_county_plot <- ggplot() +
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_sf(data = state_map,
            aes(fill = infected_pct)
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
    geom_sf_label(data = stmap1,
                  aes(label = county_label, geometry = geometry),
                  nudge_y = 0.09
    ) +
    guides(fill = guide_colorbar(title="Percent\nInfected")) + 
    theme(legend.justification=c(0.0, 0.0),
          legend.position=c(0.89, 0.02)
    ) + 
    labs(x = "", y = "")  ### +
  #geom_sf_label(data = state_map,
  #              na.rm = TRUE, 
  #              nudge_y = 0.09,
  #              mapping = aes(label = NAME,
  #                            geometry = geometry
  #              ),
  #              color = "gray40",
  #              fill = "#ffffdd"
  #)
  todaymapfilename <- sprintf("%s_covid19_statemap_%s.png", 
                              gsub(' ', '_', statefipscodes$STATE_NAME[which(statefipscodes$STATE == state_code)]),
                              Sys.Date()
  )
  ggsave(todaymapfilename, plot=state_county_plot)
  return(state_county_plot)
}

# EOF