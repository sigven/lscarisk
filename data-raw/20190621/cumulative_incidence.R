#' Function that estimates cumulative risk(incidence) for a given gene/gender/cancer and age_start/age_stop
#'
#' @param air_df_raw data frame with annual incidence rate data (AIR) along with gene/gender/cancertype data
#' @param qsex gender for which AIR has been calculated
#' @param qcancer cancer type (ICD9-ish code)
#' @param age_start start age for cumulative incidence calculation
#' @param age_stop stop age for cumulative incidence calculation
#' @param qgene gene
#' @param age_marks age intervals
#' @param percentage logical indicating if risk is expressed in percent
#'
#' @return cumulative incidence geom_histogram plot from ggplot2
#'
#'
cumulative_risk_estimate <- function(air_df_raw, qsex = 'FEMALE', qcancer = '174', age_start = 25, age_stop = 75, qgene = 'MLH1', num_significant_digits = 3, percentage = T){
  
  if(!('sex' %in% colnames(air_df_raw))){
    rlogging::stop("Missing required column \'sex\' in raw table with annual incidence data")
  }
  if(!('cancer' %in% colnames(air_df_raw))){
    rlogging::stop("Missing required column \'cancer\' in raw table with annual incidence data")
  }
  if(!('age' %in% colnames(air_df_raw))){
    rlogging::stop("Missing required column \'age\' in raw table with annual incidence data")
  }
  if(!('gene' %in% colnames(air_df_raw))){
    rlogging::stop("Missing required column \'gene\' in raw table with annual incidence data")
  }
  if(!('air' %in% colnames(air_df_raw))){
    rlogging::stop("Missing required column \'air\' in raw table with annual incidence data")
  }
  if(!('time_years' %in% colnames(air_df_raw))){
    rlogging::stop("Missing required column \'time_years\' in raw table with annual incidence data")
  }
  if(!('events_cases' %in% colnames(air_df_raw))){
    rlogging::stop("Missing required column \'events_cases\' in raw table with annual incidence data")
  }
  
  df_web <- dplyr::filter(air_df_raw, sex == qsex & cancer == qcancer & age >= age_start & age <= age_stop & gene == qgene)
  if(nrow(df_web) > 1){
    df_web$tmp <- rep(1,nrow(df_web))
    for (i in 2:nrow(df_web)) {
      df_web$tmp[i] <- df_web$tmp[i - 1] - (df_web$tmp[i - 1] * df_web$air[i - 1])
    }
    df_web$cum_risk_percent_dynamic <- (1 - df_web$tmp) * 100
  }
  else{
    df_web$cum_risk_percent_dynamic <- df_web$air * 100
  }
  
  tmp1 <- df_web %>% dplyr::filter(age == age_stop)
  cum_incidence_web <- round(as.numeric(tmp1$cum_risk_percent_dynamic),digits = num_significant_digits)
  
  ## Set risk at start age to zero
  if(age_stop == 25){
    cum_incidence_web <- 0
  }

  return(cum_incidence_web)
}

