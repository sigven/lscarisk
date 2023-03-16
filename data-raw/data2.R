library(dplyr)
library(magrittr)

load('ls_risk_data.rda')
load('ls_risk_data_subsequent.rda')
load('ls_organ.rda')


get_cumulative_incidence_CI <- function(lsrisk_df, sex = 'FEMALE', cancer = 'CRC', age_start = 25, age_stop = 70, gene = 'MLH1', percentage = F, num_digits = 7){
  
  ### TEST RESULTS - AS SHOWN ON WEBSITE
  df_web <- dplyr::filter(lsrisk_df, Sex == sex & Cancer == cancer & Age >= age_start & Age <= age_stop & Gene == gene)      
  if(nrow(df_web) > 1){
    df_web$tmp <- rep(1,nrow(df_web))        
    for (i in 2:nrow(df_web)) {
      df_web$tmp[i] <- df_web$tmp[i - 1] - (df_web$tmp[i - 1] * df_web$Annual_incidence_rate[i - 1])
    }
    df_web$cum_risk_percent_dynamic <- (1 - df_web$tmp) * 100
  }
  else{
    df_web$cum_risk_percent_dynamic <- df_web$Annual_incidence_rate * 100
  }
  
  tmp1 <- df_web %>% dplyr::filter(Age == age_stop)
  cum_incidence_web <- tmp1$cum_risk_percent_dynamic
  #### END RESULT - WEB
  
  
  ### get annual incidence rates (air) per 5 year interval
  age_marks <- c(25,30,35,40,45,50,55,60,65,70,75)
  air_df <- dplyr::filter(lsrisk_df, Sex == sex & Gene == gene & Cancer == cancer & Age %in% age_marks)
  organ <- unique(air_df$Organ)
  air_df$air <- air_df$Cases_5int / air_df$Years_5int
  if(nrow(air_df[is.nan(air_df$air),]) > 0){
    air_df[is.nan(air_df$air),]$air <- 0
  }
  
  ## standard error
  air_df$air_SE <- sqrt(air_df$air * (1 - air_df$air)/air_df$Years_5int)
  k <- 1
  while(k <= nrow(air_df)){
    if(is.nan(air_df[k,]$air_SE)){
      air_df[k,]$air_SE <- 0
    }
    k <- k + 1
  }
  
  ### upper and lower limits of 95% confidence intervals for AIR estimate
  air_df$air_hi <- air_df$air + 1.96 * air_df$air_SE
  air_df$air_lo <- air_df$air - 1.96 * air_df$air_SE
  
  ### initialize cumulative estimate figures for each 5 year interval
  air_df$cum_n <- rep(0,nrow(air_df))
  air_df$cum_ln1 <- rep(0,nrow(air_df))
  air_df$cum_nln <- rep(0,nrow(air_df))
  air_df$cum_lnSE <- rep(0,nrow(air_df))
  air_df$cum_lnVar <- rep(0,nrow(air_df))
  
  ### get number of years in each interval
  n <- 1
  cum_n <- c()
  while(n <= nrow(air_df)){
    tmp <- max(0,min(air_df[n,]$Age + 5, age_stop) - max(air_df[n,]$Age,age_start))
    cum_n <- append(cum_n,tmp)
    n <- n + 1
  }
  air_df$cum_n <- cum_n
  
  test_n <- sum(air_df$cum_n)
  
  air_df$cum_ln1 <- -log(1 - air_df$air)
  air_df$cum_nln <- air_df$cum_n * air_df$cum_ln1
  air_df$cum_lnSE <- air_df$cum_n * air_df$air_SE / (1 - air_df$air)
  air_df$cum_lnVar <- air_df$cum_lnSE^2
  
  ## summarised over all years (from age_start to age_stop)
  final_cum_estimate <- 1 - exp(- sum(air_df$cum_nln))
  final_cum_estimate_SE <- sqrt(sum(air_df$cum_lnVar)) * (1 - final_cum_estimate)
  
  ## confidence interval of cumulative incidence
  final_cum_estimate_lo <- final_cum_estimate - 1.96 * final_cum_estimate_SE
  final_cum_estimate_hi <- final_cum_estimate + 1.96 * final_cum_estimate_SE
  
  #cat(cancer,gene,sex,final_cum_estimate_hi,final_cum_estimate_lo,sep="\t","\n")
  
  truncated_lo <- FALSE
  truncated_hi <- FALSE
  
  if(final_cum_estimate_lo < 0){
    final_cum_estimate_lo <- 0
    truncated_lo <- TRUE
  }
  if(final_cum_estimate_hi > 1){
    final_cum_estimate_hi <- 1
    truncated_hi <- TRUE
  }
  
  #final_cum_estimate <- round(final_cum_estimate,num_digits)
  #final_cum_estimate_lo <- round(final_cum_estimate_lo,num_digits)
  #final_cum_estimate_hi <- round(final_cum_estimate_hi,num_digits)
  
  if(percentage == TRUE){
    final_cum_estimate <- sprintf("%.4f",final_cum_estimate * 100)
    final_cum_estimate_lo <- sprintf("%.4f",final_cum_estimate_lo * 100)
    final_cum_estimate_hi <- sprintf("%.4f",final_cum_estimate_hi * 100)
    final_cum_estimate_SE <- sprintf("%.4f",final_cum_estimate_SE * 100)
  }
  
  return(list(cum_estimate = final_cum_estimate, cum_estimate_SE = as.numeric(final_cum_estimate_SE), upper_limit = final_cum_estimate_hi,lower_limit = final_cum_estimate_lo, truncated_lo = truncated_lo, truncated_hi = truncated_hi, cum_incidence_web = cum_incidence_web, Organ = organ))
}


age_start <- 25
age_stop <- 26
genes <- c('MLH1','MSH2','MSH6','PMS2')
Cancers <- c('PANCANCER','151','152','153','154','156','157','174','182','183','185','188','189','191','182_183','188_189','153_154','151_152_156_157')
sexes <- c('BOTH','MALE','FEMALE')

start <- age_start
stop <- age_stop
all_cum_est <- data.frame()
for(can in Cancers){
  for(g in genes){
    for(s in sexes){
      if((can == '174' | can == '182' | can == '183' | can == '182_183') & (s == 'BOTH' | s == 'MALE')){
        next
      }
      if((can == 'PANCANCER' | can == '191' | can == '189' | can == '188' | can == '151' | can == '156' | can == '157' | can == '188_189' | can == '153_154' | can == '151_152_156_157' | can == '152') & (s == 'MALE' | s == 'FEMALE')){
        next
      }
      if((can == '153' | can == '154') & (g == 'PMS2')){
        next
      }
      if((can == '185') & (s == 'BOTH' | s == 'FEMALE')){
        next
      }
      cat(paste0(can," ",g," ",s),sep='\n')
      start <- 25
      while(start <= 70){
        stop <- start
        while(stop <= 74){
          df_list <- get_cumulative_incidence_CI(organ_table, sex = s, cancer = can, age_start = start, age_stop = stop, gene = g)
          all_cum_est <- rbind(all_cum_est, data.frame('cum_estimate' = df_list$cum_estimate * 100, 'cum_incidence_web' = df_list$cum_incidence_web, 'cum_estimate_SE' = df_list$cum_estimate_SE * 100, 'upper_limit' = df_list$upper_limit * 100, 'lower_limit' = df_list$lower_limit * 100, 'Cancer' = can, 'Age_start' = start, 'Age_stop' = stop, 'Sex' = s, 'Gene' = g, 'Organ' = df_list$Organ))
          stop <- stop + 1
        }
        start <- start + 1
      }
    }
  }
}

pms2_missing <- data.frame()
for(s in c('MALE','FEMALE')){
  for(can in c('153','154')){
    start <- 25
    while(start <= 70){
      stop <- start
      while(stop <= 74){
        organ <- 'Sigmoid/rectum'
        if(can == '153'){
          organ <- 'Colon'
        }
        df <- data.frame('cum_estimate' = 0, 'cum_incidence_web' = 0, 'cum_estimate_SE' = 0, 'upper_limit' = 0, 'lower_limit' = 0, 'Cancer' = can, 'Age_start' = start, 'Age_stop' = stop, 'Sex' = s, 'Gene' = 'PMS2', 'Organ' = organ)
        pms2_missing <- rbind(pms2_missing, df)
        stop <- stop + 1
      }
      start <- start + 1
    }
  }
}

ls_organ_precomputed <- rbind(all_cum_est, pms2_missing)
save(ls_organ_precomputed, file="ls_organ_precomputed.rda")

