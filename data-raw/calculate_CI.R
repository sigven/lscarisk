require(dplyr)

air_df[1,]$Years_5int <- 876
air_df[2,]$Years_5int <- 1190
air_df[3,]$Years_5int <- 1320
air_df[4,]$Years_5int <- 1415
air_df[5,]$Years_5int <- 1088
air_df[6,]$Years_5int <- 820
air_df[7,]$Years_5int <- 509
air_df[8,]$Years_5int <- 286
air_df[9,]$Years_5int <- 149
air_df[9,]$Cases_5int <- 7
air_df[8,]$Cases_5int <- 9
air_df[7,]$Cases_5int <- 18
air_df[6,]$Cases_5int <- 33
air_df[5,]$Cases_5int <- 37
air_df[4,]$Cases_5int <- 44
air_df[3,]$Cases_5int <- 22
air_df[2,]$Cases_5int <- 24
air_df[1,]$Cases_5int <- 5

get_cumulative_incidence_CI <- function(lsrisk_df, sex = 'FEMALE', cancer = 'PANCANCER', age_start = 25, age_stop = 70, gene = 'MLH1', percentage = F, num_digits = 1){
    ### get annual incidence rates (air) per 5 year interval
    age_marks <- c(25,30,35,40,45,50,55,60,65)
    air_df <- dplyr::filter(lsrisk_df, Sex == sex & Gene == gene & Cancer == cancer & Age %in% age_marks)
    air_df$air <- air_df$Cases_5int / air_df$Years_5int
    
    ## standard error
    air_df$air_SE <- sqrt(air_df$air * (1 - air_df$air)/air_df$Years_5int)
    
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
    
    final_cum_estimate <- round(final_cum_estimate,num_digits)
    final_cum_estimate_lo <- round(final_cum_estimate_lo,num_digits)
    final_cum_estimate_hi <- round(final_cum_estimate_hi,num_digits)
    
    if(percentage == TRUE){
        final_cum_estimate <- sprintf("%.1f",final_cum_estimate * 100)
        final_cum_estimate_lo <- sprintf("%.1f",final_cum_estimate_lo * 100)
        final_cum_estimate_hi <- sprintf("%.1f",final_cum_estimate_hi * 100)
    }

    

    return(list(cum_estimate = final_cum_estimate, upper_limit = final_cum_estimate_hi,lower_limit = final_cum_estimate_lo, truncated_lo = truncated_lo, truncated_hi = truncated_hi))
}

load('ls_risk_data_final_v3.rda')
risk_df <- all_cum_ind

### TABLE 3

cancer <- 'PANCANCER'
age_start <- 25
age_stop <- c(40,50,60,70)
sexes <- c('MALE','FEMALE')
gene <- 'ALL'

df_res <- NULL
for(s in sexes){
    for(a in age_stop){
        ci_est <- get_cumulative_incidence_CI(risk_df,sex=s,percentage=T,cancer=cancer,age_start=age_start, num_digits=3,age_stop=a,gene=gene)
        ci <- paste0('[',ci_est$lower_limit,' - ',ci_est$upper_limit,']')
        ci_truncation <- '.'
        if(ci_est$truncated_lo == TRUE){
            ci_truncation <- '*'
        }
        if(ci_est$truncated_hi == TRUE){
            ci_truncation <- '**'
        }
        df <- data.frame('cancertype'=cancer,'current_age'=age_start,'age_stop'=a,'sex'=s,'gene'=gene,'cum_incidence_risk_percent'=ci_est$cum_estimate,'CI'=ci,'CI_truncation'=ci_truncation)
        if(is.null(df_res)){
            df_res <- df
        }
        else{
            df_res <- rbind(df_res, df)
        }
    }
}
write.table(df_res,sep='\t',quote=F,row.names=F,file='Table3_update_v4.tsv',col.names=T)

### TABLE 4

cancer <- 'PANCANCER'
#cancer <- 'CRC'
age_stop <- 70
age_starts <- c(25,40,50,60)
genes <- c('MLH1','MSH2','MSH6','PMS2')

df_res <- NULL
for(g in genes){
    for(a in age_starts){
        ci_est <- get_cumulative_incidence_CI(risk_df,sex='BOTH',percentage=T,cancer=cancer,age_start=a, num_digits=3,age_stop=age_stop,gene=g)
        ci <- paste0('[',ci_est$lower_limit,' - ',ci_est$upper_limit,']')
        ci_truncation <- '.'
        if(ci_est$truncated_lo == TRUE){
            ci_truncation <- '*'
        }
        if(ci_est$truncated_hi == TRUE){
            ci_truncation <- '**'
        }
        df <- data.frame('cancertype'=cancer,'current_age'=a,'age_stop'=age_stop,'sex'='BOTH','gene'=g,'cum_incidence_risk_percent'=ci_est$cum_estimate,'CI'=ci,'CI_truncation'=ci_truncation)
        if(is.null(df_res)){
            df_res <- df
        }
        else{
            df_res <- rbind(df_res, df)
        }
    }
}
write.table(df_res,sep='\t',quote=F,row.names=F,file='Table4_update_v4.tsv',col.names=T)

cancers <- c('CRC','PANCANCER')
age_stop <- 70
age_starts <- c(40,50,60)
genes <- c('MLH1','MSH2','MSH6')

df_res <- NULL
for (c in cancers){
    for(g in genes){
        for(a in age_starts){
            ci_est <- get_cumulative_incidence_CI(risk_df,sex='BOTH',percentage=T,cancer=c,age_start=a, num_digits=3,age_stop=age_stop,gene=g)
            ci <- paste0('[',ci_est$lower_limit,' - ',ci_est$upper_limit,']')
            ci_truncation <- '.'
            if(ci_est$truncated_lo == TRUE){
                ci_truncation <- '*'
            }
            if(ci_est$truncated_hi == TRUE){
                ci_truncation <- '**'
            }
            df <- data.frame('cancertype'=c,'current_age'=a,'age_stop'=age_stop,'sex'='BOTH','gene'=g,'cum_incidence_risk_percent'=ci_est$cum_estimate,'CI'=ci,'CI_truncation'=ci_truncation)
            if(is.null(df_res)){
                df_res <- df
             }
            else{
               df_res <- rbind(df_res, df)
            }
        }
  
     }

}
    
    
write.table(df_res,sep='\t',quote=F,row.names=F,file='Table5_next_cancer_update_v4.tsv',col.names=T)

### TABLE 5

cancers <- c('PANCANCER','CRC')
age_start <- 25
age_stops <- c(40,50,60,70)
genes <- c('MLH1','MSH2','MSH6','PMS2')
sexes <- c('MALE','FEMALE','BOTH')

df_res <- NULL
for(c in cancers){
    for(g in genes){
        for(s in sexes){
            for(a in age_stops){
                ci_est <- get_cumulative_incidence_CI(risk_df,sex=s,percentage=T,cancer=c,age_start=age_start, num_digits=3,age_stop=a,gene=g)
                ci <- paste0('[',ci_est$lower_limit,' - ',ci_est$upper_limit,']')
                ci_truncation <- '.'
                if(ci_est$truncated_lo == TRUE){
                    ci_truncation <- '*'
                }
                if(ci_est$truncated_hi == TRUE){
                    ci_truncation <- '**'
                }
                df <- data.frame('cancertype'=c,'current_age'=age_start,'age_stop'=a,'sex'=s,'gene'=g,'cum_incidence_risk_percent'=ci_est$cum_estimate,'CI'=ci,'CI_truncation'=ci_truncation)
                if(is.null(df_res)){
                    df_res <- df
                }
                else{
                    df_res <- rbind(df_res, df)
                }
            }
        }
    }
}

cancers <- c('URO','UGI','END','OVARIAN')
age_start <- 25
age_stops <- c(40,50,60,70)
genes <- c('MLH1','MSH2','MSH6','PMS2')
sexes <- c('FEMALE')

for(c in cancers){
    for(g in genes){
        for(s in sexes){
            for(a in age_stops){
                ci_est <- get_cumulative_incidence_CI(risk_df,sex=s,percentage=T,cancer=c,age_start=age_start, num_digits=3,age_stop=a,gene=g)
                ci <- paste0('[',ci_est$lower_limit,' - ',ci_est$upper_limit,']')
                ci_truncation <- '.'
                if(ci_est$truncated_lo == TRUE){
                    ci_truncation <- '*'
                }
                if(ci_est$truncated_hi == TRUE){
                    ci_truncation <- '**'
                }
                df <- data.frame('cancertype'=c,'current_age'=age_start,'age_stop'=a,'sex'=s,'gene'=g,'cum_incidence_risk_percent'=ci_est$cum_estimate,'CI'=ci,'CI_truncation'=ci_truncation)
                if(is.null(df_res)){
                    df_res <- df
                }
                else{
                    df_res <- rbind(df_res, df)
                }
            }
        }
    }
}

df_res[df_res$cancertype == 'URO',]$sex <- 'BOTH'
df_res[df_res$cancertype == 'UGI',]$sex <- 'BOTH'


write.table(df_res,sep='\t',quote=F,row.names=F,file='Table5_update_v4.tsv',col.names=T)

