#library(rdrop2)
#library(reticulate)
library(dplyr)
library(magrittr)

source('data-raw/20190621/cumulative_incidence.R')


plsd_raw <- read.table(file="data-raw/20190621/plsd_raw_air.txt",sep="\t",header=T,stringsAsFactors = F, quote="") %>%
  dplyr::filter(Version == "V3_20190219_A" | Version == "V3_20190219_B")
colnames(plsd_raw) <- c('age','sex','cancer','organ','gene','time_years','events_cases','version','air')
plsd_raw <- plsd_raw %>%
  dplyr::mutate(cancer = dplyr::if_else(cancer == "ICD9UGI","151_152_156_157",as.character(cancer))) %>%
  dplyr::mutate(cancer = dplyr::if_else(cancer == "ICD9URO","188_189",as.character(cancer))) %>%
  dplyr::mutate(cancer = dplyr::if_else(cancer == "182183","182_183",as.character(cancer))) %>%
  dplyr::mutate(cancer = dplyr::if_else(cancer == "VP_LS_AIR_BASE","PANCANCER",as.character(cancer))) %>%
  dplyr::mutate(cancer = dplyr::if_else(cancer == "153154","153_154",as.character(cancer))) %>%
  dplyr::mutate(cancer = dplyr::if_else(cancer == "185188189","185_188_189",as.character(cancer)))


confidence_intervals_fiveyearsteps_1 <- read.table(file="data-raw/20190621/PLSD_GenetMed_RISK_ESTIMATES.1.tsv",sep="\t", quote="",header = T, stringsAsFactors = F) %>%
  dplyr::select(-c(ci_lower_wald,ci_upper_wald)) %>%
  dplyr::rename(upper_limit = ci_upper_wilson, lower_limit = ci_lower_wilson) %>%
  dplyr::rename(Cancer = cancer, Organ = organ, Age_start = start_age, Age_stop = stop_age, Sex = gender, Gene = gene) %>%
  dplyr::mutate(Cancer = as.character(Cancer))

confidence_intervals_fiveyearsteps_2 <- read.table(file="data-raw/20190621/PLSD_GenetMed_RISK_ESTIMATES.2.tsv",sep="\t", quote="",header = T, stringsAsFactors = F) %>%
  dplyr::select(-c(ci_lower_wald,ci_upper_wald)) %>%
  dplyr::rename(upper_limit = ci_upper_wilson, lower_limit = ci_lower_wilson) %>%
  dplyr::rename(Cancer = cancer, Organ = organ, Age_start = start_age, Age_stop = stop_age, Sex = gender, Gene = gene) %>%
  dplyr::mutate(Cancer = as.character(Cancer))

ci_plsd_all <- dplyr::bind_rows(confidence_intervals_fiveyearsteps_1, confidence_intervals_fiveyearsteps_2) %>%
  dplyr::mutate(Cancer = dplyr::if_else(Cancer == "ICD9UGI","151_152_156_157",as.character(Cancer))) %>%
  dplyr::mutate(Cancer = dplyr::if_else(Cancer == "ICD9URO","188_189",as.character(Cancer))) %>%
  dplyr::mutate(Cancer = dplyr::if_else(Cancer == "182183","182_183",as.character(Cancer))) %>%
  dplyr::mutate(Cancer = dplyr::if_else(Cancer == "VP_LS_AIR_BASE","PANCANCER",as.character(Cancer))) %>%
  dplyr::mutate(Cancer = dplyr::if_else(Cancer == "153154","153_154",as.character(Cancer))) %>%
  dplyr::mutate(Cancer = dplyr::if_else(Cancer == "185188189","185_188_189",as.character(Cancer)))

cancer2Organ <- dplyr::select(ci_plsd_all, Cancer, Organ) %>% 
  dplyr::distinct()

age_start <- 25
age_stop <- 25
genes <- c('MLH1','MSH2','MSH6','PMS2')
#genes <- c('PMS2')
cancers <- c('PANCANCER','174','182','183','185','188','189','191','182_183','153_154','151_152_156_157')
sexes <- c('BOTH','MALE','FEMALE')

start <- age_start
stop <- age_stop
all_cum_est <- data.frame()
for(can in cancers){
  for(g in genes){
    for(s in sexes){
      if((can == '174' | can == '182' | can == '183' | can == '182_183') & (s == 'BOTH' | s == 'MALE')){
        next
      }
      # if((can == 'PANCANCER' | can == '191' | can == '189' | can == '188' | can == '151' | can == '156' | can == '157' | can == '188_189' | can == '153_154' | can == '151_152_156_157' | can == '152') & (s == 'MALE' | s == 'FEMALE')){
      #   next
      # }
      #if((can == '153' | can == '154') & (g == 'PMS2')){
        #next
      #}
      if((can == '185' | can == '185_188_189') & (s == 'BOTH' | s == 'FEMALE')){
        next
      }
      cat(paste0(can," ",g," ",s),sep='\n')
      start <- 25
      while(start <= 75){
        stop <- start
        while(stop <= 75){
          risk_estimate <- cumulative_risk_estimate(plsd_raw, qsex = s, qcancer = can, age_start = start, age_stop = stop, qgene = g, num_significant_digits = 6)
          all_cum_est <- rbind(all_cum_est, data.frame('cum_estimate' = risk_estimate, 'Cancer' = can, 'Age_start' = start, 'Age_stop' = stop, 'Sex' = s, 'Gene' = g))
          stop <- stop + 1
        }
        start <- start + 5
      }
    }
  }
}

all_cum_est$Cancer <- as.character(all_cum_est$Cancer)
all_cum_est$Sex <- as.character(all_cum_est$Sex)
all_cum_est$Gene <- as.character(all_cum_est$Gene)

all_cum_est2 <- all_cum_est %>% dplyr::left_join(dplyr::select(ci_plsd_all, Cancer,  Age_start, Age_stop, Sex, Gene, cumulative_risk, lower_limit, upper_limit)) %>%
  dplyr::mutate(cum_estimate = dplyr::if_else(is.na(cumulative_risk) & Age_stop == Age_start,0,as.numeric(cum_estimate))) %>%
  dplyr::mutate(lower_limit = dplyr::if_else(is.na(lower_limit) & Age_stop == Age_start,0,as.numeric(lower_limit))) %>%
  dplyr::mutate(upper_limit = dplyr::if_else(is.na(upper_limit) & Age_stop == Age_start,0,as.numeric(upper_limit))) %>%
  dplyr::select(-cumulative_risk) %>%
  dplyr::left_join(cancer2Organ)

feather::write_feather(all_cum_est2, "ls_organ_precomputed_2019_v2.feather")

