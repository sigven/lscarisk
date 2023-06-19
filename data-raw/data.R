# ovarian <- read.table('data-raw/ovarian.tsv',sep='\t',stringsAsFactors=F,dec=',',header=T,na.strings=c(''), colClasses=c('integer','character','character','numeric','integer','integer'))
# ovarian$Sex <- c(rep('FEMALE',nrow(ovarian)))
# ovarian_male <- ovarian
# ovarian_male$Sex <- rep('MALE',nrow(ovarian_male))
# ovarian_male$Annual_incidence_rate <- rep(0,nrow(ovarian_male))
#
# crc <- read.table('data-raw/crc.tsv',sep='\t',stringsAsFactors=F,dec=',',header=T,na.strings=c(''), colClasses=c('integer','character','character','character','numeric','integer','integer'))
#
# endometrial <- read.table('data-raw/end.tsv',sep='\t',stringsAsFactors=F,dec=',',header=T,na.strings=c(''), colClasses=c('integer','character','character','numeric','integer','integer'))
# endometrial$Sex <- c(rep('FEMALE',nrow(endometrial)))
# endometrial_male <- endometrial
# endometrial_male$Sex <- rep('MALE',nrow(endometrial_male))
# endometrial_male$Annual_incidence_rate <- rep(0,nrow(endometrial_male))
#
# uro <- read.table('data-raw/uro.tsv',sep='\t',stringsAsFactors=F,dec=',',header=T,na.strings=c(''), colClasses=c('integer','character','character','numeric','integer','integer'))
# uro$Sex <- c(rep('FEMALE',nrow(uro)))
# uro_male <- uro
# uro_male$Sex <- rep('MALE',nrow(uro_male))
#
# ugi <- read.table('data-raw/ugi.tsv',sep='\t',stringsAsFactors=F,dec=',',header=T,na.strings=c(''), colClasses=c('integer','character','character','numeric','integer','integer'))
# ugi$Sex <- c(rep('FEMALE',nrow(ugi)))
# ugi_male <- ugi
# ugi_male$Sex <- rep('MALE',nrow(ugi_male))
#
# pancancer <- read.table('data-raw/pancancer.tsv',sep='\t',stringsAsFactors=F,dec=',',header=T,na.strings=c(''), colClasses=c('integer','character','character','character','numeric','integer','integer'))
#
# all_cum_ind <- rbind(ovarian,ovarian_male,crc,endometrial,endometrial_male, uro, uro_male,ugi,ugi_male, pancancer)
#
# save(all_cum_ind,file="ls_risk_data.rda")
#
# all_cum_ind_previous_cancer <- read.table(file="next_cancer.txt",sep="\t",header=T,quote="",stringsAsFactors = F)
# all_cum_ind_previous_cancer$Annual_incidence_rate <- as.numeric(stringr::str_replace(all_cum_ind_previous_cancer$Annual_incidence_rate,",","."))
# save(all_cum_ind_previous_cancer,file="ls_risk_data_subsequent.rda")
#
# air_organs <- xlsx::read.xlsx("data-raw/online_data_mns4.xlsx",1,startRow=4,endRow=109,stringsAsFactors=F,header=T)
# air_organs <- air_organs %>% dplyr::filter(!is.na(ICD9))
# air_organs[air_organs$ICD9 == 'Any cancer',]$ICD9 <- 'PANCANCER'
# air_organs$ICD9 <- stringr::str_replace_all(air_organs$ICD9,", | or ","_")
#
# organ_icd9 <- data.frame('icd9_code' = character(), 'organ' = character(), stringsAsFactors = F)
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '151', 'organ' = 'Stomach', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '152', 'organ' = 'Duodenum', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '153', 'organ' = 'Colon', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '154', 'organ' = 'Sigmoid/rectum', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '156', 'organ' = 'Bile duct/gall bladder', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '157', 'organ' = 'Pancreas', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '174', 'organ' = 'Breast', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '182', 'organ' = 'Endometrium', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '183', 'organ' = 'Ovaries', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '185', 'organ' = 'Prostate', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '188', 'organ' = 'Urinary bladder', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '189', 'organ' = 'Ureter/kidney', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '191', 'organ' = 'Brain', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '188_189', 'organ' = 'Urine bladder/ureter/kidney', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '153_154', 'organ' = 'Colon/sigmoid/rectum', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '151_152_156_157', 'organ' = 'Stomach/duodenum/bile duct/gall bladder/pancreas', stringsAsFactors = F))
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = '182_183', 'organ' = 'Endometrium/ovaries', stringsAsFactors = F))
#
#
# organ_icd9 <- rbind(organ_icd9, data.frame('icd9_code' = 'PANCANCER', 'organ' = 'Any', stringsAsFactors = F))
#
#
# organ_table <- NULL
# for(i in 1:nrow(air_organs)){
#   start_age <- 25
#   gene <- air_organs[i,"Pathogenic.variant.of"]
#   sex <- 'BOTH'
#   if(!is.na(air_organs[i,"Gender"])){
#     if(air_organs[i,"Gender"] == 'Female'){
#       sex <- 'FEMALE'
#     }
#     if(air_organs[i,"Gender"] == 'Male'){
#       sex <- 'MALE'
#     }
#   }
#
#   icd_code <- air_organs[i,"ICD9"]
#   tmp <- dplyr::filter(organ_icd9, icd9_code == icd_code)
#   organ <- NA
#   if(nrow(tmp) > 0){
#     organ <- tmp$organ
#   }
#   col <- 4
#   while(col <= 34){
#
#     current_age <- start_age
#     for(k in 1:5){
#       df <- data.frame('Age' = current_age,'Sex' = sex,'Cancer' = icd_code,'Organ' = organ, 'Gene' = gene,'Years_5int' = as.integer(air_organs[i,col]),'Cases_5int' = as.integer(air_organs[i,(col+1)]),stringsAsFactors = F)
#       organ_table <- rbind(organ_table,df)
#       current_age <- current_age + 1
#     }
#     if(current_age == 80){
#       df <- data.frame('Age' = current_age,'Sex' = sex,'Cancer' = icd_code,'Organ' = organ, 'Gene' = gene,'Years_5int' = as.integer(air_organs[i,col]),'Cases_5int' = as.integer(air_organs[i,(col+1)]),stringsAsFactors = F)
#       organ_table <- rbind(organ_table,df)
#     }
#     col <- col + 3
#     start_age <- start_age + 5
#   }
#   cat(i,sep="\n")
# }
#
# organ_table$Annual_incidence_rate <- round(organ_table$Cases_5int / organ_table$Years_5int,digits=6)
# if(nrow(organ_table[is.nan(organ_table$Annual_incidence_rate),]) > 0){
#   organ_table[is.nan(organ_table$Annual_incidence_rate),]$Annual_incidence_rate <- 0
# }
#
# save(organ_table,file="ls_organ.rda")
