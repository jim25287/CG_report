# main page stat. ---------------------------------------------------------
  ##  台北目前累積病人數? Total/OB(finished/ongoing)/DM(finished/ongoing)
    ### - googleVis::gvisAnnotationChart: client_monthly_stat_report
  ##  初日減重門診，已經幫助了多少人成功減重? 
    ### - [目前分析樣本]: `r success_df_freq[["Total"]] %>% max()` 
    ### - [成功減重人數(%)]: 
        # 總共: `r success_df_freq[success_df_freq$gender == "Total", "success"]` ; `r success_df_pct[success_df_freq$gender == "Total", "success"]`
        # 女性: `r success_df_freq[success_df_freq$gender == "female", "success"]` ; `r success_df_pct[success_df_freq$gender == "female", "success"]`
        # 男性: `r success_df_freq[success_df_freq$gender == "male", "success"]` ; `r success_df_pct[success_df_freq$gender == "male", "success"]`
    ### - success_df_pct
    ### - success_df_freq





# 1-1. [初日班]read google sheet -------------------------------------------------------


library(googlesheets4)
clinical_list <- googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1T2swdx1cbfmUSUNQCQpbxa4aIJDTLD0oVKj3AuvNAuM/edit?usp=sharing', col_types = "iilccciDccDDcccdddcccc")
#clinical_list %>% glimpse()

#df clean
clinical_list <- clinical_list[-1:-5,]
clinical_list <- clinical_list[which(!is.na(clinical_list$serial_id)),]

clinical_list %<>% select(c("serial_id","id","client_type","name","date","class_date_1","class_date_2","medication","doctor","nutritionist_major","nutritionist_online","program","history","note"))

clinical_list$medication_note <- NA
#Trulicity
clinical_list[grep("(licity){1}",clinical_list$medication),"medication_note"] <- "Trulicity"
#Ozempic
clinical_list[grep("(mpic){1}",clinical_list$medication),"medication_note"] <- "Ozempic"
#Rybelsus, rebylsus(wrong name) search list
clinical_list[grep("(sus){1}",clinical_list$medication),"medication_note"] <- "Rybelsus"

clinical_list %<>% relocate(medication_note, .before = medication)



#clean client_type
#OB
clinical_list[intersect(which(is.na(clinical_list$client_type)), which(stringr::str_detect(clinical_list$program, "八週"))), "client_type"] <- 2
#DM
clinical_list[intersect(which(is.na(clinical_list$client_type)), which(stringr::str_detect(clinical_list$program, "糖"))), "client_type"] <- 1
#quit
clinical_list[intersect(which(is.na(clinical_list$client_type)), which(stringr::str_detect(clinical_list$note, "退"))), "client_type"] <- 0

#Genesis
clinical_stat <- clinical_list$client_type %>% table() %>% addmargins()
#北秀: 462人


# 1-2. [進階班] read google sheet -------------------------------------------------------

library(googlesheets4)
clinical_adv_list <- googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1T2swdx1cbfmUSUNQCQpbxa4aIJDTLD0oVKj3AuvNAuM/edit?usp=sharing', col_types = "cccDDccDDccciiccccc", sheet = "adv_list")
#clinical_adv_list %>% glimpse()
clinical_adv_list %<>% select(c("id","name","class_date_1","class_date_2","doctor","nutritionist_major","nutritionist_online","program","note","target","history","caution","medication"))

clinical_adv_list <- clinical_adv_list[complete.cases(clinical_adv_list[["id"]]),]

clinical_adv_list$medication_note <- NA
#Trulicity
clinical_adv_list[grep("(licity){1}",clinical_adv_list$medication),"medication_note"] <- "Trulicity"
#Ozempic
clinical_adv_list[grep("(mpic){1}",clinical_adv_list$medication),"medication_note"] <- "Ozempic"
#Rybelsus, rebylsus(wrong name) search list
clinical_adv_list[grep("(sus){1}",clinical_adv_list$medication),"medication_note"] <- "Rybelsus"

clinical_adv_list %<>% relocate(medication_note, .before = medication)



#clean client_type
#cause numerous missing value
clinical_adv_list$client_type <- 2
#OB
clinical_adv_list[which(stringr::str_detect(clinical_adv_list$program, "八週")), "client_type"] <- 2
#DM
clinical_adv_list[which(stringr::str_detect(clinical_adv_list$program, "糖")), "client_type"] <- 1
#quit
clinical_adv_list[which(stringr::str_detect(clinical_adv_list$note, "退")), "client_type"] <- 0

#Genesis
clinical_stat <- clinical_adv_list$client_type %>% table() %>% addmargins()


