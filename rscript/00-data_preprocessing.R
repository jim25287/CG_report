
# 01-setting --------------------------------------------------------------
  
  #load packages
  library(pacman)
  pacman::p_load(magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, extraInserts, ggvenn, ggplot2,knitr, kableExtra, openxlsx, lubridate, cowplot, ggpubr, webshot)
  #font
  font_add(family = "berlin_default", regular = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/font/STHeiti Light.ttc")
  showtext_auto(enable = TRUE)
  

  

# 02.1-input_blood --------------------------------------------------------
  #blood datasets
  clinic_blood_data <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/blood_data.csv")
  
  #blood data clean
  clinic_blood_data <-  clinic_blood_data %>% dplyr::select(c("member_id","date","hba1c","glucose_ac","glucose_pc_1hr","glucose_pc_2hr","insulin","insulin_pc_1hr",
                                                              "insulin_pc_2hr","homa_ir","homa_beta","triglyceride","total_cholesterol","high_density_lipoprotein",
                                                              "low_density_lipoprotein_cholesterol","e2","testosterone", "lipase"))
  colnames(clinic_blood_data)[c(1,2,12,13,14,15)] <- c("id","date_blood","tg","tc","hdl","ldl")
  
  clinic_blood_data <- clinic_blood_data[Reduce(dplyr::union,list(which(!is.na(clinic_blood_data$tg)),
                                                                  which(!is.na(clinic_blood_data$tc)),
                                                                  which(!is.na(clinic_blood_data$hdl)),
                                                                  which(!is.na(clinic_blood_data$ldl)))),]
  #variable format
  clinic_blood_data$date_blood <- as.Date(clinic_blood_data$date_blood)
  clinic_blood_data[c("id")] %<>% lapply(as.integer)
  clinic_blood_data[c("homa_ir", "homa_beta")] %<>% lapply(gsub, pattern = "[^0-9.-]", replacement = "NA")
  clinic_blood_data[c("glucose_pc_1hr","glucose_pc_2hr","insulin","insulin_pc_1hr","insulin_pc_2hr","homa_ir", "homa_beta","tg","tc","hdl","ldl","e2","testosterone", "lipase")] %<>% lapply(as.numeric)
  
  #wrong data
  clinic_blood_data <- clinic_blood_data[-which((clinic_blood_data$id == 470051) & (clinic_blood_data$date_blood == "2021-10-23")),]
  clinic_blood_data <- clinic_blood_data[-which(clinic_blood_data$id == 302),]


# 02.2-input_inbody -------------------------------------------------------
  #inbody datasets
  clinic_inbody_data <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/inbody_data.csv")
  #inbody data clean
  clinic_inbody_data <- clinic_inbody_data %>% dplyr::select(c("member_id","date","weight","bmi","body_fat_mass","body_fat_mass_percentage", "bsmi", "muscle_mass","vfa_level","waist_circumference","weight_without_fat","extracellular_water_ratio", "wepa50","bmr"))
  colnames(clinic_inbody_data)[c(1,2,5,6,8,9,10,11)] <- c("id","date_inbody","bf","pbf","bm","vfa","wc","ffm")
  #variable format
  clinic_inbody_data$date_inbody <- as.Date(clinic_inbody_data$date_inbody)
  clinic_inbody_data[c("id","vfa")] %<>% lapply(as.integer)
  clinic_inbody_data[c("weight","bmi","bf","pbf", "bsmi","bm","wc","ffm","extracellular_water_ratio","wepa50","bmr")] %<>% lapply(as.numeric)
  #rm outlier
  clinic_inbody_data <- clinic_inbody_data[-which(clinic_inbody_data$bmi >100),]
  clinic_inbody_data <- clinic_inbody_data[-which(clinic_inbody_data$id == 302),]
  
  

# 02.3-input_client_info --------------------------------------------------
  #clients datasets
  clinic_client_data <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/client_info.csv")
  clinic_client_data[c("btd","date_1st","date_T0","date_T1","date_T2","date_T3","date_T4")] %<>% lapply(as.Date)
  #variable format
  clinic_client_data[c("serial_id","client_type","id","age")] %<>% lapply(as.integer)
  
  

# 02.4-input_(non)genesis_list -------------------------------------------------

  #clinic_cliet_list datasets
  clinical_list <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/genesis_list.csv")
  clinical_list <- clinical_list[-1:-5,]
  clinical_list <- clinical_list[which(!is.na(clinical_list$serial_id)),]
  clinical_list %<>% dplyr::select(c("serial_id","id","client_type","name","date","class_date_1","class_date_2","medication","doctor","nutritionist_major","nutritionist_online","program","history"))
  
  clinical_list$medication_note <- NA
  #Trulicity
  clinical_list[grep("(licity){1}",clinical_list$medication),"medication_note"] <- "Trulicity"
  #Ozempic
  clinical_list[grep("(mpic){1}",clinical_list$medication),"medication_note"] <- "Ozempic"
  #Rybelsus, rebylsus(wrong name) search list
  clinical_list[grep("(sus){1}",clinical_list$medication),"medication_note"] <- "Rybelsus"
  
  clinical_list %<>% dplyr::relocate(medication_note, .before = medication)
  
  #map age, gender, hormone
  clinical_list <- lin_mapping(clinical_list, age, id, clinic_client_data, age, id)
  clinical_list <- lin_mapping(clinical_list, gender, id, clinic_client_data, gender, id)
  clinical_list <- lin_mapping(clinical_list, hormone, id, clinic_client_data, hormone_type, id)
  
  #variable format
  clinical_list[c("serial_id","id","client_type", "age")] %<>% lapply(as.integer)
  clinical_list[c("date","class_date_1","class_date_2")] %<>% lapply(as.Date)
  colnames(clinical_list)[5] <-  "date_1st"
  
  clinical_list <- clinical_list[which(clinical_list$client_type != "0"),]  
  
  
  
  out_of_genesis_list_age_gender <- readr::read_csv("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/non_genesis_list.csv")
  out_of_genesis_list_age_gender$birthday %<>% as.Date()
  out_of_genesis_list_age_gender$age <- (lubridate::ymd("2023-01-01") -  lubridate::ymd(out_of_genesis_list_age_gender$birthday)) %>% as.numeric() %>% divide_by(365) %>% floor()
  out_of_genesis_list_age_gender[c("id", "age")] %<>% lapply(as.integer)
  
  
  
  
  
  