
# 01-setting --------------------------------------------------------------

#load packages
library(pacman)
pacman::p_load(magrittr, knitr, kableExtra, dplyr, readr, readxl, tibble, showtext, extraInserts,
               ggvenn, ggplot2,knitr, kableExtra, openxlsx, lubridate, cowplot, ggpubr, webshot,
               stringr)
#font
font_add(family = "berlin_default", regular = "~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/data/font/STHeiti Light.ttc")
showtext_auto(enable = TRUE)





# 02.0-input_name_table --------------------------------------------------------
library(googlesheets4)
vars_table <- googlesheets4::read_sheet(ss = 'https://docs.google.com/spreadsheets/d/1T2swdx1cbfmUSUNQCQpbxa4aIJDTLD0oVKj3AuvNAuM/edit?usp=sharing', 
                                        sheet = "vars_table",
                                        col_types = "iccccc")
names(vars_table) <- c("num", "item_id", "ch", "en", "raw_en", "field")




# 02.1 - [Data Preprocessing] 01_profile --------------------------------------------------
#[Note:] 20230309_finish_genesis_ONLY

#input clinic_list
source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/00-read_clinic_list.R")


#Q1-1. 初日開幕前的cofit初日班 => topshow
df01_profile[dplyr::intersect(grep("初日班", df01_profile[["program_name"]]), which(df01_profile[["date_t0"]] < "2021-08-30")), "org_name"] <- "topshow"
df01_profile[dplyr::intersect(grep("秀0|秀1", df01_profile[["name"]]), which(df01_profile[["date_t0"]] < "2021-08-30")), "org_name"] <- "topshow"
#Q1-2. "program_name"初日開幕後的cofit初日班 => genesisclinic
df01_profile[Reduce(dplyr::intersect, list(grep("初日", df01_profile[["program_name"]]),
                                     which(df01_profile[["date_t0"]] >= "2021-08-30"),
                                     grep("cofit", df01_profile[["org_name"]])))
       , "org_name"] <- "genesisclinic"
#Q1-3. "name"名字有初日、初日開幕後的cofit => genesisclinic
df01_profile[Reduce(dplyr::intersect, list(grep("初日|初日001|G001", df01_profile[["name"]]),
                                     which(df01_profile[["date_t0"]] >= "2021-08-30"),
                                     grep("cofit", df01_profile[["org_name"]])))
       , "org_name"] <- "genesisclinic"

#Q1-4. FLC班 => cofit
df01_profile[grep("FLC", df01_profile[["program_name"]]), "org_name"] <- "cofit"

#C1-1. class_freq by org_name
df01_profile <- df01_profile %>% full_join(df01_profile %>% group_by(id, org_name) %>% summarise(class_freq = n()), by = c("id", "org_name"))
df01_profile <- df01_profile[with(df01_profile, order(c(date_t0, id))),] %>% janitor::remove_empty("rows")
#C1-2. class_order
for (i in unique(df01_profile$id)) {
  if (i == head(unique(df01_profile$id), 1)) {
    j = 1
    df01_profile$class_order <- NA
  }
  df01_profile[which(df01_profile[["id"]] == i), "class_order"] <- which(df01_profile[["id"]] == i) %>% order()
  progress(j, unique(df01_profile$id) %>% length())
  j = j + 1
  if (i == tail(unique(df01_profile$id), 1)){
    print("[Completed!]")
  }
}


#C2. age: btd - date_t0 年齡(療程起始當天計算)
df01_profile$age <- (lubridate::ymd(df01_profile$date_t0) - lubridate::ymd(df01_profile$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()

#C3-1.非進階
a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile$client_type <- NA #client_type 
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, client_type, id, clinical_list, client_type, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile$program_set <- NA #program_set
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, program_set, id, clinical_list, program, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile$doctor <- NA #doctor
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, doctor, id, clinical_list, doctor, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile$nutritionist_major <- NA #nutritionist_major
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, nutritionist_major, id, clinical_list, nutritionist_major, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile$nutritionist_online <- NA #nutritionist_online
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, nutritionist_online, id, clinical_list, nutritionist_online, id)

a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),]
df01_profile$medication <- NA #medication
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name, invert = TRUE)),] <- lin_mapping(a, medication, id, clinical_list, medication_note, id)

#C3-2.進階
a <- df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),]
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, client_type, id, clinical_adv_list, client_type, id)
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, program_set, id, clinical_adv_list, program, id)
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, doctor, id, clinical_adv_list, doctor, id)
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, nutritionist_major, id, clinical_adv_list, nutritionist_major, id)
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, nutritionist_online, id, clinical_adv_list, nutritionist_online, id)
df01_profile[intersect(which(df01_profile$org_name == "genesisclinic"), grep("進階", df01_profile$program_name)),] <- lin_mapping(a, medication, id, clinical_adv_list, medication_note, id)



#clean by select
df01_profile <- df01_profile %>% select(c("id", "name", "gender", "age", "client_type", "program_name","date_t0","date_t1", "org_name", "class_freq", "class_order","program_set","doctor","nutritionist_major","nutritionist_online","medication", "btd"))



# 02.2 - [Data Preprocessing] 02_inbody --------------------------------------------------

#C1. format
df02_inbody[c("height","weight","bmi","body_fat_mass","body_fat_mass_percentage","weight_without_fat","muscle_mass","real_muscle_mass","vfa","vfa_level","waist_circumference","acl","cacl","total_body_water","protein_weight","mineral_weight","body_cell_mass","body_mineral","bfmi","bsmi","ffmi","systolic_blood_pressure","diastolic_blood_pressure","pulse","bmr","wepa50","algle_50_left_arm","algle_50_left_leg","algle_50_right_arm","algle_50_right_leg","algle_50_trunk","extracellular_water_ratio","extracellular_water_ratio_left_arm","extracellular_water_ratio_left_leg","extracellular_water_ratio_right_arm","extracellular_water_ratio_right_leg","extracellular_water_ratio_trunk","intracellular_weight","intracellular_weight_left_arm","intracellular_weight_left_leg","intracellular_weight_right_arm","intracellular_weight_right_leg","intracellular_weight_trunk","extracellular_weight","extracellular_weight_left_arm","extracellular_weight_left_leg","extracellular_weight_right_arm","extracellular_weight_right_leg","extracellular_weight_trunk","left_arm_fat","left_arm_fat_percentage","left_arm_muscle","left_arm_muscle_percentage","left_leg_fat","left_leg_fat_percentage","left_leg_muscle","left_leg_muscle_percentage","right_arm_fat","right_arm_fat_percentage","right_arm_muscle","right_arm_muscle_percentage","right_leg_fat","right_leg_fat_percentage","right_leg_muscle_percentage","right_leg_muscle","trunk_fat","trunk_fat_percentage","trunk_muscle","trunk_muscle_percentage","water_weight_left_arm","water_weight_left_leg","water_weight_right_arm","water_weight_right_leg","water_weight_trunk","waist_hip_ratio","tbwffm","obesity_degree","inbody_total_score")] %<>% 
  lapply(as.numeric)
df02_inbody <- df02_inbody %>% as.tibble() 
#C2. Sarcopenia Obesity(SO): "left_arm_muscle", "left_leg_muscle", "right_arm_muscle", "right_leg_muscle" #Female: < 23.4; Male: < 29.6. 
df02_inbody <- df02_inbody %>% mutate(so_score = round((left_arm_muscle+left_leg_muscle+right_arm_muscle+right_leg_muscle)*100/weight,2))
#C3. pbm
df02_inbody <- df02_inbody %>% mutate(pbm = round((muscle_mass)*100/weight,2))
#C4. name_format
names(df02_inbody) <- names(df02_inbody) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
#C5. rm outlier
df02_inbody <- df02_inbody[-which(df02_inbody$bmi >100),]


# 02.3 - [Data Preprocessing] 03_FLC_self_report --------------------------------------------------

















