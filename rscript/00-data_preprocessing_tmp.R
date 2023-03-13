
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
#***[Note:] 20230309_finish_genesis_ONLY

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
#***[Note:] 20230310_not_full_obs.

#C1. col_names
names(df03_FLC_self_report) <- names(df03_FLC_self_report) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
df03_FLC_self_report <- df03_FLC_self_report[with(df03_FLC_self_report, order(date_flc_T0)),]

#C2. age: btd - date_t0 年齡(療程起始當天計算)
df03_FLC_self_report$age <- (lubridate::ymd(df03_FLC_self_report$date_flc_T0) - lubridate::ymd(df03_FLC_self_report$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()
#C3. (1.) (%) *100  (2.) numeric %>% round(2)
df03_FLC_self_report[,grep("%", names(df03_FLC_self_report))] %<>% multiply_by(100)
df03_FLC_self_report[c("weight(T0)","weight(T1)","∆weight","∆weight(%)","BMI(T0)","BMI(T1)","∆BMI","∆BMI(%)","Fat(T0)","Fat(T1)","∆Fat","∆Fat(%)","wc(T0)","wc(T1)","∆wc","∆wc(%)")] %<>% round(2)


#C4-1. class_freq by org_name
df03_FLC_self_report <- df03_FLC_self_report %>% full_join(df03_FLC_self_report %>% group_by(id) %>% summarise(class_freq = n()), by = c("id"))
#C4-2. class_order
for (i in unique(df03_FLC_self_report$id)) {
  if (i == head(unique(df03_FLC_self_report$id), 1)) {
    j = 1
    df03_FLC_self_report$class_order <- NA
  }
  df03_FLC_self_report[which(df03_FLC_self_report[["id"]] == i), "class_order"] <- which(df03_FLC_self_report[["id"]] == i) %>% order()
  progress(j, unique(df03_FLC_self_report$id) %>% length())
  j = j + 1
  if (i == tail(unique(df03_FLC_self_report$id), 1)){
    print("[Completed!]")
  }
}


# 02.4 - [Data Preprocessing] 04_non_FLC_self_report --------------------------------------------------

# intersect(tmp_03$id, df04_non_FLC_self_report$client_id)  #ensure no FLC client within
#C1. col_name
names(df04_non_FLC_self_report) <- names(df04_non_FLC_self_report) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
#C2-1. filter dup_id
df04_non_FLC_self_report <- df04_non_FLC_self_report %>% janitor::get_dupes(id)
#C2-2. exclude NA: id, date
df04_non_FLC_self_report <- df04_non_FLC_self_report %>% lin_exclude_NA_col(c("id", "date_time"))
#C3. order
df04_non_FLC_self_report <- df04_non_FLC_self_report[with(df04_non_FLC_self_report, order(id, date_free_version)),]
#C4. filter ∆day = 2 months(60 days + 14)

# Group the data by id and find the earliest date for each id
earliest_dates <- df04_non_FLC_self_report %>%
  group_by(id) %>%
  summarize(earliest_date = min(date_free_version))

# Join the original data frame with the earliest date to find the row with the earliest date for each id
earliest_rows <- df04_non_FLC_self_report %>%
  inner_join(earliest_dates, by = c("id", "date_free_version" = "earliest_date"))

earliest_rows$weight <- ifelse(earliest_rows$weight <= 30, NA, earliest_rows$weight)
earliest_rows$weight <- ifelse(earliest_rows$weight > 200, NA, earliest_rows$weight)
earliest_rows$bmi <- ifelse(earliest_rows$bmi <= 10, NA, earliest_rows$bmi)
earliest_rows$bmi <- ifelse(earliest_rows$bmi > 100, NA, earliest_rows$bmi)
earliest_rows$fat <- ifelse(earliest_rows$fat <= 5, NA, earliest_rows$fat)
earliest_rows$fat <- ifelse(earliest_rows$fat > 100, NA, earliest_rows$fat)
earliest_rows$wc <- ifelse(earliest_rows$wc <= 50, NA, earliest_rows$wc)


# Add 60 days to the earliest date for each id
second_dates <- earliest_dates %>%
  mutate(second_date = earliest_date + 60)

# Find the row with the second date for each id
second_rows <- df04_non_FLC_self_report %>%
  inner_join(second_dates, by = "id") %>%
  filter((date_free_version >= second_date) & (date_free_version <= second_date + 14)) %>% 
  distinct(id, .keep_all = TRUE)

second_rows$weight <- ifelse(second_rows$weight <= 30, NA, second_rows$weight)
second_rows$weight <- ifelse(second_rows$weight > 200, NA, second_rows$weight)
second_rows$bmi <- ifelse(second_rows$bmi <= 10, NA, second_rows$bmi)
second_rows$bmi <- ifelse(second_rows$bmi > 100, NA, second_rows$bmi)
second_rows$fat <- ifelse(second_rows$fat <= 5, NA, second_rows$fat)
second_rows$fat <- ifelse(second_rows$fat > 100, NA, second_rows$fat)
second_rows$wc <- ifelse(second_rows$wc <= 50, NA, second_rows$wc)


# Combine the earliest and second rows for each id
df04_non_FLC_self_report <- earliest_rows %>%
  bind_rows(second_rows)
rm(list = c("earliest_dates", "earliest_rows", "second_dates", "second_rows"))

df04_non_FLC_self_report <- df04_non_FLC_self_report %>% filter(id %in% df04_non_FLC_self_report[!is.na(df04_non_FLC_self_report$earliest_date), "id"])
df04_non_FLC_self_report <- df04_non_FLC_self_report[with(df04_non_FLC_self_report, order(id)),]
df04_non_FLC_self_report <- df04_non_FLC_self_report %>% distinct(id, date_free_version, .keep_all = TRUE)

df04_non_FLC_self_report <- df04_non_FLC_self_report %>% select(-c("dupe_count", "date_time", "earliest_date", "second_date"))

#clean pre/post table

df04_non_FLC_self_report_tmp <- df04_non_FLC_self_report

a <- df04_non_FLC_self_report_tmp[seq(1,nrow(df04_non_FLC_self_report_tmp), 2),] 
names(a)[-1] <- paste0(a %>% select(-c("id")) %>% names(), "_baseline")

b <- df04_non_FLC_self_report_tmp[seq(2,nrow(df04_non_FLC_self_report_tmp), 2),] 
names(b)[-1] <- paste0(b %>% select(-c("id")) %>% names(), "_endpoint")

aa <- b %>% select(-c("id", "date_free_version_endpoint")) - a %>% select(-c("id", "date_free_version_baseline"))
aa <- cbind(a %>% select("id"), aa)
names(aa)[-1] <- paste0("∆", df04_non_FLC_self_report_tmp %>% select(-c("id","date_free_version")) %>% names())

bb <- ((b %>% select(-c("id", "date_free_version_endpoint")) - a %>% select(-c("id", "date_free_version_baseline")))*100 /   a %>% select(-c("id", "date_free_version_baseline"))) %>% round(2) 
bb <- cbind(a %>% select("id"), bb)
names(bb)[-1] <- paste0("∆", df04_non_FLC_self_report_tmp %>% select(-c("id","date_free_version")) %>% names(), "%")

c1 <- full_join(a, b, by = c("id"))
names(c1)[grep("date", names(c1))] <- c("date_baseline","date_endpoint")
c1 %<>% relocate(c("date_baseline","date_endpoint"), .after = "id")

c1 <- full_join(c1, aa, by = c("id"))
c1 <- full_join(c1, bb, by = c("id"))
df04_non_FLC_self_report <- c1
rm(list = c("a","aa","b","bb","c1","df04_non_FLC_self_report_tmp"))

#rm outliers
df04_non_FLC_self_report <- df04_non_FLC_self_report %>% lin_exclude_NA_col(grep("weight",names(.), value = TRUE))

for (i in c(df04_non_FLC_self_report %>% names() %>% grep("∆", ., value = TRUE))) {
  df04_non_FLC_self_report[[i]] <- 
    ifelse(df04_non_FLC_self_report[[i]] < quantile(df04_non_FLC_self_report[[i]], 0.05, na.rm = TRUE) | df04_non_FLC_self_report[[i]] > quantile(df04_non_FLC_self_report[[i]], 0.95, na.rm = TRUE), NA, df04_non_FLC_self_report[[i]])
}

#sample size report
df04_non_FLC_self_report$id %>% unique() %>% length()

#df04_non_FLC_self_report %>% summary()



# 02.5 - [Data Preprocessing] 05_biochem --------------------------------------------------


#C1. format
df05_biochem[c("glucose_ac","glucose_pc_1hr","glucose_pc_2hr","insulin","insulin_pc_1hr","insulin_pc_2hr","hba1c","homa_ir","homa_beta",
         "triglyceride","total_cholesterol","high_density_lipoprotein","low_density_lipoprotein_cholesterol","sd_ldl",
         "c_peptide","egfr","blood_creatinine","uric_acid","tsh","prolactin","fsh","lh","e2","testosterone","progesterone","dhea_s","shbg","amh","t3","t3_reverse","t4_free","psa",
         "urine_spe_gravity", "urine_ph",
         "wbc","rbc","hb","esr","mcv","mch","mchc","platelet","rdw_sd","rdw_cv","neutrophils","lymphocytes","monocytes","eosinophils","basophils","monocytes_percent","eosinophils_percent","basophils_percent","alt_gpt","ast_got","amylase","lipase","apoli_a1","apoli_b","apolib_ai_ratio")] %<>% 
  lapply(as.numeric)

#C2. colname
names(df05_biochem) <- df05_biochem %>% names() %>% lin_ch_en_format(format = "en", origin = "raw_en")
#C3. order by date_blood
df05_biochem <- df05_biochem[with(df05_biochem, order(date_blood)),]
#(1) tAUCg, tAUCi (2) Pattern_major, Pattern_minor (3) OGIRIndex: iAUC-i(-30) - iAUC-g(+50)

df05_biochem$tAUCg <- lin_AUC_calc(df05_biochem, df05_biochem %>% names() %>% grep("^glucose", ., value = TRUE))
df05_biochem$tAUCi <- lin_AUC_calc(df05_biochem, df05_biochem %>% names() %>% grep("^insulin", ., value = TRUE))

df05_biochem <- df05_biochem %>% lin_insulin_rsp_pattern(df05_biochem %>% names() %>% grep("^insulin", ., value = TRUE), pattern = 2)
df05_biochem <- df05_biochem %>% rename(Pattern_major = I)
df05_biochem <- df05_biochem %>% lin_insulin_rsp_pattern(df05_biochem %>% names() %>% grep("^insulin", ., value = TRUE), pattern = 1)
df05_biochem <- df05_biochem %>% rename(Pattern_minor = I)

df05_biochem <- df05_biochem %>% mutate(OGIRIndex = lin_AUC_calc(df05_biochem, df05_biochem %>% names() %>% grep("^insulin", ., value = TRUE), increment_value = -30) - 
                              lin_AUC_calc(df05_biochem, df05_biochem %>% names() %>% grep("^glucose", ., value = TRUE), increment_value = 50))




# 02.6 - [Data Preprocessing] 06_Diet_day --------------------------------------------------
#[20230313] 待更新sql
#calorie_deficit




# 02.7 - [Data Preprocessing] 07_Diet_meal --------------------------------------------------

df07_Diet_meal <- tmp_07

df07_Diet_meal %<>% mutate(meat_bean = rowSums(select(., meat_beans_low_fat, meat_beans_medium_fat, meat_beans_high_fat), na.rm = TRUE) )
df07_Diet_meal %<>% mutate(milk = rowSums(select(., milk_whole_fat, milk_low_fat, milk_skim), na.rm = TRUE) )

df07_Diet_meal <- df07_Diet_meal %>% select(c("client_id","date_diet","meal_order","carbohydrate","protein","fat","calorie","fruits","vegetables","grains","meat_bean","milk","oil",))

df07_Diet_meal <- df07_Diet_meal[with(df07_Diet_meal, order(client_id, date_diet)),]

#condense df by id, date
df07_Diet_meal <- 
  df07_Diet_meal %>% 
  group_by(client_id, date_diet) %>% 
  summarise(fruits = sum(fruits, na.rm = TRUE),
            vegetables = sum(vegetables, na.rm = TRUE),
            grains = sum(grains, na.rm = TRUE),
            meat_bean = sum(meat_bean, na.rm = TRUE),
            milk = sum(milk, na.rm = TRUE),
            oil = sum(oil, na.rm = TRUE),
  )



# 02.8 - [Data Preprocessing] 08_3D_scanner --------------------------------------------------

tmp_99 <- tmp_08
tmp_99 %>% glimpse()

tmp_99 <- tmp_99[names(tmp_99)] %>% lapply(as.numeric)
tmp_99$client_id <- tmp_99$client_id %>% as.integer()


















