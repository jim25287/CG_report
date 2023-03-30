# save.image("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/WSpace_preproc.RData")


#身體維度資料 e.g., wc 改用 3D data ##**缺date 欄位



source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/lin_function.R")


# dashboard --------------------------------------------------------------


# Pool data from cliets w/ blood data
# - weight loss (∆kg, %) total, avg (**cal. the max, and ignore rebound)
# - fat loss (∆kg, %) total, avg (**cal. the max, and ignore rebound)
# - improvement from BMI-classification
# - 減重成效，各組百分比 (**single treatment)




#1. med_id_pool
med_id_pool <- dplyr::intersect(clinic_inbody_data_ori %>% select(id) %>% unique() %>% pull(),
                                clinic_blood_data_ori %>% select(id) %>% unique() %>% pull() )
#2. produce dashborad dataset
clinic_inbody_data_dashboard <- clinic_inbody_data_ori %>% filter(id %in% med_id_pool)
clinic_blood_data_dashboard <- clinic_blood_data_ori %>% filter(id %in% med_id_pool)
rm(med_id_pool)



for (i in unique(clinic_inbody_data_dashboard$id)) {
  #loop 1st
  if (i == head(unique(clinic_inbody_data_dashboard$id),1)) {
    clinic_inbody_data_dashboard <- clinic_inbody_data_dashboard[with(clinic_inbody_data_dashboard, order(id, date_inbody)),] #order by date
    dashboard_table <- clinic_inbody_data_dashboard[0,c(-1,-2)] #create null table(w/t ID)
  }
  #main
  #every obs. - 1st obs.
  temp <- clinic_inbody_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_inbody")) - clinic_inbody_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_inbody")) %>% head(1) %>% as.vector()
  #collect data
  dashboard_table <- rbind(dashboard_table, temp %>% apply(2, min) %>% as.vector())
  #loop last
  if (i == tail(unique(clinic_inbody_data_dashboard$id),1)) {
    colnames(dashboard_table) <- names(clinic_inbody_data_dashboard[0,c(-1,-2)])
    dashboard_table <- cbind(unique(clinic_inbody_data_dashboard$id), dashboard_table)
    colnames(dashboard_table)[1] <- "id"
    cat("[Comopleted!]")
  }
} 

for (i in unique(clinic_blood_data_dashboard$id)) {
  #loop 1st
  if (i == head(unique(clinic_blood_data_dashboard$id),1)) {
    clinic_blood_data_dashboard <- clinic_blood_data_dashboard[with(clinic_blood_data_dashboard, order(id, date_blood)),] #order by date
    dashboard_table_blood <- clinic_blood_data_dashboard[0,c(-1,-2)] #create null table(w/t ID)
  }
  #main
  #every obs. - 1st obs.
  temp <- clinic_blood_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_blood")) - clinic_blood_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_blood")) %>% head(1) %>% as.vector()
  
  #collect data: min
  dashboard_table_blood <- rbind(dashboard_table_blood, temp %>% apply(2, min) %>% as.vector())
  #loop last
  if (i == tail(unique(clinic_blood_data_dashboard$id),1)) {
    colnames(dashboard_table_blood) <- names(clinic_blood_data_dashboard[0,c(-1,-2)])
    dashboard_table_blood <- cbind(unique(clinic_blood_data_dashboard$id), dashboard_table_blood)
    colnames(dashboard_table_blood)[1] <- "id"
    cat("[Comopleted!]")
  }
} 

dashboard_table <- lin_mapping(dashboard_table, client_type, id, clinical_list, client_type, id)
dashboard_table_blood <- lin_mapping(dashboard_table_blood, client_type, id, clinical_list, client_type, id)

#exclude DM duration < 30 days
dashboard_table <- dashboard_table %>% filter(client_type != 1 | is.na(client_type))
dashboard_table <- dashboard_table %>% filter((bf < -2) & (weight < -1))

dashboard_table_blood <- dashboard_table_blood %>% filter(id %in% dashboard_table$id)





#Layout
  #total client
  dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  
  #total accomplishment
  #include all: short term: finish program, long term: weight maintenance, weight rebound
  ##inbody
  dashboard_table %>% select("weight") %>% sum(na.rm = TRUE) 
  dashboard_table %>% select("bf") %>% sum(na.rm = TRUE)
  ##身體維度
  dashboard_table %>% select("wc") %>% sum(na.rm = TRUE)
  ##血生化
  # dashboard_table_blood %>% select("hba1c") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("glucose_ac") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("insulin") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("homa_ir") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("tg") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("tc") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("hdl") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("ldl") %>% sum(na.rm = TRUE)
  
  #avg
  #include all: short term: finish program, long term: weight maintenance, weight rebound
  ##inbody
  dashboard_table %>% select("weight") %>% sum(na.rm = TRUE) / dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  dashboard_table %>% select("bf") %>% sum(na.rm = TRUE) / dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  ##身體維度
  dashboard_table %>% select("wc") %>% sum(na.rm = TRUE) / dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  ##血生化 不適用, 嘗試換算成∆%, baseline important
  # dashboard_table_blood %>% select("hba1c") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("glucose_ac") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("insulin") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("homa_ir") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("tg") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("tc") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("hdl") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("ldl") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()

 

  

# test --------------------------------------------------------------------

  stat_table_1st_ob_lm <- stat_table_1st_ob %>% select(vars_en[vars_en != "gp"])
  names(stat_table_1st_ob_lm) <- vars_ch[vars_ch != "gp"]
  
  # Diet x Eff.
    #∆體重%
    stat_table_1st_ob_lm_diet <- stat_table_1st_ob_lm %>% select("∆體重%") %>% cbind(stat_table_1st_ob_lm %>% select(vars_ch[vars_ch %>% grep("baseline$|endpoint$|[∆]|id|client|gender|gp", ., invert = TRUE)]))
    stat_table_1st_ob_lm_diet <- stat_table_1st_ob_lm_diet %>% select(c("∆體重%","年齡","飲食紀錄完成率_%","每篇上傳照片數","綠燈率","黃燈率","紅燈率","碳水化合物_E%","蛋白質_E%","脂肪_E%","攝取熱量","水果攝取量_日","蔬菜攝取量_日","全穀雜糧攝取量_日","蛋豆魚肉攝取量_日","乳品攝取量_日","油脂攝取量_日"))
    
    model <- lm(-`∆體重%` ~ ., data = stat_table_1st_ob_lm_diet)
    model %>% summary()
    k <- olsrr::ols_step_both_p(model)
    k$model
    k$model %>% summary()
    a <- k$model %>% model_equation(digits = 3, trim = TRUE)
    a
    
    #∆體脂重%
    stat_table_1st_ob_lm_diet <- stat_table_1st_ob_lm %>% select("∆體脂重%") %>% cbind(stat_table_1st_ob_lm %>% select(vars_ch[vars_ch %>% grep("baseline$|endpoint$|[∆]|id|client|gender|gp", ., invert = TRUE)]))
    stat_table_1st_ob_lm_diet <- stat_table_1st_ob_lm_diet %>% select(c("∆體脂重%","年齡","飲食紀錄完成率_%","每篇上傳照片數","綠燈率","黃燈率","紅燈率","碳水化合物_E%","蛋白質_E%","脂肪_E%","攝取熱量","水果攝取量_日","蔬菜攝取量_日","全穀雜糧攝取量_日","蛋豆魚肉攝取量_日","乳品攝取量_日","油脂攝取量_日"))
    
    model <- lm(-`∆體脂重%` ~ ., data = stat_table_1st_ob_lm_diet)
    model %>% summary()
    k <- olsrr::ols_step_both_p(model)
    k$model
    k$model %>% summary()
    b <- k$model %>% model_equation(digits = 3, trim = TRUE)
    b

      
  

  
  #All
  # stepwise regression
  stat_table_1st_ob_lm_diet <-  stat_table_1st_ob_lm %>% select(
    (stat_table_1st_ob_lm %>% names() %>% grep("date|endpoint|id|type|∆", ., value = TRUE, invert = TRUE) %>% append(stat_table_1st_ob_lm %>% names() %>% grep("%", ., , value = TRUE)))
  )
  # stat_table_1st_ob_lm_diet <-  stat_table_1st_ob_lm %>% select(
  #    "∆體重%" %>% append(stat_table_1st_ob_lm %>% names() %>% grep("date|endpoint|id|type|∆", ., value = TRUE, invert = TRUE))
  # )
  
  model <- lm(-`∆體重%` ~ ., data = stat_table_1st_ob_lm_diet)
  model %>% summary()
  k <- olsrr::ols_step_both_p(model)
  k$model
  k$model %>% summary()
  c <- k$model %>% model_equation(digits = 3, trim = TRUE)
  c
  #plot(k)
  # final model
  k$model
  
  
  
  
  #[Prediction accuracy]
  ##1. create predict_table
  predict_table <- stat_table_1st_ob_lm_diet %>% select(k$predictors %>% gsub("`", "", .) %>% append("∆體重%"))
  ##2. create actual/forcast
  predict_table$actual <- -predict_table$`∆體重%`
  predict_table$forcast <- stats::predict(k$model, predict_table)
  #rm NA row
  predict_table <- predict_table %>% lin_exclude_NA_col(variables = names(.))
  ##3. calculate MAPE
  cat(paste("\n\n", "平均絕對百分比誤差(MAPE):", (MLmetrics::MAPE(predict_table$forcast, predict_table$actual) *100) %>% round(2), "%", "\n\n"))
  

  predict_table$residue <- abs((predict_table$actual - predict_table$forcast)/predict_table$actual *100) %>% round(2)
  predict_table$true <- predict_table$residue %>% cut(c(0, 10 , Inf), c(1, 0))
  predict_table$true %>% table() %>% prop.table() %>% multiply_by(100) %>% round(2)
  
  
  MLmetrics::Accuracy(predict_table$forcast, predict_table$true)
  MLmetrics::Precision(predict_table$forcast, predict_table$actual)
  
  
  
  
  
  
  
  #install.packages("Metrics")
  library(Metrics)

  
  set.seed(777)
  split <- caTools::sample.split(stat_table_1st_ob_lm_diet, SplitRatio = 0.8)
  
  train <- subset(stat_table_1st_ob_lm_diet, split == TRUE)
  test <- subset(stat_table_1st_ob_lm_diet, split == FALSE)
  
  
  
  


  
  

# verify the "ns" reason of testosterone gps 20230302 ---------------------

  # set.seed(333)
  # #sample size set to 100
  # a <- a %>% rbind(rbind(a[sample(which(a$gp_testosterone == "Low_testosterone"), (30 - sum(a$gp_testosterone == "Low_testosterone")), replace = TRUE),], 
  #                        a[sample(which(a$gp_testosterone != "Low_testosterone"), (30 - sum(a$gp_testosterone != "Low_testosterone")), replace = TRUE),]) )
  # table(a$gp_testosterone) %>% addmargins()
  #DONE
  
  
  

# tmp ---------------------------------------------------------------------
  # 使用者   系統   流逝 
  # 0.070  0.025  0.866 
  ptm <- proc.time()
  tmp_01 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "01_profile.sql")))
  cat("[執行時間:01_profile]\n")
  print(proc.time() - ptm)
  
  # [執行時間:02_inbody]
  # 使用者   系統   流逝 
  # 0.880  0.284 12.149 
  ptm <- proc.time()
  tmp_02 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "02_inbody.sql")))
  cat("[執行時間:02_inbody]\n")
  print(proc.time() - ptm)
  
  # [執行時間:03_FLC_self_report]
  # 使用者     系統     流逝 
  # 3.840    2.035 2762.575 
  ptm <- proc.time()
  tmp_03 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "03_FLC_self_report.sql")))
  cat("[執行時間:03_FLC_self_report]\n")
  print(proc.time() - ptm)
  
  # [執行時間:04_non_FLC_self_report]
  # 使用者   系統   流逝 
  # 0.058  0.028  6.554 
  ptm <- proc.time()
  tmp_04 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "04_non_FLC_self_report.sql")))
  cat("[執行時間:04_non_FLC_self_report]\n")
  print(proc.time() - ptm)
  
  
  # [執行時間:05_biochem]
  # 使用者   系統   流逝 
  # 0.188  0.064  5.155 
  ptm <- proc.time()
  tmp_05 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "05_biochem.sql")))
  cat("[執行時間:05_biochem]\n")
  print(proc.time() - ptm)
  
  # [執行時間:06_Diet_day]
  # 使用者   系統   流逝 
  # 16.119   2.702 238.284 
  ptm <- proc.time()
  tmp_06 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "06_Diet_day.sql")))
  cat("[執行時間:06_Diet_day]\n")
  print(proc.time() - ptm)
  
  
  # [執行時間:07_Diet_meal]
  # 使用者    系統    流逝 
  # 18.530   5.139 226.633 
  ptm <- proc.time()
  tmp_07 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "07_Diet_meal.sql")))
  cat("[執行時間:07_Diet_meal]\n")
  print(proc.time() - ptm)
  
  # [執行時間:08_3D_scanner]
  # 使用者   系統   流逝 
  # 1.119  0.181  8.010 
  ptm <- proc.time()
  tmp_08 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "08_3D_scanner.sql")))
  cat("[執行時間:08_3D_scanner]\n")
  print(proc.time() - ptm)
  
  # [執行時間:09_hormone]
  # 使用者   系統   流逝 
  # 0.039  0.013 10.025 
  ptm <- proc.time()
  tmp_09 <- DBI::dbGetQuery(db, readr::read_file(paste0(path_sql, "09_hormone.sql")))
  cat("[執行時間:09_hormone]\n")
  print(proc.time() - ptm)
  
  
  
  

# [Data Preprocessing] 01. profile ---------------------------------------------------
  #[Note:] 20230309_finish_genesis_ONLY
  
  #input clinic_list
  source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/00-read_clinic_list.R")
  
  
  #Q1-1. 初日開幕前的cofit初日班 => topshow
  tmp_01[dplyr::intersect(grep("初日班", tmp_01[["program_name"]]), which(tmp_01[["date_t0"]] < "2021-08-30")), "org_name"] <- "topshow"
  tmp_01[dplyr::intersect(grep("秀0|秀1", tmp_01[["name"]]), which(tmp_01[["date_t0"]] < "2021-08-30")), "org_name"] <- "topshow"
  #Q1-2. "program_name"初日開幕後的cofit初日班 => genesisclinic
  tmp_01[Reduce(dplyr::intersect, list(grep("初日", tmp_01[["program_name"]]),
                                       which(tmp_01[["date_t0"]] >= "2021-08-30"),
                                       grep("cofit", tmp_01[["org_name"]])))
         , "org_name"] <- "genesisclinic"
  #Q1-3. "name"名字有初日、初日開幕後的cofit => genesisclinic
  tmp_01[Reduce(dplyr::intersect, list(grep("初日|初日001|G001", tmp_01[["name"]]),
                                       which(tmp_01[["date_t0"]] >= "2021-08-30"),
                                       grep("cofit", tmp_01[["org_name"]])))
         , "org_name"] <- "genesisclinic"
    
  #Q1-4. FLC班 => cofit
  tmp_01[grep("FLC", tmp_01[["program_name"]]), "org_name"] <- "cofit"
  
  #C1-1. class_freq by org_name
  tmp_01 <- tmp_01 %>% full_join(tmp_01 %>% group_by(id, org_name) %>% summarise(class_freq = n()), by = c("id", "org_name"))
  tmp_01 <- tmp_01[with(tmp_01, order(c(date_t0, id))),] %>% janitor::remove_empty("rows")
  #C1-2. class_order
  for (i in unique(tmp_01$id)) {
    if (i == head(unique(tmp_01$id), 1)) {
      j = 1
      tmp_01$class_order <- NA
    }
    tmp_01[which(tmp_01[["id"]] == i), "class_order"] <- which(tmp_01[["id"]] == i) %>% order()
    progress(j, unique(tmp_01$id) %>% length())
    j = j + 1
    if (i == tail(unique(tmp_01$id), 1)){
      print("[Completed!]")
    }
  }
  
  
  #C2. age: btd - date_t0 年齡(療程起始當天計算)
  tmp_01$age <- (lubridate::ymd(tmp_01$date_t0) - lubridate::ymd(tmp_01$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()
  
  #C3-1.非進階
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),]
  tmp_01$client_type <- NA #client_type 
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),] <- lin_mapping(a, client_type, id, clinical_list, client_type, id)
  
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),]
  tmp_01$program_set <- NA #program_set
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),] <- lin_mapping(a, program_set, id, clinical_list, program, id)
  
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),]
  tmp_01$doctor <- NA #doctor
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),] <- lin_mapping(a, doctor, id, clinical_list, doctor, id)
  
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),]
  tmp_01$nutritionist_major <- NA #nutritionist_major
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),] <- lin_mapping(a, nutritionist_major, id, clinical_list, nutritionist_major, id)
  
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),]
  tmp_01$nutritionist_online <- NA #nutritionist_online
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),] <- lin_mapping(a, nutritionist_online, id, clinical_list, nutritionist_online, id)
  
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),]
  tmp_01$medication <- NA #medication
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name, invert = TRUE)),] <- lin_mapping(a, medication, id, clinical_list, medication_note, id)
  
  #C3-2.進階
  a <- tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),]
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),] <- lin_mapping(a, client_type, id, clinical_adv_list, client_type, id)
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),] <- lin_mapping(a, program_set, id, clinical_adv_list, program, id)
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),] <- lin_mapping(a, doctor, id, clinical_adv_list, doctor, id)
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),] <- lin_mapping(a, nutritionist_major, id, clinical_adv_list, nutritionist_major, id)
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),] <- lin_mapping(a, nutritionist_online, id, clinical_adv_list, nutritionist_online, id)
  tmp_01[intersect(which(tmp_01$org_name == "genesisclinic"), grep("進階", tmp_01$program_name)),] <- lin_mapping(a, medication, id, clinical_adv_list, medication_note, id)
  
  
  
  #clean by select
  tmp_01 <- tmp_01 %>% select(c("id", "name", "gender", "age", "client_type", "program_name","date_t0","date_t1", "org_name", "class_freq", "class_order","program_set","doctor","nutritionist_major","nutritionist_online","medication", "btd"))
  

  
  
  
  # [Data Preprocessing] 02. profile ---------------------------------------------------
  
  #C1. format
  tmp_02[c("height","weight","bmi","body_fat_mass","body_fat_mass_percentage","weight_without_fat","muscle_mass","real_muscle_mass","vfa","vfa_level","waist_circumference","acl","cacl","total_body_water","protein_weight","mineral_weight","body_cell_mass","body_mineral","bfmi","bsmi","ffmi","systolic_blood_pressure","diastolic_blood_pressure","pulse","bmr","wepa50","algle_50_left_arm","algle_50_left_leg","algle_50_right_arm","algle_50_right_leg","algle_50_trunk","extracellular_water_ratio","extracellular_water_ratio_left_arm","extracellular_water_ratio_left_leg","extracellular_water_ratio_right_arm","extracellular_water_ratio_right_leg","extracellular_water_ratio_trunk","intracellular_weight","intracellular_weight_left_arm","intracellular_weight_left_leg","intracellular_weight_right_arm","intracellular_weight_right_leg","intracellular_weight_trunk","extracellular_weight","extracellular_weight_left_arm","extracellular_weight_left_leg","extracellular_weight_right_arm","extracellular_weight_right_leg","extracellular_weight_trunk","left_arm_fat","left_arm_fat_percentage","left_arm_muscle","left_arm_muscle_percentage","left_leg_fat","left_leg_fat_percentage","left_leg_muscle","left_leg_muscle_percentage","right_arm_fat","right_arm_fat_percentage","right_arm_muscle","right_arm_muscle_percentage","right_leg_fat","right_leg_fat_percentage","right_leg_muscle_percentage","right_leg_muscle","trunk_fat","trunk_fat_percentage","trunk_muscle","trunk_muscle_percentage","water_weight_left_arm","water_weight_left_leg","water_weight_right_arm","water_weight_right_leg","water_weight_trunk","waist_hip_ratio","tbwffm","obesity_degree","inbody_total_score")] %<>% 
    lapply(as.numeric)
  tmp_02 <- tmp_02 %>% as.tibble() 
  #C2. Sarcopenia Obesity(SO): "left_arm_muscle", "left_leg_muscle", "right_arm_muscle", "right_leg_muscle" #Female: < 23.4; Male: < 29.6. 
  tmp_02 <- tmp_02 %>% mutate(so_score = round((left_arm_muscle+left_leg_muscle+right_arm_muscle+right_leg_muscle)*100/weight,2))
  #C3. pbm
  tmp_02 <- tmp_02 %>% mutate(pbm = round((muscle_mass)*100/weight,2))
  #C4. name_format
  names(tmp_02) <- names(tmp_02) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
  #C5. rm outlier
  tmp_02 <- tmp_02[-which(tmp_02$bmi >100),]
  
  
  # 02.3 - [Data Preprocessing] 03_FLC_self_report --------------------------------------------------
  
  #C1. col_names
  names(tmp_03) <- names(tmp_03) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
  tmp_03 <- tmp_03[with(tmp_03, order(date_flc_T0)),]
  
  #C2. age: btd - date_t0 年齡(療程起始當天計算)
  tmp_03$age <- (lubridate::ymd(tmp_03$date_flc_T0) - lubridate::ymd(tmp_03$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()
  #C3. (1.) (%) *100  (2.) numeric %>% round(2)
  tmp_03[,grep("%", names(tmp_03))] %<>% multiply_by(100)
  tmp_03[c("weight(T0)","weight(T1)","∆weight","∆weight(%)","BMI(T0)","BMI(T1)","∆BMI","∆BMI(%)","Fat(T0)","Fat(T1)","∆Fat","∆Fat(%)","wc(T0)","wc(T1)","∆wc","∆wc(%)")] %<>% round(2)
  
  
  #C4-1. class_freq by org_name
  tmp_03 <- tmp_03 %>% full_join(tmp_03 %>% group_by(id) %>% summarise(class_freq = n()), by = c("id"))
  #C4-2. class_order
  for (i in unique(tmp_03$id)) {
    if (i == head(unique(tmp_03$id), 1)) {
      j = 1
      tmp_03$class_order <- NA
    }
    tmp_03[which(tmp_03[["id"]] == i), "class_order"] <- which(tmp_03[["id"]] == i) %>% order()
    progress(j, unique(tmp_03$id) %>% length())
    j = j + 1
    if (i == tail(unique(tmp_03$id), 1)){
      print("[Completed!]")
    }
  }
  
  
  # 02.4 - [Data Preprocessing] 04_non_FLC_self_report --------------------------------------------------
  
  tmp_99 <- tmp_04
  tmp_99 %>% glimpse()
  
  
  
  
  
  
  
  #cofit: 
    ##重複資料太多，不能用
  #clinic_df
    ##Done
  #diet calorie < 500/NA, summary, merge
    ##Done
  #progress, DM function
    ##Done
  #client type
    ##Done
  
  #df01_profile, client_type_is.na, look-up from clinic note
    #1.not clinic
    a1 <- df01_profile %>% filter(!((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow"))) 
    #2.clinic, !is.na:client_type #map_ref
    a2 <- df01_profile %>% filter((!is.na(client_type)) & ((org_name == "genesisclinic") | (org_name == "topshow")))
    #3.clinic
    a3 <- df01_profile %>% filter((org_name == "genesisclinic") | (org_name == "topshow"))
    #4. map 2 & 3 / adv.clinic_list
    a3 <- lin_mapping(a3, client_type, id, a2, client_type, id)
    a3 <- lin_mapping(a3, client_type, id, clinical_adv_list, client_type, id)
    #5. rbind, order
    a4 <- a1 %>% rbind(a3) 
    a4 <- a4[with(a4, order(date_t0, id)),]
    # a4 %>% nrow()
    df01_profile <- a4
    rm(list = c("a1","a2","a3","a4"))
  
  #df01_profile, client_type_is.na, look-up from blood_first_record
    #1.not clinic
    a1 <- df01_profile %>% filter(!((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow"))) 
    df01_profile_tmp <- df01_profile[is.na(df01_profile[["client_type"]]) & ((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow")), ]
    #a2.clinic, blood_first_record #map_ref
    a2 <- df05_biochem[which(df05_biochem[["id"]] %in% df01_profile_tmp[["id"]]),]
    a2 <- a2[with(a2, order(id, date_blood)),]
    a2 <- a2 %>% filter(DM != "Unclassified")
    a2 <- a2 %>% distinct(id, .keep_all = TRUE)
      ##if DM > client_type == "1"
    a2$client_type = NA
    a2$client_type <- ifelse(a2$DM == "DM", "1", "2")
    # table(a2$DM, a2$client_type)
    #3.clinic
    a3 <- df01_profile %>% filter((org_name == "genesisclinic") | (org_name == "topshow"))
    #4. map 2 & 3 / blood_first_record
    a3 <- lin_mapping(a3, client_type, id, a2, client_type, id)
    #5. rbind, order
    a4 <- a1 %>% rbind(a3) 
    a4 <- a4[with(a4, order(date_t0, id)),]
    # a4 %>% nrow()
    df01_profile <- a4
    rm(list = c("a1","a2","a3","a4"))
    
    #**temp adjustment - to be refine in the future: client_type_is.na : 2 Obesity
    df01_profile[(is.na(df01_profile[["client_type"]])) & ((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow")), "client_type"] <- 2
    
    # df01_profile[(is.na(df01_profile[["client_type"]])) & ((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow")), "id"] %>% 
    #   unique() %>% length()
    # df01_profile[!(is.na(df01_profile[["client_type"]])) & ((df01_profile[["org_name"]] == "genesisclinic") | (df01_profile[["org_name"]] == "topshow")), "id"] %>% 
    #   unique() %>% length()
  
    
  
  
    df05_biochem[(df05_biochem$id == 463448) , ] %>% select(date_blood) %>% pull
    df05_biochem[(df05_biochem$id == 463448) , "date_blood"] %>% pull
    clinic_blood_data[(clinic_blood_data$id == 463448) , "date_blood"] %>% pull
  
    
    df02_inbody[(df02_inbody$id == 463448) , ] %>% select(date_inbody) %>% pull
    clinic_inbody_data[(clinic_inbody_data$id == 463448) , "date_inbody"] %>% pull
    
    
    
    
    
    
    
    df03_FLC_self_report %>% 
      group_by(gender) %>% 
      summarise(
        weight = mean(`∆weight(%)`, na.rm = TRUE),
        fat = mean(`∆Fat(%)` , na.rm = TRUE)
      )
    df03_FLC_self_report$`∆weight(%)` %>% summary()
    df03_FLC_self_report$`∆Fat(%)` %>% summary()
    
    
    
    
    df04_non_FLC_self_report %>%  
      group_by(gender_baseline) %>% 
      summarise(
        weight = mean(`∆weight%`, na.rm = TRUE),
        fat = mean(`∆fat%` , na.rm = TRUE)
      )
    df04_non_FLC_self_report$`∆weight%` %>% summary()
    df04_non_FLC_self_report$`∆fat%` %>% summary()
  
    
    
    
    
    
    
    
    
# FLC & non-FLC -----------------------------------------------------------
    
    
    (Sys.Date() - months(6)) %>% lubridate::floor_date(unit = "month")
    df03_FLC_self_report %>% filter(date_flc_T1 <= "2022-09-01") 
    #csv
    
    

# Lee query ---------------------------------------------------------------


    #Effectiveness: age_gp < 25, clinic, FLC, non-FLC: plot: barplot, x:C/F/NF_by_gender; y = vars; facet: age_gp
    
    #01.clinic (C)
    stat_table_1st_ob$age_gp <- cut(stat_table_1st_ob$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
    stat_table_1st_ob$org_name_gp <- "Med"
    #02.FLC (F)
    # df03_FLC_self_report$age_gp <- cut(df03_FLC_self_report$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
    # df03_FLC_self_report <- df03_FLC_self_report %>% rename("∆weight%" = "∆weight(%)")
    # df03_FLC_self_report$org_name_gp <- "Diet"
    
    df03_FLC_self_report$age_gp <- cut(df03_FLC_self_report$年齡, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
    df03_FLC_self_report <- df03_FLC_self_report %>% rename("∆weight%" = "體重 %")
    df03_FLC_self_report <- df03_FLC_self_report %>% rename("∆weight" = "體重 △")
    df03_FLC_self_report <- df03_FLC_self_report %>% rename("gender" = "性別")
    df03_FLC_self_report$org_name_gp <- "Diet"
    
    # 03.non-FLC (NF) 
    df04_non_FLC_self_report$age_gp <- cut(df04_non_FLC_self_report$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
    df04_non_FLC_self_report$org_name_gp <- "Control"
    
    #merge Q_dr.lee_01_datasets  
    Q_dr.lee_01_datasets <- Reduce(rbind, list(stat_table_1st_ob %>% select(`∆weight`,`∆weight%`, age_gp, gender, org_name_gp), 
                                               df03_FLC_self_report %>% select(`∆weight`,`∆weight%`, age_gp, gender, org_name_gp) %>% lin_exclude_NA_col("∆weight%"),
                                               df04_non_FLC_self_report %>% select(`∆weight`,`∆weight%`, age_gp, gender, org_name_gp) %>% lin_exclude_NA_col("∆weight%")))
    Q_dr.lee_01_datasets$org_name_gp <-  Q_dr.lee_01_datasets$org_name_gp %>% factor(levels = c("Med", "Diet", "Control"))

      Q_dr.lee_01_datasets %>% 
        filter(age_gp == levels(Q_dr.lee_01_datasets$age_gp)[1]) %>% 
        select(org_name_gp, gender, `∆weight%`, age_gp) %>% rename(value = "∆weight%") %>% 
        mutate(value_adj = value %>% multiply_by(-1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "org_name_gp", alpha = 0.5, width = 0.5,
                  add = "mean", add.params = list(group = "org_name_gp"),
                  label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, 
                  position = position_dodge(0.5), 
                  xlab = "", ylab = "∆Weight Loss(%)", title = paste0("減重成效", "(Age:", levels(Q_dr.lee_01_datasets$age_gp)[1], ")"),
                  legend = "right", legend.title = "Program", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))
        
      Q_dr.lee_01_datasets %>% 
        filter(age_gp == levels(Q_dr.lee_01_datasets$age_gp)[1]) %>% 
        select(org_name_gp, gender, `∆weight`, age_gp) %>% rename(value = "∆weight") %>% 
        mutate(value_adj = value %>% multiply_by(-1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "org_name_gp", alpha = 0.5, width = 0.5,
                  add = "mean", add.params = list(group = "org_name_gp"),
                  label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, 
                  position = position_dodge(0.5), 
                  xlab = "", ylab = "∆Weight Loss(Kg)", title = paste0("減重成效", "(Age:", levels(Q_dr.lee_01_datasets$age_gp)[1], ")"),
                  legend = "right", legend.title = "Program", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))
    
    
      Q_dr.lee_01_datasets$org_name_gp %>% table() %>% addmargins()
      
      
    
    

# uric acid ---------------------------------------------------------------

    
      stat_table_1st_ob <- stat_table_1st_ob %>% mutate(delta_sua_gp = paste(sua_gp_baseline, sua_gp_endpoint, sep = ">")) 
      
      table(stat_table_1st_ob$delta_sua_gp)
      
      
      stat.test <- 
      stat_table_1st_ob %>% 
        filter(!is.na(sua_gp_baseline)) %>% 
        select(delta_sua_gp, gender, `∆weight%`) %>% rename(value = "∆weight%") %>% 
        mutate(value_adj = value %>% multiply_by(-1)) %>% 
        group_by(gender) %>% 
        rstatix::t_test(value_adj ~ delta_sua_gp) %>%
        rstatix::add_xy_position(x = "gender", fun = "mean_sd", dodge = 0.5)
      
      plot_SUA_03 <- 
      stat_table_1st_ob %>% 
        filter(!is.na(sua_gp_baseline)) %>% 
        select(delta_sua_gp, gender, `∆weight%`) %>% rename(value = "∆weight%") %>% 
        mutate(value_adj = value %>% multiply_by(-1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "delta_sua_gp", alpha = 0.5, width = 0.5,
                  add = "mean_se", add.params = list(group = "delta_sua_gp"),
                  label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, 
                  position = position_dodge(0.5), 
                  xlab = "", ylab = "∆Weight Loss(%)", title = paste0("減重成效", " x 尿酸", (" (cutoff = 5.5)")),
                  legend = "right", legend.title = "SUA Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        stat_pvalue_manual(
          stat.test, label = "p.adj.signif", tip.length = 0.01,
          bracket.nudge.y = -2, hide.ns = TRUE
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      
      
      
      table(stat_table_1st_ob$sua_gp_baseline, stat_table_1st_ob$sua_gp_endpoint)
      
      
      plot_SUA_01 <- 
      stat_table_1st_ob %>% 
        ggscatter(x = "uric_acid_baseline", y = "weight_baseline",
                  color = "black",
                  fill = "red",
                  shape = 21,
                  size = 1,
                  add = "reg.line",  # Add regressin line
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  title = "Correlation(Weight x SUA):Baseline",
                  xlab = "uric_acid(mg/dL)",
                  ylab = "Weight(kg)",
                  # xlim = c(0, 13),
                  # ylim = c(0, 180),
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        geom_vline(xintercept = c(5.5),linetype ="dashed", ) +
        annotate("text", x=5.3, y=155, label="Cutoff = 5.5 mg/dL", angle=90) +
        stat_cor(method = "pearson", size = 5, label.x = 10, label.y = 45) # Add correlation coefficient)
      
      plot_SUA_02 <- 
      stat_table_1st_ob %>% 
        ggscatter(x = "uric_acid_endpoint", y = "weight_endpoint",
                  color = "black",
                  fill = "red",
                  shape = 21,
                  size = 1,
                  add = "reg.line",  # Add regressin line
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  title = "Correlation(Weight x SUA):Endpoint",
                  xlab = "uric_acid(mg/dL)",
                  ylab = "Weight(kg)",
                  # xlim = c(0, 13),
                  # ylim = c(0, 180),
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        geom_vline(xintercept = c(5.5),linetype ="dashed", ) +
        annotate("text", x=5.3, y=150, label="Cutoff = 5.5 mg/dL", angle=90) +
        stat_cor(method = "pearson", size = 5, label.x = 7, label.y = 45) # Add correlation coefficient)
    
      
      
      ggarrange(
        ggarrange(plot_SUA_01, plot_SUA_02, ncol = 2, labels = c("A", "B")),
        ggarrange(plot_SUA_03, labels = "C"),
        nrow = 2
      )
        
      
      #corr
      
      profile_efficacy <- stat_table_1st_ob %>% 
        select(c("∆weight%","∆bf%","∆bm%","∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆homa_beta","∆tg","∆tc","∆hdl","∆ldl","∆lipase","∆uric_acid"))
      
      names(profile_efficacy) <- names(profile_efficacy) %>% lin_ch_en_format(format = "ch", origin = "en")
      
      profile_baseline <- stat_table_1st_ob %>% 
        select(c("age", "bmi_baseline","pbf_baseline","pbm_baseline",
                 "hba1c_baseline","glucose_ac_baseline","insulin_baseline","homa_ir_baseline","homa_beta_baseline", "tAUCg_baseline", "tAUCi_baseline", "OGIRIndex_baseline",
                 "tg_baseline","tc_baseline","hdl_baseline","ldl_baseline","lipase_baseline",
                 "uric_acid_baseline"))
      
      names(profile_baseline) <- names(profile_baseline) %>% lin_ch_en_format(format = "ch", origin = "en")
      
      profile_diet <- stat_table_1st_ob %>% 
        select(c("upload_day_%", "pic_counts","calorie_day","carb_E%","protein_E%","fat_E%","fruits","vegetables","grains","meat_bean","milk", "oil","light_G_%","light_Y_%","light_R_%"))
      
      names(profile_diet) <- names(profile_diet) %>% lin_ch_en_format(format = "ch", origin = "en")
      
      
      ##[Method 2] corrplot
      
      library(corrplot)
      #[Correlation r] Efficacy x Diet
      M1_sua <- cor(cbind(-profile_efficacy, profile_diet), use = "pairwise.complete.obs")
      #[2Do]change row,col names into chinese
      M_test1_sua <- cor.mtest(cbind(-profile_efficacy, profile_diet) , conf.level = .95)
      M_col_sua <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))
      
      
      # run corrplot plot
      corrplot(M1_sua,
               p.mat = M_test1_sua$p,
               type = "lower",
               insig = "label_sig",
               sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
               tl.col = "black", tl.srt = 35, tl.cex = 1.0,
               cl.ratio = 0.1,
               col = M_col_sua(200),
               title = "[Correlation] Uric acid x Diet",
               #c(bottom, left, top, right)
               mar = c(0,0,1,0))

      for (i in c(1:length(colnames(M1_sua)))) {
        if (i == 1) {
          M1_value <- M1_sua %>% round(2) 
          M1_sign <- M_test1_sua$p %>% stats::symnum(corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")) %>% as.data.frame.matrix()
          M1_df <- M1_value
          M1_df[,] <- NA
        }
        
        M1_df[,i] <- paste0(M1_value[,i], " (", M1_sign[,i], ")")
        
        if (i ==  length(colnames(M1_sua))) {
          rm(list = c("M1_value", "M1_sign"))
          M1_df <- M1_df %>% as.data.frame()
          M1_df <- M1_df %>% add_column(vars = rownames(M1_df), .before = names(M1_df)[1])
          M1_df <- M1_df %>% add_column("#" = seq(1, nrow(M1_df)), .before = names(M1_df)[1])
        }
        
      } 
      
      cor_table_01_sua <- M1_df %>% gvisTable(options=list(frozenColumns = 2,
                                                       height=300))
      
      
      M2_sua <- cor(cbind(-profile_efficacy, profile_baseline), use = "pairwise.complete.obs")
      #[2Do]change row,col names into chinese
      M_test2_sua <- cor.mtest(cbind(-profile_efficacy, profile_baseline) , conf.level = .95)
      M_col_sua <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))
      
      
      #run corrplot plot
      corrplot(M2_sua,
               p.mat = M_test2_sua$p,
               type = "lower",
               insig = "label_sig",
               sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
               tl.col = "black", tl.srt = 35, tl.cex = 1.0,
               cl.ratio = 0.1,
               col = M_col_sua(200),
               title = "[Correlation] Efficacy x Baseline",
               #c(bottom, left, top, right)
               mar = c(0,0,1,0))
      
      
      # stat_table_1st_ob$`protein_E%`
      stat_table_1st_ob %>% rename(delta_uric_acid = `∆uric_acid`) %>% rename(fat = `fat_E%`) %>% 
        ggscatter(x = "delta_uric_acid", y = "fat",
                  color = "black",
                  fill = "red",
                  shape = 21,
                  size = 1,
                  add = "reg.line",  # Add regressin line
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE, # Add confidence interval
                  title = "Correlation(Weight x SUA):Diet",
                  xlab = "∆uric_acid(mg/dL)",
                  ylab = "Intake:Protein(E%)",
                  # xlim = c(0, 13),
                  # ylim = c(0, 180),
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        stat_cor(method = "pearson", size = 5, label.x = 0, label.y = 45) # Add correlation coefficient)
      
      
      
      #
      stat_table_1st_ob$meat_bean
      
      stat.test <- 
        stat_table_1st_ob %>% 
        filter(!is.na(sua_gp_baseline)) %>% 
        select(delta_sua_gp, gender, `fat_E%`) %>% rename(value = "fat_E%") %>% 
        mutate(value_adj = value %>% multiply_by(1)) %>% 
        group_by(gender) %>% 
        rstatix::t_test(value_adj ~ delta_sua_gp) %>%
        rstatix::add_xy_position(x = "gender", fun = "mean_sd", dodge = 0.5)
      
      stat_table_1st_ob %>% 
        filter(!is.na(sua_gp_baseline)) %>% 
        select(delta_sua_gp, gender, `fat_E%`) %>% rename(value = "fat_E%") %>% 
        mutate(value_adj = value %>% multiply_by(1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "delta_sua_gp", alpha = 0.5, width = 0.5,
                  add = "mean_se", add.params = list(group = "delta_sua_gp"),
                  label = TRUE, lab.nb.digits = 2, lab.pos = "out", lab.vjust = -1, 
                  position = position_dodge(0.5), 
                  xlab = "", ylab = "Intake: fat(E%)", title = paste0("Diet", " x 尿酸"),
                  legend = "right", legend.title = "SUA Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        stat_pvalue_manual(
          stat.test, label = "p.adj.signif", tip.length = 0.01,
          bracket.nudge.y = 1, step.increase = 0.05, hide.ns = TRUE
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      
      
      

# 星座 ----------------------------------------------------------------------
      # 生肖
      # seq(1899 + 1, 2100, by = 12)
      # as.Date(df[[variables]]) %>% format("%Y") %in% seq(1899 + 9, 2100, by = 12)
      
      
      a <- df01_profile %>% filter(org_name %in% c("topshow", "genesisclinic", "lumez")) %>% distinct(id, .keep_all = TRUE)  #Clinic
      b <- stat_table %>% rename(delta_weight_p = `∆weight%`)
      a <- lin_mapping(a, bmi_baseline, id, b, bmi_baseline, id)
      a <- lin_mapping(a, delta_weight_p, id, b, delta_weight_p, id)
      a <- lin_mapping(a, homa_ir_baseline, id, b, homa_ir_baseline, id)
      a <- lin_mapping(a, "upload_day_%", id, b, "upload_day_%", id)
      a <- lin_mapping(a, "light_G_%", id, b, "light_G_%", id)
      a <- a %>% mutate(diet_obediance = (`"upload_day_%"` * `"light_G_%"` /100) %>% round(2))
      a <- a %>% filter(!is.na(delta_weight_p))
      # a <- df01_profile %>% distinct(id, .keep_all = TRUE)  #All
      a <- lin_astrological_type(a, "btd")
      a1 <- table(a$astro) %>% addmargins() %>% as.data.frame()
      table(a$astro, a$gender) %>% addmargins()
      a1[with(a1, order(Freq, decreasing = TRUE)),]
      
      
      a %>% 
        filter(!is.na(astro)) %>% 
        select(astro, gender, `bmi_baseline`) %>% rename(value = "bmi_baseline") %>% 
        mutate(value_adj = value %>% multiply_by(1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "astro", alpha = 0.5, width = 0.7,
                  add = "mean", add.params = list(group = "astro"),
                  label = TRUE, lab.nb.digits = 1, lab.pos = "out", lab.vjust = -2, lab.size = 2,
                  position = position_dodge(0.7), 
                  xlab = "", ylab = "weight(Kg)", title = paste0("BMI", "x 星座"),
                  legend = "right", legend.title = "Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      a %>% 
        filter(!is.na(astro)) %>% 
        select(astro, gender, `homa_ir_baseline`) %>% rename(value = "homa_ir_baseline") %>% 
        mutate(value_adj = value %>% multiply_by(1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "astro", alpha = 0.5, width = 0.7,
                  add = "mean", add.params = list(group = "astro"),
                  label = TRUE, lab.nb.digits = 1, lab.pos = "out", lab.vjust = -2, lab.size = 2,
                  position = position_dodge(0.7), 
                  xlab = "", ylab = "HOMA-IR", title = paste0("胰島素阻抗", "x 星座"),
                  legend = "right", legend.title = "Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      a %>% 
        filter(!is.na(astro)) %>% 
        select(astro, gender, `delta_weight_p`) %>% rename(value = "delta_weight_p") %>% 
        mutate(value_adj = value %>% multiply_by(-1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "astro", alpha = 0.5, width = 0.7,
                  add = "mean", add.params = list(group = "astro"),
                  label = TRUE, lab.nb.digits = 1, lab.pos = "out", lab.vjust = -2, lab.size = 2,
                  position = position_dodge(0.7), 
                  xlab = "", ylab = "∆weight%(Kg)", title = paste0("減重成效", "x 星座"),
                  legend = "right", legend.title = "Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      a %>% 
        filter(!is.na(astro)) %>% 
        select(astro, gender, `"upload_day_%"`) %>% rename(value = `"upload_day_%"`) %>% 
        mutate(value_adj = value %>% multiply_by(1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "astro", alpha = 0.5, width = 0.7,
                  add = "mean", add.params = list(group = "astro"),
                  label = TRUE, lab.nb.digits = 1, lab.pos = "out", lab.vjust = -2, lab.size = 2,
                  position = position_dodge(0.7), 
                  xlab = "", ylab = "飲食完成率(%)", title = paste0("飲食完成率", "x 星座"),
                  legend = "right", legend.title = "Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      a %>% 
        filter(!is.na(astro)) %>% 
        select(astro, gender, diet_obediance) %>% rename(value = diet_obediance) %>% 
        mutate(value_adj = value %>% multiply_by(1)) %>% 
        ggbarplot(x = "gender", y = "value_adj", fill = "astro", alpha = 0.5, width = 0.7,
                  add = "mean", add.params = list(group = "astro"),
                  label = TRUE, lab.nb.digits = 1, lab.pos = "out", lab.vjust = -2, lab.size = 2,
                  position = position_dodge(0.7), 
                  xlab = "", ylab = "飲食紀律分數", title = paste0("飲食紀律分數", "x 星座"),
                  legend = "right", legend.title = "Group", ggtheme = theme_light() ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 17), 
          axis.text.x = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y.left = element_text(hjust = 0.5, face = "bold", size = 14)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      
      
      