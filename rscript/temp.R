save.image("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/WSpace_preproc.RData")


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
  # 使用者    系統    流逝 
  # 0.474   0.331 724.842 
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
  
  
  i = 2941
  a <- df06_Diet_day %>% filter(client_id == 2941)
  
  a[(a[["client_id"]] == i) & ((a[["calorie"]] < 500) | (is.na(a[["calorie"]]))), a %>% names() %>% grep("client_id|date_diet|begin_date|end_date|target_updated_at", .,  invert = TRUE)] <- 
    a[(a[["client_id"]] == i) & ((a[["calorie"]] >= 500)), a %>% names() %>% grep("client_id|date_diet|begin_date|end_date|target_updated_at", .,  invert = TRUE)] %>% lapply(., function(x) mean(x, na.rm =TRUE) %>% round(2) )
  
  
  
  
  
  
  
  